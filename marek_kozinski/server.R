library(shiny)
library(DT)
library(bsicons)
library(LiblineaR)
library(shinymodels)
library(tidymodels)

tidymodels_prefer()
conflicted::conflicts_prefer(shiny::observe)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath, sep = input$sep)
  })
  
  output$summ_table <- renderDT(
    data.frame(
        c(ncol(data()), nrow(data()), sum(is.na(data()))),
        row.names = c('Number of variables','Number of observations','Missing values')) |> 
      `colnames<-`('Value'),
    options = list(dom = 't'))
  
  output$distPlot <- renderPlot({

      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')

  })
  # 
  # output$distPlot2 <- renderPlot({
  #     
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #        xlab = 'Waiting time to next eruption (in mins)',
  #        main = 'Histogram of waiting times')
  #     
  # })
  
  modifiedData <- reactiveValues(df = NULL)
  
  observe({
    modifiedData$df <- data()
  })
  
  output$varSelect <- renderUI({
    df <- data()
    lapply(names(df), function(name){
      tagList(
        selectInput(inputId = name, 
                    label = paste("Choose a type for variable:", name), 
                    choices = c("Numeric", "Character", "Logical")),
        htmlOutput(paste0(name, "_error"))
      )
    })
  })
  
  observe({
    lapply(names(data()), function(name){
      observeEvent(input[[name]], {
        tryCatch({
          if(input[[name]] == "Numeric") {
            modifiedData$df[[name]] <<- as.numeric(modifiedData$df[[name]])
          } else if(input[[name]] == "Character") {
            modifiedData$df[[name]] <<- as.character(modifiedData$df[[name]])
          } else if(input[[name]] == "Logical") {
            modifiedData$df[[name]] <<- as.logical(modifiedData$df[[name]])
          }
          output[[paste0(name, "_error")]] <-  renderUI({
            tags$span(style = "color: green;", "Done: Conversion successful!")
          })
        }, warning = function(w) {
          output[[paste0(name, "_error")]] <- renderUI({
            tags$span(style = "color: orange;", paste("Warning: Cannot convert variable", name, "to type", input[[name]], "- ", w$message))
          })
        }, error = function(e) {
          output[[paste0(name, "_error")]] <- renderUI({
            tags$span(style = "color: red;", paste("Error: Cannot convert variable", name, "to type", input[[name]], "- ", e$message))
          })
        })
      })
    })
  })
  
  output$target_var_ui <- renderUI({
    selectInput("target_var", "Choose target variable", choices = names(modifiedData$df))
  })
  
  output$v_box <- renderUI({
    req(input$target_var)
    value_box(title = "Target variable",
              value = input$target_var)
  })
  
  output$type <- renderUI({
    req(input$target_var)
    col <- input$target_var
    type <- ifelse(sapply(modifiedData$df[col],class)=="numeric","Regression","Classification")
    icon <- ifelse(type=="Regression","graph-up","bullseye")
    value_box(title = "Type of task",
              value = type,
              showcase = bs_icon(icon))
  })
  
  output$pre_steps <- renderUI({
    selected_checkboxes <- sum(c(input$normalize, input$dummy, input$pca, input$remove_zero_var, 
                                 input$remove_near_zero_var, input$log_transform, input$class_other, 
                                 input$sqrt_transform), na.rm = TRUE)
    value_box(title = "Preprocessing steps",
      value = selected_checkboxes,
      icon = icon("check-square"),
      color = "blue"
    )
  })
  
  
  output$pca_ui <- renderUI({
    if (input$pca) {
      sliderInput("pca_comp",
                  "Number of components",
                  min = 2,
                  max = ncol(modifiedData$df),
                  value = round(ncol(modifiedData$df)/2),
                  step = 1
      )
    }
  })
  
  output$class_other_ui <- renderUI({
    if (input$class_other) {
      sliderInput("other",
                  "Threshold",
                  min = 0.01,
                  max = 0.95,
                  value = 0.15
      )
    }
  })
  
  output$transforms_ui <- renderUI({
    if (input$log_transform) {
      numeric_vars <- names(modifiedData$df)[sapply(modifiedData$df, is.numeric)]
      selectizeInput("select_log",
                     "Select variables for log trasform",
                     choices = numeric_vars,
                     multiple = TRUE
      )
    }
  })
  
  output$train_size <- renderUI({
    req(modifiedData$df)
    value_box(title = "Train set size",
              value = paste0(input$split*100, "% (",round(nrow(modifiedData$df)*input$split),")"))
  })
  
  output$model_select <- renderUI({
    value_box(title = "Model selected",
              value = input$model_select)
  })
  
  data_split <- eventReactive(input$train_model, {
    initial_split(modifiedData$df, prop = input$split)
  })
  
  dummy_model <- function(input_model, input_dummy) {
    if ((input_model == "XGBoost" || input_model == "SVM") && input_dummy == F) {
      "This model need 'Dummy Variable' preprocessing step"
    } else {
      NULL
    }
  }
  
  req_data <- function(input) {
    if (!is.data.frame(input)) {
      "Provide data"
    } else {
      NULL
    }
  }
  
  model_fit <- eventReactive(input$train_model, {
    
    validate(req_data(modifiedData$df))
    validate(dummy_model(input$model_select, input$dummy))
    
    switch(input$model_select,
           "Decision Tree"={
             if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
               model_spec <- decision_tree(mode = "regression")
             }
             else {
               model_spec <- decision_tree(mode = "classification")
             }
             
           },
           "XGBoost"={
             if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
               model_spec <- boost_tree(mode = "regression")
             }
             else {
               model_spec <- boost_tree(mode = "classification")
             }
           },
           "Random Forest"={
             if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
               model_spec <- rand_forest(mode = "regression")
             }
             else {
               model_spec <- rand_forest(mode = "classification")
             }
           },
           "SVM"={
             if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
               model_spec <- svm_linear(mode = "regression")
             }
             else {
               model_spec <- svm_linear(mode = "classification")
             }
           })

    rec <- recipe(as.formula(paste(input$target_var, "~ .")), data = training(data_split()))

    if (input$dummy){
      rec <- rec %>% 
        step_dummy(all_string_predictors())
    }
    
    if (input$normalize){
      rec <- rec %>% 
        step_normalize(all_numeric_predictors())
    }
    
    if (input$remove_zero_var){
      rec <- rec %>% 
        step_zv(all_numeric_predictors())
    }
    
    if (input$remove_near_zero_var){
      rec <- rec %>% 
        step_nzv(all_numeric_predictors())
    }
    
    if (input$log_transform){
      rec <- rec %>% 
        step_log(input$select_log)
    }
    
    if (input$sqrt_transform){
      rec <- rec %>% 
        step_sqrt(all_numeric_predictors())
    }
    
    if (input$class_other){
      rec <- rec %>% 
        step_other(all_string_predictors(), threshold = input$other)
    }
    
    if (input$pca){
      rec <- rec %>% 
        step_pca(all_numeric_predictors(),num_comp = input$pca_comp)
    }

    wf <- workflow() %>%
      add_model(model_spec) %>%
      add_recipe(rec)

    fit(wf, data = modifiedData$df)
  })
  
  output$build_result <- renderUI({
    req(model_fit())
    HTML('<span style="color: green; font-size: 16px; font-weight: bold; margin-top: 10px; display: flex; align-items: center;">
           <span style="margin-right: 8px; font-size: 20px;">&#10004;</span> Model built successfully
         </span>')
  })

  output$model_summary <- renderTable({
    req(model_fit())
    summary(model_fit())
  })
  
  output$model_pred <- DT::renderDataTable({
    req(model_fit())
    pred <- predict(model_fit(), testing(data_split()))
    pred_full <- cbind(testing(data_split()), pred)
    DT::datatable(pred_full, options = list(scrollX = TRUE))
  })
  
  output$model_mets <- renderTable({
    req(model_fit())
    if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
      pred_test <- predict(model_fit(), testing(data_split()))
      pred_train <- predict(model_fit(), training(data_split()))
      
      pred_train_df <- cbind(training(data_split()),".pred"=pred_train)
      pred_test_df <- cbind(testing(data_split()),".pred"=pred_test)
      
      target <- input$target_var
      
      met1_train <- rmse(pred_train_df, truth = !!sym(target), estimate = ".pred")
      met2_train <- mae(pred_train_df, truth = !!sym(target), estimate = ".pred")
      met3_train <- rsq(pred_train_df, truth = !!sym(target), estimate = ".pred")
      mets_train <- rbind(met1_train,met2_train,met3_train)
      names(mets_train) <- c("metric",".estimator","train_score")
      
      met1_test <- rmse(pred_test_df, truth = !!sym(target), estimate = ".pred")
      met2_test <- mae(pred_test_df, truth = !!sym(target), estimate = ".pred")
      met3_test <- rsq(pred_test_df, truth = !!sym(target), estimate = ".pred")
      mets_test <- rbind(met1_test,met2_test,met3_test)

      mets_combined <- cbind(mets_train, "test_score" = mets_test$".estimate") %>% 
        select(-".estimator")
      mets_combined
    } else {
      pred_test <- predict(model_fit(), testing(data_split()), type = "class")
      pred_train <- predict(model_fit(), training(data_split()), type = "class")
      
      pred_train_df <- cbind(training(data_split()), .pred_class = pred_train)
      pred_test_df <- cbind(testing(data_split()), .pred_class = pred_test)
      
      target <- input$target_var
      pred_train_df[[target]] <- as.factor(pred_train_df[[target]])
      pred_test_df[[target]] <- as.factor(pred_test_df[[target]])
      
      pred_train_df$.pred_class <- factor(pred_train_df$.pred_class, levels = levels(pred_train_df[[target]]))
      pred_test_df$.pred_class <- factor(pred_test_df$.pred_class, levels = levels(pred_test_df[[target]]))
      
      met1_train <- yardstick::accuracy(pred_train_df, truth = !!sym(target), estimate = .pred_class)
      met2_train <- yardstick::f_meas(pred_train_df, truth = !!sym(target), estimate = .pred_class)
      met3_train <- yardstick::precision(pred_train_df, truth = !!sym(target), estimate = .pred_class)
      mets_train <- rbind(met1_train, met2_train, met3_train)
      names(mets_train) <- c("metric", ".estimator", "train_score")
      
      met1_test <- yardstick::accuracy(pred_test_df, truth = !!sym(target), estimate = .pred_class)
      met2_test <- yardstick::f_meas(pred_test_df, truth = !!sym(target), estimate = .pred_class)
      met3_test <- yardstick::precision(pred_test_df, truth = !!sym(target), estimate = .pred_class)
      mets_test <- rbind(met1_test, met2_test, met3_test)
      
      mets_combined <- cbind(mets_train, "test_score" = mets_test$".estimate")
      mets_combined <- mets_combined %>% select(-".estimator")
      mets_combined
    }
  })
  
  session$onSessionEnded(function() { stopApp() })
}