library(shiny)
library(DT)
library(bsicons)
library(LiblineaR)
library(shinymodels)
library(tidymodels)
library(plotly)
library(psych)
library(dplyr)
library(ranger)
library(xgboost)

tidymodels_prefer()
conflicted::conflicts_prefer(shiny::observe)
conflicted::conflicts_prefer(plotly::layout)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    df <- read.csv(input$upload$datapath, sep = input$sep, header = input$header, dec = if(input$sep == ";") "," else ".")
    # updateSelectInput(session, "summ_variable", choices = names(df))  # Update the selectInput here
    updateSelectInput(session, "variables_vis", choices = names(df))  # Update the selectInput here
    df
  })
  
  modifiedData <- reactiveValues(df = NULL)
  
  observe({
    modifiedData$df <- data()
    # updateSelectInput(session, "variables", choices = names(df))
  })
  
  
  output$summ_table <- renderDT(
    data.frame(
        c(ncol(data()), nrow(modifiedData$df), sum(is.na(modifiedData$df))),
        row.names = c('Number of variables','Number of observations','Missing values')) |> 
      `colnames<-`('Value'),
    options = list(dom = 't'))
  
  modifiedData <- reactiveValues(df = NULL)
  
  observe({
    output$df_table <- renderDT(datatable(modifiedData$df))
  })
  
  output$varSelect <- renderUI({
    df <- data()
    lapply(names(df), function(name){
      tagList(
        selectInput(inputId = name, 
                    label = paste("Choose a type for variable:", name), 
                    choices = if(is.numeric(df[[name]])) c("Numeric", "Character", "Logical") else c("Character", "Numeric", "Logical")),
        htmlOutput(paste0(name, "_error")),
        hr()
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
          output[[paste0(name, "_error")]] <- renderUI({
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
                                 input$remove_near_zero_var, input$yeojohnson, input$class_other, 
                                 input$inv_transform), na.rm = TRUE)
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
    if (input$yeojohnson) {
      numeric_vars <- names(modifiedData$df)[sapply(modifiedData$df, is.numeric)]
      selectizeInput("select_yj",
                     "Select variables for Yeo-Johnson trasform",
                     choices = numeric_vars,
                     multiple = TRUE
      )
    }
  })
  
  output$transforms_ui_inv <- renderUI({
    if (input$inv_transform) {
      numeric_vars <- names(modifiedData$df)[sapply(modifiedData$df, is.numeric)]
      selectizeInput("select_inv",
                     "Select variables for Inverse trasform",
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
      "This model need 'Dummy Variable' preprocessing step!"
    } else {
      NULL
    }
  }
  
  req_data <- function(input) {
    if (!is.data.frame(input)) {
      "Provide data!"
    } else {
      NULL
    }
  }
  
  any_nans <- function(input) {
    if (anyNA.data.frame(input)) {
      "Data can't have any NA values!"
    } else {
      NULL
    }
  }
  
  model_fit <- eventReactive(input$train_model, {
    
    validate(req_data(modifiedData$df))
    validate(any_nans(modifiedData$df))
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
    
    if (input$yeojohnson){
      rec <- rec %>% 
        step_YeoJohnson(input$select_yj)
    }
    
    if (input$inv_transform){
      rec <- rec %>% 
        step_inverse(input$select_inv)
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
    if (sapply(modifiedData$df[input$target_var],class)=="numeric"){
      print(head(testing(data_split()), n=1))
      pred <- predict(model_fit(), testing(data_split()))
      pred_full <- cbind(testing(data_split()), pred)
      DT::datatable(pred_full, options = list(scrollX = TRUE))
    } else {
      target <- input$target_var
      pred_test_df <- testing(data_split())
      pred_test_df[[target]] <- as.factor(pred_test_df[[target]])
      pred <- predict(model_fit(), pred_test_df, type = "class")
      pred_full <- cbind(pred_test_df, pred)
      DT::datatable(pred_full, options = list(scrollX = TRUE))
    }
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
  
  output$imputation <- renderUI({
    df <- modifiedData$df 
    lapply(names(df), function(name) {
      tagList(
        tags$div(
          selectInput(
            inputId = paste0(name, "_impute_method"), 
            label = HTML(paste("Variable: <b>", name, "</b></br>Choose an imputation method")), 
            choices = if(is.numeric(df[[name]])) c("Median") else "Mode"
          ),
          class = "custom-spacing"
        ),
        tags$div(
          textInput(
            inputId = paste0(name, "_na_strings"),
            label = HTML(paste("Enter values to be treated as NA's (comma separated)")),
            value = ""
          ),
          class = "custom-spacing"
        ),
        tags$div(
          actionButton(
            inputId = paste0(name, "_impute_button"), 
            label = "Impute"
          ),
          class = "custom-spacing"
        ),
        tags$div(hr())
      )
    })
  })
  
  observe({
    lapply(names(data()), function(name){
      observeEvent(input[[paste0(name, "_impute_button")]], {
        na_strings <- unlist(strsplit(input[[paste0(name, "_na_strings")]], ","))
        modifiedData$df[[name]][modifiedData$df[[name]] %in% na_strings] <<- NA
        if(input[[paste0(name, "_impute_method")]] == "Median") {
          modifiedData$df[[name]][is.na(modifiedData$df[[name]])] <<- median(modifiedData$df[[name]], na.rm = TRUE)
        } else if(input[[paste0(name, "_impute_method")]] == "Mode") {
          mode_val <- as.character(names(which.max(table(modifiedData$df[[name]]))))
          modifiedData$df[[name]][is.na(modifiedData$df[[name]])] <<- mode_val
        }
      })
    })
  })
  
  calculate_dataframe_statistics <- function(df) {
    
    calculate_numeric_stats <- function(x, var_name) {
      stats <- data.frame(
        Variable = var_name,
        Mean = round(mean(x, na.rm = TRUE), 3),
        Median = round(median(x, na.rm = TRUE), 3),
        Std_Dev = round(sd(x, na.rm = TRUE), 3),
        Variance = round(var(x, na.rm = TRUE), 3),
        Min = round(min(x, na.rm = TRUE), 3),
        Max = round(max(x, na.rm = TRUE), 3),
        Skewness = round(psych::skew(x, na.rm = TRUE), 3),
        Kurtosis = round(psych::kurtosi(x, na.rm = TRUE), 3)
      )
      return(stats)
    }
    
    calculate_character_stats <- function(x, var_name) {
      value_counts <- sort(table(x), decreasing = TRUE)
      most_common_values <- names(value_counts)[1:min(5, length(value_counts))]
      most_common_counts <- as.numeric(value_counts[1:min(5, length(value_counts))])
      most_common <- paste(most_common_values, "(", most_common_counts, ")", sep = "", collapse = ", ")
      
      stats <- data.frame(
        Variable = var_name,
        Count = length(x),
        Mode = most_common_values[1],
        Unique_Values = length(unique(x)),
        Most_Common = most_common
      )
      return(stats)
    }
    
    numeric_vars <- df %>% select(where(is.numeric))
    character_vars <- df %>% select(where(is.character))
    
    numeric_stats <- lapply(names(numeric_vars), function(var) {
      calculate_numeric_stats(numeric_vars[[var]], var)
    })
    character_stats <- lapply(names(character_vars), function(var) {
      calculate_character_stats(character_vars[[var]], var)
    })
    
    numeric_result_df <- bind_rows(numeric_stats)
    character_result_df <- bind_rows(character_stats)
    
    return(list(numeric = numeric_result_df, character = character_result_df))
  }
  
  output$numeric_var_table <- renderDT({
    req(data())
    df <- modifiedData$df
    result_df <- calculate_dataframe_statistics(df)
    datatable(result_df$numeric)
  })
  
  output$character_var_table  <- renderDT({
    req(data())
    df <- modifiedData$df
    result_df <- calculate_dataframe_statistics(df)
    result_df_char <- result_df$character
    colnames(result_df_char) <- c('Variable','Count', 'Mode', 'Unique values', '5 most common values')
    datatable(result_df_char)
  })
  
  output$visualisation_plot <- renderPlotly({
    req(input$variables_vis, data())
    df <- modifiedData$df
    
    # If one variable is selected
    if(length(input$variables_vis) == 1) {
      var <- df[[input$variables_vis]]
      var_name <- input$variables_vis
      
      # If the variable is numeric, plot a histogram and density plot
      if(is.numeric(var)) {
        p <- plot_ly(df, x = ~var, type = "histogram",  name = "Histogram") %>%
          add_trace(y = ~density(var)$y / sum(density(var)$y), x = ~density(var)$x, type = "scatter", mode = "lines", name = "Density", yaxis = "y2") %>%
          layout(xaxis = list(title = var_name),
                 yaxis = list(title = paste0("count",var_name), range = c(0,NA)),
                 yaxis2 = list(overlaying = "y", side = "right", title = "Density", range = c(0,NA)))
        p
      } else {  # If the variable is not numeric, plot a bar chart
        p <- plot_ly(df, x = ~var, type = "histogram", name = "Histogram") %>%
          layout(xaxis = list(title = var_name))
        p
      }
      
    } else if(length(input$variables_vis) == 2) {  # If two variables are selected
      var1 <- df[[input$variables_vis[1]]]
      var2 <- df[[input$variables_vis[2]]]
      var1_name <- input$variables_vis[1]
      var2_name <- input$variables_vis[2]
      
      # If one variable is numeric and the other is not, plot a boxplot
      if(is.numeric(var1) && !is.numeric(var2)) {
        p <- plot_ly(df, y = ~var1, color = ~var2, type = "box") %>%
          layout(yaxis = list(title = var1_name), xaxis = list(title = var2_name))
        p
      } else if(!is.numeric(var1) && is.numeric(var2)) {
        p <- plot_ly(df, y = ~var2, color = ~var1, type = "box") %>%
          layout(yaxis = list(title = var2_name), xaxis = list(title = var1_name))
        p
      } else if(is.numeric(var1) && is.numeric(var2)) {  # If both variables are numeric, plot a scatter plot
        p <- plot_ly(df, x = ~var1, y = ~var2, mode = "markers") %>%
          layout(xaxis = list(title = var1_name), yaxis = list(title = var2_name))
        p
      } else {  # If not both variables are numeric, show a warning message
        showModal(modalDialog(
          title = "Warning",
          "At least one variable needs to be numeric for creating plot."
        ))
      }
    }
  })
}
