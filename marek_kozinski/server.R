library(shiny)
library(DT)
library(bsicons)

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
    value_box(title = "Target variable",
              value = input$target_var)
  })
  
  output$type <- renderUI({
    col <- input$target_var
    type <- ifelse(sapply(modifiedData$df[col],class)=="numeric","Regression","Classification")
    icon <- ifelse(type=="Regression","graph-up","bullseye")
    value_box(title = "Type of task",
              value = type,
              showcase = bs_icon(icon))
  })
  
  output$inputs <- renderUI({
    req(input$preproc)
    lapply(input$preproc, function(i) {
      if (i == "PCA") {
        sliderInput("pca_comp",
                    "Number of components",
                    min = 2,
                    max = ncol(modifiedData$df),
                    value = round(ncol(modifiedData$df)/2),
                    step = 1
        )
      } else if (i == "Class other") {
        sliderInput("other",
                    "Threshold",
                    min = 0.01,
                    max = 0.95,
                    value = 0.15
        )
      } else if (i %in% c("Logarythm transform", "Square root transform")) {
        numeric_vars <- names(modifiedData$df)[sapply(modifiedData$df, is.numeric)]
        selectizeInput(paste0("select_", gsub(" ", "_", i)),
                       paste0("Select variables for ", i),
                       choices = numeric_vars,
                       multiple = TRUE
        )
      }
    })
  })
}