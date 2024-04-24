library(shiny)
library(DT)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath, sep = input$sep)
  })
  
  modifiedData <- reactiveValues(df = NULL)
  
  observe({
    modifiedData$df <- data()
  })
  
  output$summ_table <- renderDT(
    data.frame(
        c(ncol(data()), nrow(modifiedData$df), sum(is.na(modifiedData$df))),
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
  
  observe({
    output$df_table <- renderDT(modifiedData$df)
  })
  
  output$varSelect <- renderUI({
    df <- data()
    lapply(names(df), function(name){
      tagList(
        selectInput(inputId = name, 
                    label = paste("Choose a type for variable:", name), 
                    choices = if(is.numeric(df[[name]])) c("Numeric", "Character", "Logical") else c("Character", "Numeric", "Logical")),
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
  
  output$missing_values <- renderDT({
    req(data())
    df <- modifiedData$df 
    missing_values <- df %>%
      dplyr::summarise_all(function(x) sum(is.na(x))) %>%
      tidyr::gather(key = "Variable", value = "Number of Missing Values")
    DT::datatable(missing_values, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$imputation <- renderUI({
    df <- modifiedData$df 
    lapply(names(df), function(name){
      tagList(
        selectInput(inputId = paste0(name, "_impute_method"), 
                    label = HTML(paste("Choose an imputation method for variable: <b>", name, "</b>")), 
                    choices = if(is.numeric(df[[name]])) c("Median") else "Mode"),
        textInput(inputId = paste0(name, "_na_strings"),
                  label = HTML(paste("Enter values to be treated as NA's (comma separated)")),
                  value = ""),
        actionButton(inputId = paste0(name, "_impute_button"), label = "Impute")
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
  
  
}
