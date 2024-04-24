library(shiny)
library(DT)
library(plotly)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    df <- read.csv(input$upload$datapath, sep = input$sep)
    updateSelectInput(session, "variables", choices = names(df))  # Update the selectInput here
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
  
  # Render the plot based on the selected variables
  output$variables_plot <- renderPlotly({
    req(input$variables, data())
    df <- modifiedData$df
    
    # If one variable is selected
    if(length(input$variables) == 1) {
      var <- df[[input$variables]]
      var_name <- input$variables
      
      # If the variable is numeric, plot a histogram and density plot
      if(is.numeric(var)) {
        p <- plot_ly(df, x = ~var, type = "histogram", histnorm = "probability", name = "Histogram") %>%
          add_trace(y = ~density(var)$y, x = ~density(var)$x, type = "scatter", mode = "lines", name = "Density") %>%
          layout(xaxis = list(title = var_name))
        p
      } else {  # If the variable is not numeric, plot a bar chart
        p <- plot_ly(df, x = ~var, type = "histogram", name = "Histogram") %>%
          layout(xaxis = list(title = var_name))
        p
      }
      
    } else if(length(input$variables) == 2) {  # If two variables are selected
      var1 <- df[[input$variables[1]]]
      var2 <- df[[input$variables[2]]]
      var1_name <- input$variables[1]
      var2_name <- input$variables[2]
      
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
