library(shiny)
library(bslib)
library(DT)

thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- fillPage(
  tags$style(HTML("
    .container-fluid {
      max-width: 100%;
    }
    #df_table_div {
      overflow-x: auto;
      overflow-y: auto;
      margin-left: 20px;
      margin-right: 20px;
    }
    #data_title {
      margin-left: 20px;
      margin-top: 10px;
    }
  ")),
  layout_sidebar(
    sidebar = sidebar(
      titlePanel(HTML("Imputation")),
      width = 500,
      open = "closed",
      DTOutput("missing_values"),
      uiOutput("imputation"),
    ),
    fillable = T,
    fill = T,
    div(id = "data_title", titlePanel(HTML("Data"))),
    div(id = "df_table_div", DTOutput("df_table")),
  )
)

# Tab 2

tab2 <- fluidPage(
  layout_sidebar(
    sidebar = sidebar(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30
      ),
      width = 300,
      open = "open",
    ),
    plotOutput("distPlot")
  )
)



# Tab 3

tab3 <- "place_holder"


# Tab 4

tab4 <- "place_holder"



# Page

exploration <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Data Exploration</span>"),
    nav_panel("Data", tab1), # data browse and imputation
    nav_panel("Variables", tab2), # data description, variable statistics
    nav_panel("Visualisation", tab3) # distributions, boxplots, dependecies 
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

#===============================================================================
# Page 2
#===============================================================================

page2 <- page_fillable(
  navset_card_underline(
    title = "Histograms by species",
    nav_panel("dwa", "content"),
    nav_panel("trzy", "content")
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

#===============================================================================
# Sidebar
#===============================================================================

sidebar_main <- sidebar(
  tags$style(HTML("
                  .card-body {padding: 0px;} 
                  .container-fluid{padding-left: 0px;} 
                  .bslib-sidebar-layout>.main{padding: 0px;}
                  ")),
  titlePanel(HTML("Upload file")),
  fileInput("upload", "Upload a .csv/.tsv file", accept = c(".csv",".tsv"), multiple = FALSE),
  selectInput("sep", "Choose the separator:",
              choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
  titlePanel((HTML("Summary"))),
  width = 365,
  open = "desktop",
  fillable = TRUE,
  DTOutput("summ_table"),
  titlePanel(HTML("Variables types")),
  uiOutput("varSelect"),
)

#===============================================================================
# Combine
#===============================================================================

page_navbar(
  title = HTML("&emsp;<ins>VISUAL MODEL </ins></br>&emsp;<ins>B</ins><span style='font-size:150%; font-weight:bold;'>[UI]</span><ins>LDING</ins>&emsp;"),
  theme = bs_theme(
    preset = "pulse",
  ),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>DATA EXPLORATION</span>"), exploration),
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>MODELLING & RESULTS</span>"), page2)
)


