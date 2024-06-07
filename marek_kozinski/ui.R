library(shiny)
library(bslib)
library(DT)

thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- fluidPage(
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



# Tab 2

tab2 <- "place_holder"



# Tab 3

tab3 <- "place_holder"



# Page

exploration <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Data Exploration</span>"),
    nav_panel("Imputation", tab1), # data imputation
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

tab_m1 <- fluidPage(
  layout_sidebar(
    sidebar = sidebar(
      titlePanel("Model target"),
      uiOutput("target_var_ui"),
      titlePanel("Model selection"),
      selectInput("model_select","Model",
                  choices = c("Decision Tree","XGBoost","Random Forest","SVM")),
      titlePanel("Dataset split"),
      sliderInput("split","Train size", min = 0.1, max = 0.95, step = 0.01, value = 0.8),
      titlePanel("Preprocessing steps"),
      checkboxInput("normalize", "Normalize"),
      checkboxInput("dummy", "Dummy variables"),
      checkboxInput("pca", "PCA"),
      conditionalPanel(condition = "input.pca == true", uiOutput("pca_ui")),
      checkboxInput("remove_zero_var", "Remove zero variance"),
      checkboxInput("remove_near_zero_var", "Remove near zero variance"),
      checkboxInput("log_transform", "Logarythm transform"),
      conditionalPanel(condition = "input.log_transform == true", uiOutput("transforms_ui")),
      checkboxInput("class_other", "Class other"),
      conditionalPanel(condition = "input.class_other == true", uiOutput("class_other_ui")),
      checkboxInput("sqrt_transform", "Square root transform"),
      width = 300,
      open = "open",
    ),
    titlePanel("Model building summary"),
    uiOutput("v_box"),
    uiOutput("model_select"),
    uiOutput("train_size"),
    uiOutput("type"),
    uiOutput("pre_steps"),
    actionButton("train_model", "Train Model"),
    htmlOutput("build_result")
  )
)

tab_m2 <- fluidPage(
    #tableOutput("model_summary"),
    div(class = "centered-table", titlePanel("Metrics summary")),
    tags$head(
      tags$style(HTML("
      .centered-table {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .dataTables_wrapper {
        width: auto !important;
        margin: 0 auto !important;
      }
    "))
    ),
    div(class = "centered-table", tableOutput("model_mets")),
    titlePanel("Model predictions"),
    DT::dataTableOutput("model_pred"),
)

page2 <- page_fillable(
  navset_card_underline(
    title = "Model Building",
    nav_panel("Building", tab_m1),
    nav_panel("Results", tab_m2)
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


