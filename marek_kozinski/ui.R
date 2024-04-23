#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(DT)

thematic::thematic_shiny()

page1 <- page_fillable(
  navset_card_tab(
    title = "Histograms by species",
    nav_panel("jeden", 
              p(
                layout_sidebar(
                  sidebar = sidebar(
                    sliderInput("bins2",
                                "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30
                    ),
                    width = 300,
                    open = "always",
                  ),
                  plotOutput("distPlot2")
                )
              )),
    nav_panel("dwa", "content"),
    nav_panel("trzy", "content")
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

page2 <- page_fillable(
  navset_card_underline(
    title = "Histograms by species",
    nav_panel("jeden", 
              p(
                layout_sidebar(
                  sidebar = sidebar(
                    sliderInput("bins",
                                "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30
                    ),
                    width = 300,
                    open = "always",
                  ),
                  plotOutput("distPlot")
                )
              )),
    nav_panel("dwa", "content"),
    nav_panel("trzy", "content")
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

sidebar_main <- sidebar(
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

page_navbar(
  title = HTML("<ins>VISUAL MODEL </ins></br>&emsp;<ins>B</ins><span style='font-size:150%; font-weight:bold;'>[UI]</span><ins>LDING</ins>&emsp;&emsp;"),
  theme = bs_theme(
    preset = "pulse",
  ),
  underline = TRUE,
  sidebar = sidebar_main,
  nav_panel(title = HTML("<span style='font-size:80%; font-weight:bold;'>DATA EXPLORATION</span>"), page1),
  nav_panel(title = HTML("<span style='font-size:80%; font-weight:bold;'>MODELLING & RESULTS</span>"), page2)
)


