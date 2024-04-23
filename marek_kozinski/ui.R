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

# Define UI for application that draws a histogram
# page_fluid(
#   # Application title
#   # titlePanel("Visual Model bUIlding"),
  # theme = bs_theme(
  #   bg = "#101010",
  #   fg = "#FFF",
  #   primary = "#E69F00",
  #   secondary = "#0072B2",
  #   success = "#009E73",
  #   base_font = font_google("Inter"),
  #   code_font = font_google("JetBrains Mono")
  # ),
#   lang = "en",
#   titlePanel(
#     HTML("VISUAL MODEL B<span style='font-size:150%; font-weight:bold;'>[UI]</span>LDING")
#   ),
#   # Sidebar with a slider input for number of bins
#   layout_sidebar(
#     sidebar = sidebarPanel(
#             titlePanel(
#               HTML("Sidebar")
#             ),
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30
#               ),
#             # sliderInput("bins",
#             #             "Number of bins:",
#             #             min = 1,
#             #             max = 50,
#             #             value = 30
#             #   ),
#             width = 12,
#             ),
#     splitLayout(
#       mainPanel(
#         navset_underline(
#           # title = "",
#           nav_panel(
#             title = "cokolwiek",
#             column(plotOutput("distPlot"),
#                    width = 6),
#             column(titlePanel(HTML("Sidebar")),
#                    width = 6)
#           ),
#           nav_panel(
#             title = "cokolwiek2",
#             #content
#           ),
#         ),
#         width = 12,
#       )
#     )
#   )
# )

#===============================================================================

# first page
# page1 <- page_fillable(
#   theme = bs_theme(preset = "pulse"),
#   layout_sidebar(
#     sidebar = sidebar(
#       sliderInput("bins2",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30
#       ),
#       width = 300,
#       open = "always",
#     ),
#     plotOutput("distPlot2")
#   )
# )

# page2 <- page_fillable(
#   layout_columns(
#     card(titlePanel("Options"),
#          sliderInput("bins",
#                        "Number of bins:",
#                        min = 1,
#                        max = 50,
#                        value = 30
#     )),
#     card(plotOutput("distPlot")),
#     col_widths = c(2,10)
#   ),
#   theme = bs_theme(
#     preset = "pulse"
#   )
# )

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
  # tags$head(
  #   tags$style(
  #     HTML("
  #       .sidebar {
  #         position: fixed;
  #         # top: 0;
  #         # left: 0;
  #         # bottom: 0;
  #         # width: 100px; /* Adjust as needed */
  #         background-color: #f8f9fa; /* Sidebar background color */
  #         box-shadow: 0 0 10px rgba(0, 0, 0, 0.1); /* Optional shadow effect */
  #       }
  #     ")
  #   )
  # ),
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

# bs_global_add_rules(HTML(".navbar .navbar-default .navbar-static-top {margin-bottom: 0px}"))
# page combined 
page_navbar(
  # tags$head(tags$style(HTML(".navbar .navbar-default .navbar-static-top {margin-bottom: 0px}"))),
  # tags$style(".content-wrapper{margin-left: 0px;}"),
  title = HTML("<ins>VISUAL MODEL </ins></br>&emsp;<ins>B</ins><span style='font-size:150%; font-weight:bold;'>[UI]</span><ins>LDING</ins>&emsp;&emsp;"),
  # tym steruje się tematem we wszsytkich page'ach
  # osobno w np. page2 nie zmienia sie
  theme = bs_theme(
    # bg = "black",
    # fg = "white",
    # primary = "#E69F00",
    # secondary = "#0072B2",
    # success = "#009E73",
    # base_font = font_google("Inter"),
    # code_font = font_google("JetBrains Mono")
    preset = "pulse",
  ),
  underline = TRUE,
  sidebar = sidebar_main,
  # sidebar = sidebarPanel(
  #     card(titlePanel(
  #       HTML("Sidebar")
  #     )),
  #     card(fileInput("upload", "Upload a file")),
  #     card(sliderInput("bins3",
  #                      "Number of bins:",
  #                      min = 1,
  #                      max = 50,
  #                      value = 30
  #     )),
  #     width = 12
  # ),
  nav_panel(title = HTML("<span style='font-size:80%; font-weight:bold;'>DATA EXPLORATION</span>"), page1),
  nav_panel(title = HTML("<span style='font-size:80%; font-weight:bold;'>MODELLING & RESULTS</span>"), page2)
)


