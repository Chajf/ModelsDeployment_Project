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
page1 <- page_fluid(
  theme = bs_theme(preset = "pulse"),
  layout_sidebar(
    sidebar = sidebarPanel(
      sliderInput("bins2",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30
      ),
      width = 12,
    ),
    plotOutput("distPlot2")
  )
)

page2 <- page_fillable(
  layout_columns(
    card(titlePanel("Options"),
         sliderInput("bins",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30
    )),
    card(plotOutput("distPlot")),
    col_widths = c(2,10)
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

# page combined 
page_navbar(
  title = titlePanel(HTML("<ins>VISUAL MODEL </ins></br>&emsp;<ins>B</ins><span style='font-size:150%; font-weight:bold;'>[UI]</span><ins>LDING</ins>&emsp;&emsp;")),
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
  sidebar = sidebarPanel(
    titlePanel(
      HTML("Sidebar")
    ),
    fileInput("upload", "Upload a file"),
    titlePanel((HTML("Summary"))),
    #column(width = 12,tableOutput('table')),
    sliderInput("bins3",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30
    ),
    width = 12,
  ),
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
  nav_panel(title = titlePanel(HTML("<span style='font-size:80%; font-weight:bold;'>ONE</span>")), p(page1)),
  nav_panel(title = titlePanel(HTML("<span style='font-size:80%; font-weight:bold;'>TWO</span>")), p(page2))
)


