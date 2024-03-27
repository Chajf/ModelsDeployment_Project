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
page_sidebar(
    # Application title
    # titlePanel("Visual Model bUIlding"),
    titlePanel(
      HTML("VISUAL MODEL </br>B<span style='font-size:150%; font-weight:bold;'>[UI]</span>LDING")
    ),
    # Sidebar with a slider input for number of bins
    sidebar = sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30
              ),
              width = 12,
            ),
    mainPanel(
      navset_card_tab(
        # title = "",
        nav_panel(
          title = "cokolwiek",
          plotOutput("distPlot")
        ),
        nav_panel(
          title = "cokolwiek2",
          
        ),
      ),
      width = 12,
    ),
    
    theme = bs_theme(
      bg = "#101010",
      fg = "#FFF",
      primary = "#E69F00",
      secondary = "#0072B2",
      success = "#009E73",
      base_font = font_google("Inter"),
      code_font = font_google("JetBrains Mono")
    ),
    lang = "en",
)