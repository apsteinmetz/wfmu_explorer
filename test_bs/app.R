# TEST BSLIB

library(shiny)
library(bslib)

sidebar1 = sidebar(
  sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ))

# Define UI for application that draws a histogram
ui <- page_fluid(
  navset_card_tab(
    height = 450,
    full_screen = TRUE,
    title = "HTML Widgets",
    nav_panel(
      "Plotly",
      card_title("A plotly plot"),
      plotly_widget
    ),
    nav_panel(
      "Leaflet",
      card_title("A leaflet plot"),
      plotly_widget
    ),
    nav_panel(
      shiny::icon("circle-info"),
      markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
    )
  )
  
  )


# server -----------------------------------------------------------------------
server <- function(input, output) {
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

# bs_theme_preview()

