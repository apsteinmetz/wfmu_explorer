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
  theme = bs_theme(bootswatch = "shiny"),
  input_dark_mode(mode = "dark"),
  page_navbar(
    title = "My App",
    nav_panel(title = "One",
              card(
                sidebar = "sidebar",
                p("First tab content."))),
    nav_panel(title = "Two", p("Second tab content.")),
    nav_panel(title = "Three", p("Third tab content")),
    nav_menu(
      title = "Four",
      nav_panel(title = "A", p("First tab content.")),
      nav_panel(title = "B", p("Second tab content.")),
      
    ),
    nav_spacer(),
    nav_item(input_dark_mode()), 
  )
)

# server -----------------------------------------------------------------------
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(
            x,
            breaks = bins,
            col = 'darkgray',
            border = 'white',
            xlab = 'Waiting time to next eruption (in mins)',
            main = 'Histogram of waiting times'
        )
    })
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

bs_theme_preview()

