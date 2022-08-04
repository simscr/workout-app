library(shiny)

ui <- fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

shinyApp(ui = ui, server = server)

# browseURL("https://www.jdtrat.com/blog/connect-shiny-google/")

# browseURL("https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/")

# browseURL("https://jnolis.com/blog/shiny_mobile/")

# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "workout-app/.secrets"
# )
