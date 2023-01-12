library(shiny)

ui <- fluidPage(
  titlePanel("Normal Distribution Generator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean", "Mean:", min = -10, max = 10, value = 0, step = 0.1),
      sliderInput("variance", "Variance:", min = 0.1, max = 10, value = 1, step = 0.1),
      sliderInput("min", "Minimum:", min = -10, max = 10, value = -4, step = 0.1),
      sliderInput("max", "Maximum:", min = -10, max = 10, value = 4, step = 0.1)
    ),
    mainPanel(
      plotOutput("dist_plot")
    )
  )
)

server <- function(input, output) {
  dist <- reactive({
    # Generate the normal distribution
    x <- seq(input$min, input$max, length.out = 100)
    y <- dnorm(x, mean = input$mean, sd = sqrt(input$variance))
    return(data.frame(x, y))
  })
  
  output$dist_plot <- renderPlot({
    plot(dist()$x, dist()$y, type = "l", xlab = "Value", ylab = "Density", main = "Normal Distribution")
  })
  
  # Get the maximum density(we will use it to scale the uniform distribution)
  max_density <- reactive({
    max(dist()$y)
  })
  
  # Print the maximum density to the console
  observe({
    print(max_density())
  })
}

shinyApp(ui = ui, server = server)