
library(shiny)

ui <- fluidPage(
  titlePanel("Normal Distribution Generator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", "Mean:", 0, min = -100, max = 100),
      numericInput("variance", "Variance:", 1, min = 0.1, max = 100),
      numericInput("min", "Minimum:", -5, min = -100, max = 100),
      numericInput("max", "Maximum:", 5, min = -100, max = 100),
      numericInput("numSamples", "Number of Samples:", 10000, min = 1),
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("rejSampPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    #generates a sequence of numbers [min, max]
    x <- seq(input$min, input$max, length.out = 500)
    #pdf ul of normal dist, with parameters: mean, standars deviation
    y <- dnorm(x, mean = input$mean, sd = sqrt(input$variance))
    
    plot(x, y, type = "l", xlab = "X", ylab = "Y", main = "Probability Density Function")
    
    #generates samples using accept reject 
    samples <- reactive({
      
      numSamples <- input$numSamples
      samples <- numeric(numSamples)
      
      accepted_samples <- numeric(numSamples)
      accepted_samples_x <- numeric(numSamples)
      accepted_samples_y <- numeric(numSamples)
      
      rejected_samples_x <- numeric(numSamples)
      rejected_samples_y <- numeric(numSamples)
      
      count <- 0
      accepted_count <- 0
      rejected_count <- 0
      
      while (count < numSamples) {
        #2 uniformly distributed RV are generated to match the interest area   
        x <- runif(1, min = input$min, max = input$max)
        y <- runif(1, min = 0, max = dnorm(input$mean, mean = input$mean, sd = sqrt(input$variance)))
        
        # pdf(x) > y -=> accept, reject otherwise
        if (y < dnorm(x, mean = input$mean, sd = sqrt(input$variance))) {
          samples[count+1] <- x
          accepted_samples_x[rejected_count+1] <- x
          accepted_samples_y[rejected_count+1] <- y          
          accepted_count <- accepted_count + 1
          count <- count + 1
        }else{
          rejected_samples_x[rejected_count+1] <- x
          rejected_samples_y[rejected_count+1] <- y
          rejected_count <- rejected_count + 1
          count <- count + 1
        }
      }
      #creates a list that contains the x and y coordinates of the accepted and rejected samples 
      list(accepted_samples_x = accepted_samples_x, accepted_samples_y = accepted_samples_y, rejected_samples_x = rejected_samples_x, rejected_samples_y = rejected_samples_y)
    
    })
    
    #plot the samples 
    points(samples()$accepted_samples_x, samples()$accepted_samples_y, pch = 19, col = "red")
    points(samples()$rejected_samples_x, samples()$rejected_samples_y, pch = 3, col = "blue")
  })
}

shinyApp(ui, server)