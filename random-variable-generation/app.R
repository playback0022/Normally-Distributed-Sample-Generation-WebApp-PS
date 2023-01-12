library("shiny")
library("bslib")


ui = fluidPage(
  # bootstrap theme
  theme = bs_theme(bg = "#232324", fg = "white", primary = "#F9E153"),
  
  h4(strong("Normally Distributed Sample Generation Engine"), align = "center"),
  
  br(),
  tabsetPanel(
             tabPanel(title = "Box-Muller",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "numberOfIterationsBoxMuller", label = "Number of Iterations", min = 500, max = 100000, value = 10000, step =  1),
                          sliderInput(inputId = "sampleRangeBoxMuller", label = "Sample Range", min = -500, max = 500, value = c(-100, 100), step = 1),
                          sliderInput(inputId = "numberOfBarsBoxMuller", label = "Number of Histogram Bars", min = 50, max = 1000, value = 500, step =  1),
                          numericInput(inputId = "desiredMeanBoxMuller", label = "Desired Mean", value = 0, step = 0.1),
                          numericInput(inputId = "desiredVarianceBoxMuller", label = "Desired Variance", min = 0, value = 1, step = 0.1),
                          checkboxInput(inputId = "showPDFBoxMuller", label = "Show PDF", value = FALSE),
                          submitButton(text = "Generate"),
                          br(),
                          p("The Box-Muller method makes use of two randomly generated uniformly distributed numbers to generate a pair of numbers with a standard normal distribution (mean = 0, variance = 1)."),
                          p("The generated sample can then be transformed into another normally distributed sample with the specified mean and variance.")
                        ),
                        mainPanel(
                          br(),
                          plotOutput(outputId = "BoxMuller")
                        )
                      )),
             
             tabPanel(title = "Rejection",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "numberOfIterationsRejection", label = "Number of Iterations", min = 500, max = 100000, value = 10000, step =  1),
                          sliderInput(inputId = "sampleRangeRejection", label = "Sample Range", min = -500, max = 500, value = c(-100, 100), step = 1),
                          sliderInput(inputId = "numberOfBarsRejection", label = "Number of Histogram Bars", min = 50, max = 1000, value = 500, step =  1),
                          numericInput(inputId = "desiredMeanRejection", label = "Desired Mean", value = 0, step = 0.1),
                          numericInput(inputId = "desiredVarianceRejection", label = "Desired Variance", min = 0, value = 1, step = 0.1),
                          radioButtons(inputId = "typeOfGraph", label = "Type of Graph", choices = c("Histogram" = 0, "Acceptance Visualization" = 1)),
                          checkboxInput(inputId = "showPDFRejection", label = "Show PDF", value = FALSE),
                          submitButton(text = "Generate"),
                          br(),
                          p("Rejection Sampling makes use of two independent uniformly distributed random variables to generate an element in the specified range and a number in the range of possible values taken by the desired normal distribution's PDF."),
                          p("When the (y-axis) generated value of the latter uniform variable is greater than the value of the PDF at the former uniform number, said number is rejected. Else, it is accepted and admitted into the sample."),
                          p("Intuitively, at the points at which the PDF is greater, more uniform numbers will be accepted, since their pair y-axis uniform values have a larger 'acceptance' range.")
                        ),
                        mainPanel(
                          br(),
                          plotOutput(outputId = "Rejection")
                        )
                      )),
             ),
  br(),
  
  tags$div(h6("This WebApp was developed as a project for the ", strong("Probabilities and Statistics"), " course of the ", strong("University of Bucharest.")),
  h6("Project Contributors: ", strong("playback0022 (Toma), IRadu15 (Radu)")), style = "background-color:#303030; padding: 0.5rem; margins: 0; text-align: center;")
)


server = function(input, output) {
  
  output$BoxMuller = renderPlot ({
    
    numberOfIterations = input$numberOfIterationsBoxMuller
    mean = input$desiredMeanBoxMuller
    variance = input$desiredVarianceBoxMuller
    startRange = input$sampleRangeBoxMuller[1]
    endRange = input$sampleRangeBoxMuller[2]
    numberOfBreaks = input$numberOfBarsBoxMuller 
    
    generateNormallyDistributedPair <- function () {
      # generating the two independent uniformly distributed random variables
      uniformFirst <- runif(1)
      uniformSecond <- runif(1)
      
      # computing various component parts of the formula
      k <- sqrt(-2 * log(uniformFirst))
      t <- 2 * pi * uniformSecond
      
      # generating the normally distributed random variables
      normalPair <- c(k * cos(t), k * sin(t))
      return((normalPair))
    }
    
    # vector storing the generated samples
    generatedSample = c()
    
    for (i in seq(1, numberOfIterations/2 + numberOfIterations %% 2, 1)) {
      sample = generateNormallyDistributedPair()
      # transform generated sample (normal distribution with mean = 0, 
      # variance = 1) to the normal distribution with the specified parameters;
      # multiplying by the standard deviation and adding mean of the 
      # desired distribution
      sample = sqrt(variance) * sample + mean
      # filter out the elements outside the specified range of values
      sample = sample[sample >= startRange & sample <= endRange]
      
      generatedSample = c(generatedSample, sample)
    }
    
    if (length(generatedSample)) {
      # plotting the histogram of the generated sample
      par(bg = "#f2f2f2")
      hist(generatedSample, main = "Box-Muller Sampling", col = "#b5b5b5", border = "#b5b5b5", xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
      # generating the sequence of values in the specified
      # range based on which to generate and plot the PDF 
      x = seq(startRange, endRange, 0.01)
      # plotting PDF over the histogram
      if (input$showPDFBoxMuller)
        curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "#F9E153", add = TRUE)
    }
  })
  
  output$Rejection <- renderPlot({
    numberOfIterations = input$numberOfIterationsRejection
    mean = input$desiredMeanRejection
    variance = input$desiredVarianceRejection
    startRange = input$sampleRangeRejection[1]
    endRange = input$sampleRangeRejection[2]
    numberOfBreaks = input$numberOfBarsRejection 
    
    #generates a sequence of numbers [min, max]
    x <- seq(startRange, endRange, length.out = 500)
    #pdf of normal dist, with parameters: mean, standard deviation
    y <- dnorm(x, mean = mean, sd = sqrt(variance))
    
    plot(x, y, type = "l", xlab = "X", ylab = "Y", main = "Probability Density Function")
    
    #generates samples using accept reject 
    
    accepted_samples_x <- numeric(numberOfIterations)
    accepted_samples_y <- numeric(numberOfIterations)
    
    rejected_samples_x <- numeric(numberOfIterations)
    rejected_samples_y <- numeric(numberOfIterations)
    
    accepted_count <- 0
    rejected_count <- 0
    count = 0 
    
    while (count < numberOfIterations) {
      #2 uniformly distributed RV are generated to match the interest area   
      x <- runif(1, min = startRange, max = endRange)
      y <- runif(1, min = 0, max = dnorm(mean, mean = mean, sd = sqrt(variance)))
      
      # pdf(x) > y -=> accept, reject otherwise
      if (y <= dnorm(x, mean = mean, sd = sqrt(variance))) {
        accepted_samples_x[accepted_count+1] <- x
        accepted_samples_y[accepted_count+1] <- y          
        accepted_count <- accepted_count + 1
      }else{
        rejected_samples_x[rejected_count+1] <- x
        rejected_samples_y[rejected_count+1] <- y
        rejected_count <- rejected_count + 1
      }
      count = count + 1
    }
      
    print(accepted_samples_x)
    #plot the samples
    if (input$typeOfGraph == 1) {
      points(accepted_samples_x, accepted_samples_y, pch = 19, col = "#F9E153")
      points(rejected_samples_x, rejected_samples_y, pch = 3, col = "#a61b1b")
    }
    else {
      # plotting the histogram of the generated sample
      par(bg = "#f2f2f2")
      hist(accepted_samples_x, main = "Rejection Sampling", col = "#b5b5b5", border = "#b5b5b5", xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
      # generating the sequence of values in the specified
      # range based on which to generate and plot the PDF 
      x = seq(startRange, endRange, 0.01)
      # plotting PDF over the histogram
      if (input$showPDFRejection)
        curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "#F9E153", add = TRUE)
    }
  })

}


shinyApp(ui = ui, server = server)