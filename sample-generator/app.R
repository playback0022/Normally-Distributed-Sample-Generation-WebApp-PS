library("shiny")
library("bslib")


ui <- fluidPage(
  # bootstrap theme
  theme = bs_theme(bg = "#232324", fg = "white", primary = "#F9E153"),
  
  h4(strong("Normally Distributed Sample Generation Engine"), align = "center"),
  
  br(),
  tabsetPanel(
             tabPanel(title = "Box-Muller",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "numberOfIterationsBoxMuller", label = "Number of Iterations", min = 10000, max = 500000, value = 100000, step =  1),
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
                          sliderInput(inputId = "numberOfIterationsRejection", label = "Number of Iterations", min = 10000, max = 500000, value = 100000, step =  1),
                          sliderInput(inputId = "sampleRangeRejection", label = "Sample Range", min = -500, max = 500, value = c(-5, 5), step = 1),
                          sliderInput(inputId = "numberOfBarsRejection", label = "Number of Histogram Bars", min = 50, max = 1000, value = 500, step =  1),
                          numericInput(inputId = "desiredMeanRejection", label = "Desired Mean", value = 0, step = 0.1),
                          numericInput(inputId = "desiredVarianceRejection", label = "Desired Variance", min = 0, value = 1, step = 0.1),
                          radioButtons(inputId = "typeOfGraph", label = "Type of Graph", choices = c("Histogram" = 0, "Accepted Point Visualization" = 1)),
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
  
  output$BoxMuller <- renderPlot ({
    # set the plot background color
    par(bg = "#f2f2f2")
    
    numberOfIterations <- input$numberOfIterationsBoxMuller
    mean <- input$desiredMeanBoxMuller
    variance <- input$desiredVarianceBoxMuller
    startRange <- input$sampleRangeBoxMuller[1]
    endRange <- input$sampleRangeBoxMuller[2]
    numberOfBreaks <- input$numberOfBarsBoxMuller 
    
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
    
    # vector storing the generated samples;
    # at most 'numberOfIterations' pairs of samples can be generated;
    # allocating a large size to the vector from the very 
    # beginning is much faster then appending elements
    generatedSample <- numeric(2 * numberOfIterations)
    count <- 0
    
    for (i in seq(1, numberOfIterations, 1)) {
      sample <- generateNormallyDistributedPair()
      # transform generated sample (normal distribution with mean = 0, 
      # variance = 1) to the normal distribution with the specified parameters;
      # multiplying by the standard deviation and adding mean of the 
      # desired distribution
      sample <- sqrt(variance) * sample + mean
      # filter out the elements outside the specified range of values
      sample <- sample[sample >= startRange & sample <= endRange]
      
      # there is no certainty about the accepted sample size
      # (might be 0, 1, 2), so we iterate through the sample
      for (number in sample) {
        count <- count + 1
        generatedSample[count] = number
      }
    }
    
    # the histogram and curve are shown only when a 
    # non-null number of samples has been generated
    if (count) {
      # plotting the histogram of the generated sample
      hist(generatedSample[1:count], main = "Box-Muller Sampling", col = "#b5b5b5", border = "#b5b5b5", xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
      # generating the sequence of values in the specified
      # range based on which to generate and plot the PDF 
      x <- seq(startRange, endRange, 0.01)
      # plotting PDF over the histogram
      if (input$showPDFBoxMuller)
        curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "#F9E153", add = TRUE)
    }
  })
  
  output$Rejection <- renderPlot({
    numberOfIterations <- input$numberOfIterationsRejection
    mean <- input$desiredMeanRejection
    variance <- input$desiredVarianceRejection
    startRange <- input$sampleRangeRejection[1]
    endRange <- input$sampleRangeRejection[2]
    numberOfBreaks <- input$numberOfBarsRejection 
    
    # plotting normal PDF with the desired parameters 
    x <- seq(startRange, endRange, length.out = 500)
    y <- dnorm(x, mean = mean, sd = sqrt(variance))
    plot(x, y, type = "l", xlab = "X", ylab = "Y", main = "Rejection Sampling")
    maxValuePDf <- dnorm(mean, mean = mean, sd = sqrt(variance))
    
    # generating normally-distributed sample;
    # both the accepted and rejected points will be stored,
    # in order to plot the acceptance and rejection regions
    # and illustrate that the accepted samples fall within
    # the PDF boundaries;
    # as previously stated, allocating a large size to the vector
    # from the very beginning is much faster then appending elements
    accepted_samples_x <- numeric(numberOfIterations)
    accepted_samples_y <- numeric(numberOfIterations)
    rejected_samples_x <- numeric(numberOfIterations)
    rejected_samples_y <- numeric(numberOfIterations)
    
    accepted_count <- 0
    rejected_count <- 0
    
    for (i in seq(1, numberOfIterations, 1)) {
      # elements in the sample range
      x <- runif(1, min = startRange, max = endRange)
      # numbers describing x's probability
      y <- runif(1, min = 0, max = maxValuePDf)
      
      # as specified, only the points falling under the PDF are accepted
      if (y <= dnorm(x, mean = mean, sd = sqrt(variance))) {
        accepted_count <- accepted_count + 1
        accepted_samples_x[accepted_count] <- x
        accepted_samples_y[accepted_count] <- y          
      }
      else{
        rejected_count <- rejected_count + 1
        rejected_samples_x[rejected_count] <- x
        rejected_samples_y[rejected_count] <- y
      }
    }
      
    # accepted rate visualization
    if (input$typeOfGraph == 1) {
      points(accepted_samples_x, accepted_samples_y, pch = 19, col = "#F9E153")
      points(rejected_samples_x, rejected_samples_y, pch = 3, col = "#b5b5b5")
    }
    # histogram
    else {
      # plotting the histogram of the generated sample
      par(bg = "#f2f2f2")
      hist(accepted_samples_x[1:accepted_count], main = "Rejection Sampling", col = "#b5b5b5", border = "#b5b5b5", xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
      # generating the sequence of values in the specified
      # range based on which to generate and plot the PDF 
      x <- seq(startRange, endRange, 0.01)
      # plotting PDF over the histogram
      if (input$showPDFRejection)
        curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "#F9E153", add = TRUE)
    }
  })

}


shinyApp(ui = ui, server = server)