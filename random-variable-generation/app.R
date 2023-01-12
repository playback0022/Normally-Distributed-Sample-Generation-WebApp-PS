library("shiny")
library("bslib")


ui = fluidPage(
  # bootstrap theme
  theme = bs_theme(bg = "#232324", fg = "white", primary = "#F9E153"),
  
  h4(strong("Normally Distributed Sample Generation Engine"), align = "center"),
  
  tabsetPanel(br(),
             tabPanel(title = "Box-Muller",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "sampleSizeBoxMuller", label = "Sample Size", min = 500, max = 500000, value = 10000, step =  1),
                          sliderInput(inputId = "sampleRangeBoxMuller", label = "Sample Range", min = -10000, max = 10000, value = c(-100, 100), step = 1),
                          sliderInput(inputId = "numberOfBarsBoxMuller", label = "Number of Histogram Bars", min = 50, max = 5000, value = 500, step =  1),
                          numericInput(inputId = "desiredMeanBoxMuller", label = "Desired Mean", value = 0, step = 0.01),
                          numericInput(inputId = "desiredVarianceBoxMuller", label = "Desired Variance", min = 0, value = 1, step = 0.01),
                          checkboxInput(inputId = "showPDFBoxMuller", label = "Show PDF", value = FALSE),
                          p("The Box-Muller method makes use of two randomly generated uniformly distributed numbers to generate a pair of numbers with a standard normal distribution (mean = 0, variance = 1)."),
                          p("The generated sample can then be transformed into another normally distributed sample with the specified mean and variance.")
                        ),
                        mainPanel()
                      )),
             
             tabPanel(title = "Rejection",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "sampleSizeRejection", label = "Sample Size", min = 500, max = 500000, value = 10000, step =  1),
                          sliderInput(inputId = "sampleRangeRejection", label = "Sample Range", min = -10000, max = 10000, value = c(-100, 100), step = 1),
                          sliderInput(inputId = "numberOfBarsRejection", label = "Number of Histogram Bars", min = 50, max = 5000, value = 500, step =  1),
                          numericInput(inputId = "desiredMeanRejection", label = "Desired Mean", value = 0, step = 0.01),
                          numericInput(inputId = "desiredVarianceRejection", label = "Desired Variance", min = 0, value = 1, step = 0.01),
                          checkboxInput(inputId = "showPDFRejection", label = "Show PDF", value = FALSE),
                          p("Rejection Sampling makes use of two independent uniformly distributed random variables to generate an element in the specified range and a number in the range of possible values taken by the desired normal distribution's PDF."),
                          p("When the (y-axis) generated value of the latter uniform variable is greater than the value of the PDF at the former uniform number, said number is rejected. Else, it is accepted and admitted into the sample."),
                          p("Intuitively, at the points at which the PDF is greater, more uniform numbers will be accepted, since their pair y-axis uniform values have a larger 'acceptance' range.")
                        ),
                        mainPanel()
                      )),
             br()
             ),
  
  tags$div(h6("This WebApp was developed as a project for the ", strong("Probabilities and Statistics"), " course of the ", strong("University of Bucharest.")),
  h6("Project Contributors: ", strong("playback0022 (Toma), IRadu15 (Radu)")), style = "background-color:#303030; padding: 0.5rem; margins: 0; text-align: center;")
)


server = function(input, output) {
  
}


shinyApp(ui = ui, server = server)