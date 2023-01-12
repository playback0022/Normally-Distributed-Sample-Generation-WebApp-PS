  numberOfIterations = 100000
  mean = 55
  variance = 1000
  startRange = -100
  endRange = 100
  numberOfBreaks = 50 
  
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
  
  maximumOfPDF = 1 / sqrt(2 * pi * variance) 
  
  while (count < numberOfIterations) {
    #2 uniformly distributed RV are generated to match the interest area   
    x <- runif(1, min = startRange, max = endRange)
    y <- runif(1, min = 0, max = maximumOfPDF)
    
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
    # plotting the histogram of the generated sample
    par(bg = "#f2f2f2")
    hist(accepted_samples_x, main = "Rejection Sampling", col = "#b5b5b5", border = "#b5b5b5", xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
    # generating the sequence of values in the specified
    # range based on which to generate and plot the PDF 
    x = seq(startRange, endRange, 0.01)
    # plotting PDF over the histogram
      
      curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "#F9E153", add = TRUE)