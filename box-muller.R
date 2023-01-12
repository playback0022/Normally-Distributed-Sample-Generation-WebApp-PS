boxMuller <- function (sampleSize, mean, variance, startRange, endRange, numberOfBreaks) {
  # making sure provided parameters are valid
  if (sampleSize <= 0 | variance <= 0 | numberOfBreaks <= 0) {
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(startRange, endRange), ylim = c(0,1))
    # generating the sequence of values in the specified
    # range based on which to generate and plot the PDF 
    x = seq(startRange, endRange, 0.01)
    # plotting PDF over the histogram
    curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 2, col = "red", add = TRUE)
    return()
  }
  
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
  
  # keep generating pairs of normally-distributed random variables
  # for as long as the sample size is smaller than the desired one
  while (length(generatedSample) < sampleSize) {
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
  
  # the extra generated element (if any) should be removed
  if (length(generatedSample) != sampleSize)
    generatedSample = generatedSample[-length(generatedSample)]
  
  # plotting the histogram of the generated sample
  line = par(lwd = 2)
  hist(generatedSample, main = "Box-Muller Sampling", col = "gray80", border = "gray40", lwd = 2, xlab = "Generated Sample", probability = TRUE, breaks = numberOfBreaks)
  # generating the sequence of values in the specified
  # range based on which to generate and plot the PDF 
  x = seq(startRange, endRange, 0.01)
  # plotting PDF over the histogram
  curve(dnorm(x, mean = mean, sd = sqrt(variance)), lwd = 3, col = "indianred3", add = TRUE)
}

