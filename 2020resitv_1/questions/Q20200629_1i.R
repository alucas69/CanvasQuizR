Q20200629_1i <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 3
    #
    # generate random mu, sigma2, and quantile
    #
    iN <- sample(5:25, 1)
    dMu <- sample(100:260, 1)
    dSigma <- sample(6:50, 1) / 2
    dQuantile <- sample( c(1, 5, 10, 20), 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf("In a sample of size %d from a normally $N(\\mu, \\sigma^2)$ distributed 
      population, we find a sample mean of %d.0 and a sample standard deviation of 
      %3.1f. Give the *LOWER* bound of the %d%% confidence interval for $\\mu$ in %d decimals.",
      iN, dMu, dSigma, 100 - dQuantile, iDigits),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    dZ <- qt(dQuantile/200, iN - 1)
    dZerror <- 0.001
    Answers <- matrix(dMu + dSigma * (dZ + dZerror * c(0, -1, 1)) / sqrt(iN), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(conf. interval)", QuestionText, 
                            Answers, Counters, texify = TRUE)
  }
  
  return(Counters)
}