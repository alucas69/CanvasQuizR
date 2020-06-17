Q20200605_1j <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 3
    #
    # generate random mu, sigma2, and quantile
    #
    iN <- sample(5:35, 1)
    dMu <- sample(100:260, 1)
    dSigma <- sample(6:50, 1) / 2
    dQuantile <- sample( c(1, 5, 10, 20), 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf("In a sample of size %d from a normally $N(\\mu, \\sigma^2)$ distributed 
      population, we find a sample mean of %d.0 and a sample standard deviation of 
      %3.1f. Give the lower bound of the %d%% confidence interval for $\\sigma^2$ in %d decimals.",
      iN, dMu, dSigma, 100 - dQuantile, iDigits),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    dZ <- round(qchisq(1 - dQuantile/200, iN - 1), digits = 2) + 0.005 * c(0,-1,1)
    dZerror <- 0.015
    Answers <- matrix((iN - 1) * dSigma * dSigma / (dZ + dZerror * c(0,1,-1)), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(conf. interval)", QuestionText, 
                            Answers, Counters, texify = TRUE)
  }
  
  return(Counters)
}