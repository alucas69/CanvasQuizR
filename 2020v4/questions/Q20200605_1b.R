Q20200605_1b <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 2
    #
    # generate random mu, sigma2, and quantile
    #
    dAlpha <- sample(c(1:4, 6:9), 1) / 10
    dMu <- sample(c(-1,1), 1) * sample(4:24, 1) * 10
    dSigma <- sample(1:floor(abs(dMu) / 3), 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf(
        "Let $X$ be a normal random variable $N(\\mu = %d, \\sigma^2 = %d)$. Compute
        the %d-th percentile of the distribution of $X$ in %d decimal positions.",
        dMu, dSigma^2, 100 * dAlpha, iDigits
      ),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    dZ <- qnorm(dAlpha)
    dZerror <- 0.015
    Answers <- matrix(dMu + dSigma * ( dZ + dZerror * c(0, -1, 1)), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(percentile calculation)", QuestionText,
                           Answers, Counters, texify=TRUE)
  }
  
  return(Counters)
}

