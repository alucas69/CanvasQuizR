Q20200629_1a <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate random mu, sigma2, and quantile
    #
    dAlpha = sample(c(0.01, 0.05, 0.10), 1)
    dMu <- sample(c(-1,1), 1) * sample(4:24, 1) * 10
    dSigma <- sample(1:floor(abs(dMu) / 3), 1)
    dQuantile <- sample(c(-1,1),1) * runif(1, min = 0.5, max = 2)
    dQuantile <- round( dMu + dQuantile * dSigma, digits = 0)
    iDirection <- sample(c(-1,1), 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf(
        "Let $X$ be a normal random variable $N(\\mu = %d, \\sigma^2 = %d)$. Compute
        the probability $P(X %s %d)$ in %d decimal positions.",
        dMu, dSigma^2, c("\\le", "\\ge")[(iDirection == 1) + 1], dQuantile, iDigits
      ),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    dZ <- (dQuantile - dMu) / dSigma
    dZerror <- 0.018
    Answers <- matrix(pnorm( dZ + dZerror * c(0, -1, 1), lower.tail = (iDirection == -1) ),
                      1, 3)
    for (i1 in 1:nrow(Answers)) Answers[i1, 2:3] <- sort(Answers[i1, 2:3])
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(prob. calculation)", QuestionText,
                           Answers, Counters, texify=TRUE)
  }
  
  return(Counters)
}

