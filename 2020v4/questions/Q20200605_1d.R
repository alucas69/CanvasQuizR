Q20200605_1d <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate random mu, sigma2, and quantile
    #
    iN <- sample(c(6:10, 12, 14, 16), 1)
    iQ <- sample(c(2,3,3,4), 1)
    iDirection <- sample(c(-1,1), 1)
    dPi <- sample(c(0.1, 0.2, 0.3, 0.4, 0.5), 1)
    iSharp <- c(0, sample(c(0,1), 1), 1)[iQ-1] # make sure <= for small Q, < for large Q
    dP <- pbinom(iQ - iSharp, size = iN, prob = dPi)
    dPerror <- 0.00016 * iQ
    sIneq <- matrix(c("\\le", "<", "\\ge", ">"), nrow = 2, byrow = TRUE)[1 + (iDirection == 1), iSharp + 1]
    if (iDirection == 1) {
      iQ <- iN - iQ
      dPi <- 1 - dPi
    }
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf(
        "Let $Y$ be a binomial random variable with a $Binomial(n = %d, \\pi = %3.1f)$
        distribution. Compute the probability $P(Y %s %d)$ in %d decimal positions.",
        iN, dPi, sIneq, iQ, iDigits
      ),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    Answers <- matrix(dP + dPerror * c(0, -1, 1), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(prob. calculation)", QuestionText,
                           Answers, Counters, texify=TRUE)
  }
  
  return(Counters)
}

