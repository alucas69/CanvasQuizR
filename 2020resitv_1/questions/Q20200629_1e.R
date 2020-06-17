Q20200629_1e <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate random mu, sigma2, and quantile
    #
    dPi <- sample(c(2:8), 1) / 10
    iSampleSize <- ceiling(10 / dPi) + sample(0:30, 1)
    iSampleSize <- 100
    iN <- sample(c(6:10, 12, 14, 16), 1)
    dQ <- sample(c(-1,1), 1) * runif(1, min = 0.3, max = 1.8)
    dQ <- round(round( iSampleSize * (dPi*iN + sqrt(dPi*(1-dPi)*iN/iSampleSize)), 
                       digits = 0) / iSampleSize, digits = 2)
    iDirection <- sample(c(-1,1), 1)
    sIneq <- c("\\le", "\\ge")[1 + (iDirection == 1)]
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf(
        "Let $Y_1, Y_2,..., Y_%d$ be a set of independent and identically distributed
        $Binomial(n = %d, \\pi = %3.1f)$ random variables, and let $\\overline Y$ be
        the average of all these variables. Compute the probability 
        $P(\\overline Y %s %3.2f)$ in %d decimal positions.",
        iSampleSize, iN, dPi, sIneq, dQ, iDigits
      ),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    # Answer using normal approximation
    dZerror <- 0.018
    Answers <- matrix( pnorm((dQ - iN*dPi) / sqrt(iN*dPi*(1-dPi)/iSampleSize) + 
                         dZerror * c(0, -1, 1), lower.tail = (iDirection == -1)), 1, 3)
    # Answer using binomial
    Answers <- rbind(Answers,
                     pbinom(
                       round(dQ*iSampleSize - (iDirection == 1), digits = 0) + c(0, -1, 1),
                       size = iSampleSize*iN, prob = dPi, lower.tail = (iDirection == -1)
                     ))
    for (i1 in 1:nrow(Answers)) Answers[i1, 2:3] <- sort(Answers[i1, 2:3])
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(prob. calculation)", QuestionText,
                           Answers, Counters, texify=TRUE)
  }
  
  return(Counters)
}

