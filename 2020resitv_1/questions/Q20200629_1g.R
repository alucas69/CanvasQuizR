Q20200629_1g <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate random mu, sigma2, and quantile
    #
    iN <- sample(c(5:7), 1)
    dPi <- sample(c(0.05, 0.15, (1:7)/10), 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf(
        "You sell high end luxury goods. The probability that you close at least one 
        sale is $\\pi = %d%%$ per day. Assume that sales are independent across days
        and that you are open %d days a week. Compute the probability of having no 
        sales in a particular week in %d decimal places.",
        round(dPi*100, digits = 0), iN, iDigits
      ),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    Answers <- matrix((1-dPi)^iN + 0.0002 * c(0, -1, 1), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(prob. calculation)", QuestionText,
                           Answers, Counters, texify=TRUE)
  }
  
  return(Counters)
}

