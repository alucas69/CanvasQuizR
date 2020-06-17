Q20200605_1h <- function(QuizFile, Counters, NumberOfVariations) {
  
  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate random mu, sigma2, and quantile
    #
    iN <- sample(3:10, 1)
    dPi <- sample(c(1, 5, 10, 15, 20), 1) 
    #
    # formulate the question
    #
    QuestionText <- c(sprintf(
      "A null hypothesis $H_0$ is tested %d times in tests based on independent 
      samples. Per test, $H_0$ is rejected with probability %d%% when in fact it 
      is true. The researcher decides to reject $H_0$ if it is rejected at least once 
      in these %d tests. Compute in %d decimal places the probability that in this 
      procedure $H_0$ is rejected when in fact it is true.",
      iN, dPi, iN, iDigits), 
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    Answers <- round( 1 - (1 - dPi/100)^iN, digits = iDigits)
    Answers <- matrix(Answers + 0.0002 * c(0, -1, 1), 1, 3)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(probability calculation)", QuestionText, 
                            Answers, Counters, texify = TRUE)
  }
  
  return(Counters)
}