Q20200605_1y <- function(QuizFile, Counters, NumberOfVariations) {

  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 4
    #
    # generate data with insignificant Levene test
    #
    dFreq <- sample(1:5, 1)
    dAlpha <- sample(1:5, 1)
    dBeta <- sample(1:5, 1)
    #
    # formulate the question
    #
    QuestionText <- c(
      sprintf("A new test has been developed to detect a disease. It is known that 
      %d%% of the population has the disease. If a person has the disease, the test
      will be positive (detect that the person has the disease) with probability 
      %d%%. Conversely, if a person is healthy, the test will be falsely positive 
      with a probability of %d%% (and so will indicate in %d%% of those cases that 
      the tested person indeed does not have the disease).", dFreq, 100-dAlpha, 
              dBeta, 100-dBeta),
      sprintf("A randomly selected person tests positive (i.e., the test indicates
      this person has the disease). Compute the probability that this really has
      the disease using %d decimal places.", iDigits),
      DigitDecimalWarning(iDigits)
    )
    #
    # compute the answer and error margin
    #
    dError <- 0.0002 * c(0, -1, 1)
    Answers <- matrix(
      (dFreq/100 * (1 - dAlpha/100)) / 
      (dFreq/100 * (1 - dAlpha/100) + (1 - dFreq/100) * dBeta/100) + dError,
      nrow = 1)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(probability)", QuestionText,
                            Answers, Counters, texify = TRUE)
  }
  
  return(Counters)
}
