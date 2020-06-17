Q20200629_1w <- function(QuizFile, Counters, NumberOfVariations) {

  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 3
    #
    # generate data with insignificant Levene test
    #
    Sample <- Q20200629_1nw_core(QuizeFile, Counters, NumberOfVariations)
    dAlpha <- sample(c(0.01,0.05,0.10), 1)
    iN <- nrow(Sample)
    dMean <- mean(Sample$Sample1)
    dSd <- sd(Sample$Sample1)
    iNullValue <- round(dMean + sample(c(-1,1),1) * dSd * sample(1:4, 1) / 2 / sqrt(iN))
    iDirection <- round(1.5 + sign(dMean - iNullValue)/2 )
    sNullIneq <- c("\\ge", "\\le")[iDirection]
    sAltIneq <- c("<", ">")[iDirection]
    #
    # formulate the question
    #
    tbSummary <- describe(Sample, fast = TRUE)[c("n","mean","sd")]
    QuestionText <- c(
      sprintf("For a quality control study, you measure the temperature of an item right after 
      production in two different facilities. Summary statistics are in the table below. 
      You want to construct a %d%% confidence interval for $\\mu_1$. Compute the
      UPPER bound of this confidence interval using %d decimal places.", 
              100 - 100*dAlpha, iDigits),
      DigitDecimalWarning(iDigits),
      sprintf("<pre>%s</pre>", myprettytableprint(tbSummary, floatcolumn = c(0,1,1)))
    )
    #
    # compute the answer and error margin
    #
    dError <- 0.002 * c(0, -1, 1)
    Answers <- matrix(
      dMean + dSd * (qt(1 - dAlpha/2, iN-1) + dError) / sqrt(iN) + dError,
      nrow = 1)
    #
    # pose the question
    #
    Counters <- NUMquestion(QuizFile, "(testing)", QuestionText,
                            Answers, Counters, texify = TRUE)
  }
  
  return(Counters)
}
