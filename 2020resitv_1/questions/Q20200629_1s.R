Q20200629_1s <- function(QuizFile, Counters, NumberOfVariations) {

  for (i1 in 1:NumberOfVariations) {
    #
    # though asking # decimals, we are more generous ...
    #
    iDigits <- 3
    #
    # generate data with insignificant Levene test
    #
    dLevenePval <- 0
    while (dLevenePval < 0.10) {
      Sample <- Q20200629_1nw_core(QuizeFile, Counters, NumberOfVariations)
      y <- c(Sample$Sample1, Sample$Sample2)
      Smpl <- y
      Smpl[1:nrow(Sample)] <- "Sample1"
      Smpl[(nrow(Sample)+1):(2*nrow(Sample))] <- "Sample2"
      dfLevene <- data.frame(y, Smpl)
      tbLevene <- leveneTest(y ~ Smpl, dfLevene)
      dLevenePval <- tbLevene$`Pr(>F)`[1]
    }
    dAlpha <- sample(c(0.01,0.05,0.10), 1)
    iN <- nrow(Sample)
    dDiff <- mean(Sample$Sample1) - mean(Sample$Sample2)
    iDirection <- sign(dDiff)
    if (iDirection == 0) iDirection <- -1
    sDirection <- c("less than", "greater than")[round(1.5 + iDirection/2)]
    #
    # formulate the question
    #
    tbTtest <- t.test(Sample$Sample1, Sample$Sample2, var.equal = TRUE, 
                      alternative = c("less","two.sided","greater")[2+iDirection])
    tbSummary <- describe(Sample, fast = TRUE)[c("n","mean","sd")]
    QuestionText <- c(
      sprintf("For a quality control study, you measure the temperature of an item right after 
      production in two different facilities. You want to 'prove' that $\\mu_1$ is 
      %s $\\mu_2$. Give the appropriate NULL hypothesis.", sDirection)
    )
    #
    # compute the answer and error margin
    #
    # with equal variances
    Answers <- matrix(c(
      0, "$H_0: \\mu_1 = \\mu_2$",
      -1, "$H_0: \\mu_1 \\ge \\mu_2$",
      1, "$H_0: \\mu_1 \\le \\mu_2$",
      0, "None of the above"),
      nrow = 2)[, c(sample(1:3, 3), 4)]
    Answers
    #
    # pose the question
    #
    Counters <- MCquestion(QuizFile, "(testing)", QuestionText,
                            Answers[2,], (as.numeric(Answers[1,]) == iDirection), Counters, texify = TRUE)
  }
  
  return(Counters)
}
