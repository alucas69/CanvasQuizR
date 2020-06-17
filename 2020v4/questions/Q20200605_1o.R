Q20200605_1o <- function(QuizFile, Counters, NumberOfVariations) {

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
      Sample <- Q20200605_1nw_core(QuizeFile, Counters, NumberOfVariations)
      y <- c(Sample$Sample1, Sample$Sample2)
      Smpl <- y
      Smpl[1:nrow(Sample)] <- "Sample1"
      Smpl[(nrow(Sample)+1):(2*nrow(Sample))] <- "Sample2"
      dfLevene <- data.frame(y, Smpl)
      tbLevene <- leveneTest(y ~ Smpl, dfLevene)
      dLevenePval <- tbLevene$`Pr(>F)`[1]
    }
    iDirection <- sample(c(0, sign(mean(Sample$Sample1) - mean(Sample$Sample2))), 1)
    dAlpha <- sample(c(0.01,0.05,0.10), 1)
    #
    # formulate the question
    #
    tbTtest <- t.test(Sample$Sample1, Sample$Sample2, var.equal = TRUE, 
                      alternative = c("less","two.sided","greater")[2+iDirection])
    tbSummary <- describe(Sample, fast = TRUE)[c("n","mean","sd")]
    QuestionText <- c(
      sprintf("For a quality control study, you measure the temperature of an item right after 
      production in two different facilities. You are interested in testing whether 
      the *standard deviations* of temperatures $\\sigma_1$ at facility 1 
      (sample 1) and $\\sigma_2$ at facility 2 (sample 2) are significantly different.
      What is your conclusion at $\\alpha = %4.2f$ and why (provide *ALL* correct answers)? 
      You can use the following tables.",
        dAlpha),
      sprintf("<pre>%s</pre>", myprettytableprint(tbSummary, floatcolumn = c(0,1,1))),
      sprintf("<pre>%s</pre>", myprettyleveneprint(tbLevene)),
      sprintf("<pre>%s</pre>", myprettyttestprint(tbTtest, withalternative = FALSE))
    )
    #
    # compute the answer and error margin
    #
    Answers <- matrix(c(
      0, sprintf("The standard deviations differ significantly, because $\\sigma_1 %s \\sigma_2$", c("<", ">")[(sd(Sample$Sample1) > sd(Sample$Sample2)) + 1]),
      0, sprintf("The standard deviations differ significantly, because $s_1 %s s_2$", c("<", ">")[(sd(Sample$Sample1) > sd(Sample$Sample2)) + 1]),
      1, "The standard deviations differ significantly, because the Levene test is significant",
      0, "The standard deviations do *not* differ significantly, because the Levene test is significant",
      0, "The standard deviations differ significantly, because the Levene test is *not* significant",
      2, "The standard deviations do *not* differ significantly, because the Levene test is *not* significant",
      0, "The standard deviations differ significantly, because the t test is significant",
      0, "The standard deviations do *not* differ significantly, because the t test is significant",
      0, "The standard deviations differ significantly, because the t test is *not* significant",
      0, "The standard deviations do *not* differ significantly, because the t test is *not* significant"
    ), nrow = 2)
    Answers <- Answers[, c(1:2, sample(3:6, 2), sample(7:10, 2))]
    Answers[1,] <- 0 + (as.numeric(Answers[1,]) == (1 + (tbLevene$`Pr(>F)`[1] > dAlpha)))
    Answers <- cbind(Answers, c(0, "None of the above"))
    if (min(as.numeric(Answers[1,])) == 0) Answers[1,ncol(Answers)] <- 1
    #
    # pose the question
    #
    Counters <- MAquestion(QuizFile, "(testing)", QuestionText,
                            Answers[2,], (as.numeric(Answers[1,]) > 0), Counters, texify = TRUE)
  }
  
  return(Counters)
}
