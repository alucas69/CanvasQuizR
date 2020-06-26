Q20200629_1n <- function() {
  
  # generate data with insignificant Levene test
  iDigits <- 3
  qtype= "mc"
  dLevenePval <- 0
  while (dLevenePval < 0.10) {
    Sample <- Q20200605_1nw_core()
    y <- c(Sample$Sample1, Sample$Sample2)
    Smpl <- y
    Smpl[1:nrow(Sample)] <- "Sample1"
    Smpl[(nrow(Sample)+1):(2*nrow(Sample))] <- "Sample2"
    dfLevene <- data.frame(y, Smpl)
    tbLevene <- leveneTest(y ~ Smpl, dfLevene)
    dLevenePval <- tbLevene$`Pr(>F)`[1]
  }
  iDirection <- sample(c(0, sign(mean(Sample$Sample1) - mean(Sample$Sample2))), 1)
  if (iDirection == 0) {
    sDirection <- "the mean temperatures at facility 1 (sample 1) and facility 2 
      (sample 2) are significantly different"
  } else {
    sDirection <- sprintf("the mean temperature at facility 1 (sample 1) is 
                           significantly %s than at facility 2 (sample 2)",
                          c("lower","lower","higher")[iDirection+2])
  }
  dAlpha <- sample(c(0.01,0.05,0.10), 1)
  
  # formulate the question
  tbTtest <- t.test(Sample$Sample1, Sample$Sample2, var.equal = TRUE, 
                    alternative = c("less","two.sided","greater")[2+iDirection])
  tbSummary <- describe(Sample, fast = TRUE)[c("n","mean","sd")]
  QuestionText <- c(
    tex2math(sprintf("For a quality control study, you measure the temperature of an item right after 
      production in two different facilities. You are interested in testing whether 
      %s. What is your conclusion at $\\alpha = %4.2f$ and why? You can use the following tables.",
                     sDirection, dAlpha)),
    sprintf("<pre>%s</pre>", myprettytableprint(tbSummary, floatcolumn = c(0,1,1))),
    sprintf("<pre>%s</pre>", myprettyleveneprint(tbLevene)),
    sprintf("<pre>%s</pre>", myprettyttestprint(tbTtest, withalternative = FALSE))
  )
  #
  # compute the answer and error margin
  #
  Answers <- matrix(c(
    0, "Reject $H_0$ as the Levene test is not significant",
    0, "Do not reject $H_0$ as the Levene test is not significant",
    0, sprintf("Reject $H_0$ as $\\mu_1 - \\mu_2 %s 0$", c("<", ">")[(mean(Sample$Sample1) > mean(Sample$Sample2)) + 1]),
    0, sprintf("Do not reject $H_0$ as $\\mu_1 - \\mu_2 %s 0$", c("<", ">")[(mean(Sample$Sample1) > mean(Sample$Sample2)) + 1]),
    1, sprintf("Reject $H_0$ as the test t-statistic %1.3f is in the critical region", tbTtest$statistic),
    0, sprintf("Do not reject $H_0$ as the test t-statistic %1.3f is in the critical region", tbTtest$statistic),
    0, sprintf("Reject $H_0$ as the test t-statistic %1.3f is *not* in the critical region", tbTtest$statistic),
    2, sprintf("Do not reject $H_0$ as the test t-statistic %1.3f is *not* in the critical region", tbTtest$statistic)
  ), nrow = 2)
  Answers[2,]= tex2math(Answers[2,])
  Ai <- (as.numeric(Answers[1,]) == (tbTtest$p.value > dAlpha) + 1)
  
  return(list(type=qtype, text=QuestionText, answer=Answers[2,], correct=Ai))    
}
