Q20200629_1q <- function() {
  
  # generate data with insignificant Levene test
  iDigits <- 3
  qtype= "num"
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
  dAlpha <- sample(c(0.01,0.05,0.10), 1)
  iN <- nrow(Sample)
  dDiff <- mean(Sample$Sample1) - mean(Sample$Sample2)
  dSdDiff1 <- sqrt((iN - 1) * (sd(Sample$Sample1)^2 + sd(Sample$Sample2)^2) / 
                     (2*iN - 2) * (2 / iN))
  dDiffNull <- round(dDiff + sample(c(-1,1), 1) * sample(3:6,1) * 0.5 * dSdDiff1, 
                     digits = 1)
  
  # formulate the question
  iDirection <- sample(c(0, sign(mean(Sample$Sample1) - mean(Sample$Sample2))), 1)
  tbTtest <- t.test(Sample$Sample1, Sample$Sample2, var.equal = TRUE, 
                    alternative = c("less","two.sided","greater")[2+iDirection])
  tbSummary <- describe(Sample, fast = TRUE)[c("n","mean","sd")]
  QuestionText <- c(
    tex2math(sprintf("For a quality control study, you measure the temperature of an item right after 
      production in two different facilities. You want to test 
      $H_0: \\mu_1 - \\mu_2 = %1.1f$ at $\\alpha = %4.2f$. Compute the CRITICAL VALUE 
      of the usual (standardized) test statistic to test this hypothesis, using %d decimal places.",  
                     dDiffNull, dAlpha, iDigits)),
    DigitDecimalWarning(iDigits),
    sprintf("<pre>%s</pre>", myprettytableprint(tbSummary, floatcolumn = c(0,1,1))),
    sprintf("<pre>%s</pre>", myprettyleveneprint(tbLevene)),
    sprintf("<pre>%s</pre>", myprettyttestprint(tbTtest, withalternative = FALSE))
  )
  
  # compute the answer and error margin
  # with equal variances
  dError <- 0.002
  Answers <- qt(1-dAlpha/2, 2*iN - 2) + dError * matrix(c(0,-1,1), nrow = 1)
  # with unequal variances
  dS12 <- sd(Sample$Sample1)^2
  dS22 <- sd(Sample$Sample2)^2
  dfW <- (dS12/iN + dS22/iN)^2 / ( (dS12/iN)^2/(iN-1) + (dS22/iN)^2/(iN-1))
  Answers <- rbind(Answers, 
                   matrix(c(
                     qt(1 - dAlpha/2, dfW),
                     sort(c(qt(1 - dAlpha/2, c(ceiling(dfW), floor(dfW)))))
                   ) + dError * c(0,-1,1), nrow = 1)
  )
  
  return(list(type=qtype, text=QuestionText, answer=Answers))    
}
