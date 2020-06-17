############################################################################
############################################################################
############################################################################

DGP <- function(vFilenames) {

  # run command:
  # DGP(sprintf("D%s",stri_pad(1:5, 3, "0", side = "left")))
  
  # check existence of map
  sDir <- "../datafiles"
  if (!dir.exists(sprintf("%s", sDir))) dir.create(sprintf("%s", sDir))
  
  # write files to map
  for (name in vFilenames) {

    # sample size
    iN <- sample(50:80, 1)
    dMiss <- 0.05
    
    # continuous variables
    x1 <- rnorm(iN)
    x1[which(runif(iN) < dMiss)] <- NA
    x2 <- rnorm(iN)
    x2[which(runif(iN) < dMiss)] <- NA
    x3 <- rnorm(iN)
    x3[which(runif(iN) < dMiss)] <- NA
    x4 <- rnorm(iN)
    x4[which(runif(iN) < dMiss)] <- NA
    y1 <- 3*x2 + 5*x3 + rnorm(iN)
    y2 <- -2 * x1
    y3 <- rnorm(iN)
    
    # categorical variables
    z1 <- sample(c(1:4, NA), iN, replace = TRUE, prob = c(0.23, 0.23, 0.23, 0.23, 0.08))
    z2 <- sample(c(1:4, NA), iN, replace = TRUE, prob = c(0.23, 0.23, 0.23, 0.23, 0.08))
    z3 <- sample(c("short", "middle", "long", "extreme", NA), iN, replace = TRUE, prob = c(0.23, 0.23, 0.23, 0.23, 0.08))
    
    dfD <- data.frame(y1, y2, y3, x1, x2, x3, x4, z1, z2, z3)
    write.csv(dfD, file = sprintf("%s/%s", sDir, name))
  }
}


############################################################################
############################################################################
############################################################################


# Introductory text and question
Q_0a <- function() {
  
  sText <- c(
    "<b><big>Intro text  Business Statistics</big></b>",
    "<b><big>Digital exam 3 (12/06/2020 15:45-17:15, extra time students till 17:35)</big></b>",
    "&nbsp;",
    "<b>General</b>",
    "a) You have 90 minutes for this test (extra time students: 110 minutes)",
    "b) Datafiles can always be downloaded again in case you accidentally deleted or changed the data. <b>Make a photo of the link shown in the data download question later on.</b>",
    "c) You cannot go back to a previous question. So double-check before you press 'NEXT'.",
    "&nbsp;",
    "<b>Instructions</b>",
    "a) make this current quiz.",
    sprintf("b) Emergency questions on the exam can be asked by mailing to <a href=\"mailto:%s\">%s</a> using the subject \"RQUIZ\".", sEmailaddress, sEmailaddress),
    "b) For numerical answers, always use a decimal point, not a comma (so 2.31 and <b>not</b> 2,31). Your answer will be incorrect otherwise. Both your R command and your numerical answer should be correct to get full credits. Provide all digits and decimals provided by R.",
    "c) Advice: store all your commands in an R script file to quickly replicate your steps. Save the file repeatedly while progressing, just like the lab sessions.",
    "d) If you need more than one R command, type the answer in the quiz answer box by separating the commands by a semicolon (\";\") .",
    "&nbsp;",
    "<b><big>Before you start, choose the correct statement below.</big></b>"
  )
  
  sType <- "mc"
  sq <- "Introduction and statement"
  vAnswers <- c(
    "I made this exam <b>with</b> outside help during the time of the exam.",
    "I communicated with friends during the exam.",
    "I made this exam by myself <b>without</b> outside help for the entire duration of the exam."
  )
  vOrder <- sample(1:length(vAnswers), length(vAnswers))
  vCorrect <- c(FALSE, FALSE, TRUE)[vOrder]
  vAnswers <- vAnswers[vOrder]
  
  return (list(type=sType, q=sq, text=sText, answer=vAnswers, correct=vCorrect))
}


############################################################################
############################################################################
############################################################################


Q_0b <- function() {
  sType <- "mb"
  sq <- "Personal details"
  sText <- c("Please provide your personal details.",
    "First name (e.g. John):<br />[firstt]",
    "Family name (e.g. Merry):<br />[family]",
    "VU-NET ID (jmy983):<br />[vunet]",
    "Student number (2561834):<br />[stnumber]")
  vAnswerNames <- c("firstt", "family", "vunet", "stnumber")
  vAnswers <- rep(".", length(vAnswerNames))
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_0c <- function(sLinkname) {
  sType <- "mb"
  sq <- "Reading data"
  sText <- c(sprintf("Download your personal data file from <a href=\"%s\" target=\"_blank\">%s</a>. Right click, and \"save as\" in the appropriate location.<br /><b>Make a photo of this link, such that you can download the file again in case you corrupt this first file!!</b>", sLinkname, sLinkname),
             "Your data file contains the following 10 variables:",
             "<b style=\"font-family:'Courier New'; color:blue\">y1, y2, y3: continuous numerical variables<br />
             x1, x2, x3, x4: continuous numerical variables<br />
             z1, z2, z3: categorical variables</b>",
             "Note there can be missing values in your data set.",
             "Read your data file into R and provide the correct R command for this. (0 points)",
             "<b>R command for loading file:</b><br />[answer1]")
  vAnswerNames <- c("answer1")
  vAnswers <- c(sLinkname)
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_0ctest <- function(sLinkname) {
  sType <- "mb"
  sq <- "Reading data"
  sText <- c(sprintf("Download your personal test data file from <a href=\"%s\" target=\"_blank\">%s</a>. At the real exam, we will ask you to download a different data file. Answers obtained with a different datafile will be incorrect!", sLinkname, sLinkname),
             "Your test data file contains the following 2 variables:",
             "<b style=\"font-family:'Courier New'; color:blue\">Salary: continuous numerical variable<br />
             Nationality: categorical variable</b>",
             "Note there can be missing values in your data set.",
             "Read your data file into R and provide the correct R command for this. (0 points)",
             "<b>R command for loading file:</b><br />[answer1]")
  vAnswerNames <- c("answer1")
  vAnswers <- c(sLinkname)
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_1a <- function() {
  
  sVariable <- sample(c("y1", "y2", "y3", "x1", "x2", "x3", "x4"), 1)
  sStatistic <- matrix(c(
    "sum", "sum",
    "mean", "sample mean", 
    "sd", "sample standard deviation", 
    "var", "sample variance", 
    "median", "sample median", 
    "max", "maximum value", 
    "min", "minimum value"), nrow = 2)
  sStatistic <- sStatistic[ , sample(1:ncol(sStatistic), 1)]
  sStatistic[1] <- sprintf("%s(df$%s, na.rm = TRUE)", sStatistic[1], sVariable)
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("Consider the variable <b style=\"font-family:'Courier New'; color:blue\">%s</b> in your database. Provide the R command to compute the <b>%s</b> and provide the numerical answer (all decimals). (1 point)", sVariable, sStatistic[2]),
             "<b>R-command:</b> <br />[Rcomm]",
             "<b>Numerical value in all decimals:</b><br />[statistic]")
  vAnswerNames <- c("Rcomm", "statistic")
  vAnswers <- c(sStatistic[1], ".")
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_1b <- function() {
  
  sVariable <- sample(c("y1", "y2", "y3", "x1", "x2", "x3", "x4"), 1)
  sStatistic <- matrix(c(
    "sum", "sum",
    "mean", "sample mean", 
    "sd", "sample standard deviation", 
    "var", "sample variance", 
    "median", "sample median", 
    "max", "maximum value", 
    "min", "minimum value"), nrow = 2)
  sStatistic <- sStatistic[ , sample(1:ncol(sStatistic), 1)]
  sSign <- sample(c("positive","negative"), 1)
  sStatistic[1] <- sprintf("%s(df$%s[ which(df$%s %s 0) ])", sStatistic[1], sVariable, sVariable, c("&lt;","&gt;")[1 + (sSign == "positive")])
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("Consider the variable <b style=\"font-family:'Courier New'; color:blue\">x1</b> in your database. Provide the R command to compute the <b>%s</b> using only the <b>strictly %s</b> values of this variable, and provide the numerical answer (all decimals). (1 point)", sStatistic[2], sSign),
             "<b>R-command:</b><br />[Rcomm]",
             "<b>Numerical value in all decimals:</b><br />[statistic]")
  vAnswerNames <- c("Rcomm", "statistic")
  vAnswers <- c(sStatistic[1], ".")
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_1c <- function() {
  
  sType <- "mb"
  sq <- ""
  iZ1 <- sample(1:4, 1)
  iZ2 <- sample(1:4, 1)
  sPair <- sprintf("(z1=%d, z2=%d)", iZ1, iZ2)
  sText <- c(sprintf("The categorical variables %s and %s both can take 4 
  different values. In total, therefore, there are 16 possible values for the 
  pair %s. Count the number of cases for which both %s and %s are available 
  and %s. Provide the R command and the numerical answer. (1 point)<br />
  <b>R-command:</b><br />[Rcomm]<br />
	<b>Numerical value:</b><br />[z1z2]", 
  BlueCourier("z1"), BlueCourier("z2"), BlueCourier("(z1,z2)"), 
  BlueCourier("z1"), BlueCourier("z2"), BlueCourier(sPair)))
  vAnswerNames <- c("Rcomm", "z1z2")
  vAnswers <- c(sprintf("table(df$z1, dfd$z2)[%d,%d]", iZ1, iZ2), ".")
                  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}


############################################################################
############################################################################
############################################################################


Q_1d <- function() {
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("Make a plot of %s (vertical axis) against %s (horizontal
                     axis). The title of the plot should be \"y2 against x1\" and
                     the plot should have <b>no</b> x and y labels. What is the 
                     R command for making this plot?<br />
                     <b>R command:</b><br />[Plotcmd]", BlueCourier("y1"), 
                     BlueCourier("x1")))
  vAnswerNames <- c("Plotcmd")
  vAnswers <- c("plot(x1, y1, main = \"y2 against x1\", xlab = \"\", ylab = \"\")")
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2a <- function() {
  
  iSide <- sample(c(-1,0,1), 1)
  iAlpha <- sample(c(1,5,10), 1)
  iH0 <- sample(-10:10, 1)
  dAlternative <- sample(0:3, 1) / 2
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("Perform an %s-tailed test to see whether the mean of %s
    is significantly %s $%1.1f$ using $\\alpha = %d%%$.<br />
    Provide the R command and report the numerical value of the t-test statistic (all decimals). (1 point)", 
                     c("left", "two", "right")[iSide+2],
                     BlueCourier("x2"), c("lower than", "different from", "greater than")[iSide+2],
                     iH0/10, iAlpha),
             "<b>R command(s):</b><br />[Rcomm]",
             "<b>Numerical value (all decimals):</b><br />[statistic]"
             )
  vAnswerNames <- c("Rcomm", "statistic")
  vAnswers <- c(sprintf("t.text(df$x2, mu = %1.1f, conf.level = %1.2f, alternative = %s)", iH0/10, iAlpha/100, c("less","two.tailed","greater")[2+iSide]),
                sprintf("t.text(df$x2, mu = %1.1f, conf.level = %1.2f, alternative = %s)$statistic", iH0/10, iAlpha/100, c("less","two.tailed","greater")[2+iSide]))
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2b <- function() {
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You perform a regression of the variable %s on the 3 variables
    %s.<br />Report the R command(s) for doing this regression, and give the 
    numerical value of the <b>adjusted R-squared</b> of this regression (all
    decimals). (1 point)", BlueCourier("y1"), BlueCourier("x1, x2, x3")),
             "<b>R command:</b><br />[Rcomm]",
             "<b>Numerical value adjusted R-squared (all decimals):</b><br />[statistic]"
             )
  vAnswerNames <- c("Rcomm", "statistic")
  vAnswers <- c("summary(lm(df$y1 ~ df$x1 + df$x2 + df$x3))$adj.r.squared", ".")
                  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2c <- function() {
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You perform a regression of the variable %s on the 3 variables
    %s. You want to test whether the coefficient of %s is equal to zero or not.
    Report the <b>p-value</b> of this test (all decimals).(1 point)", 
    BlueCourier("y1"), BlueCourier("x1, x2, x3"), BlueCourier("x1")),
    "<b>p-value (all decimals):</b><br />[statistic]")
  vAnswerNames <- c("statistic")
  vAnswers <- c("summary(lm(df$y1 ~ df$x1 + df$x2 + df$x3))$coefficients[\"x1\", 4]")
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2d <- function() {
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You perform a regression of the variable %s on the 3 variables
    %s. You want to use a t-test so test whether the coefficient for %s is <strong>larger than 5</strong>. Report 
    the R command for computing this t-test, as well as the numerical value of the t-test statistic 
    (all decimals). (1 point)", 
                     BlueCourier("y1"), BlueCourier("x1, x2, x3"), BlueCourier("x3")),
             "<b>R command(s):</b><br />[Rcomm]",
             "<b>Numerical value (all decimals):</b><br />[statistic]"
             )
  vAnswerNames <- c("Rcomm", "statistic")
  vAnswers <- c("(summary(lm(df$y1 ~ df$x1 + df$x2 + df$x3))$coefficients[\"x3\", 1] - 5)/summary(lm(df$y1 ~ df$x1 + df$x2 + df$x3))$coefficients[\"x3\", 2]", ".")
    
                  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2e <- function() {
  
  iAlpha <- sample(c(1,5,10), 1)
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You want to test whether the two categorical variables %s 
    and %s are related. Provide the R command for doing the appropriate test for
    this, and report the numerical value of the test statistic, as well as your decision at $\\alpha=%d%%$. 
    Ignore any R warnings when answering this question. (1 point)", 
    BlueCourier("z1"), BlueCourier("z2"), iAlpha),
    "<b>R command(s):</b><br />[Rcomm]",
    "<b>Numerical value test statistic (all decimals):</b><br />[statistic]",
    "<b>Decision (reject H0/not reject H0):</b><br />[decis]")
  vAnswerNames <- c("Rcomm", "statistic", "decis")
  vAnswers <- c("chisq.test(df$z1, df$z2)", "chisq.test(df$z1, df$z2)$statistic", "c(\"reject H0\",\"not reject H0\")[1 + (chisq.test(df$z1, df$z2)$p.value &gt; iAlpha/100)]")
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_2ftest <- function() {
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You want to know whether the means of the variable %s are
    the same for the different levels of the categorical variable %s. 
    Provide the R command for doing the appropriate test for this at $\\alpha=0.05$,
    the value of the test statistic, and your decision. (1 point)", 
    BlueCourier("Salary"), BlueCourier("Nationality")),
    "<b>R command(s):</b><br />[Rcomm]",
    "<b>Numerical value test statistic (all decimals):</b><br />[statistic]",
    "<b>Decision (reject H0/not reject H0):</b><br />[decis]")
  vAnswerNames <- c("Rcomm", "statistic", "decis")
  vAnswers <- c("summary(aov(df$Salary ~ df$Nationality))", 
                  "report the F-statistic of the output here: 2.855302", 
                  "the p-value is 0.09617829 &gt; 0.05, so \"reject H0\"")
                  
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_3a <- function() {
  
  iProb <- sample(30:70, 1)
  iThrows <- sample(15:30, 1)
  iHeads <- round(runif(1, min = 0.3, max = 0.7) * iThrows)
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("You have an unfair coin, that yields \"heads\" with 
    probability %d%%. What is the probability of observing precisely %d times 
    \"heads\" out of %d coin flips? Provide the R command for computing this 
    probability, as well as the numerical value of the probability (all 
    decimals). (1 point)", iProb, iHeads, iThrows),
    "<b>R command(s):</b><br />[Rcomm]",
    "<b>Numerical value (all decimals):</b><br />[prob]")
  vAnswerNames <- c("Rcomm", "prob")
  vAnswers <- c(sprintf("dbinom(%d,%d,%1.2f)", iHeads, iThrows, iProb/100), dbinom(iHeads, iThrows, iProb/100))
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_3b <- function() {
  
  iMean <- sample(5:25, 1) * 10
  iSd <- sample(5:30, 1)
  iLow <- round(iMean + iSd * qnorm(runif(1, min = 0.1, max = 0.4)))
  iUp <- round(iMean + iSd * qnorm(runif(1, min = 0.6, max = 0.9)))
                              
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("If $X$ is normally distributed with mean %d and variance
    %d, what is the R command for computing the probability that $X$ lies in the interval 
    from %d to %d, and what is the numerical value of this probability (all decimals).
    (1 point)", iMean, iSd*iSd, iLow, iUp),
             "<b>R command(s):</b><br />[Rcomm]",
             "<b>Numerical value (all decimals):</b><br />[prob]")
  vAnswerNames <- c("Rcomm", "prob")
  vAnswers <- c(sprintf("pnorm(%d, mean = %d, sd = %d) - pnorm(%d, mean = %d, sd = %d)", iUp, iMean, iSd, 
                        iLow, iMean, iSd), pnorm(iUp, mean = iMean, sd = iSd) - pnorm(iLow, mean = iMean, sd = iSd))
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_3c <- function() {
  
  iAlpha <- sample(c(1,5,10), 1)
  iSide <- matrix(c(-1,"left",0,"two",1,"right"), nrow = 2)[ , sample(1:3,1)]
  iDf <- sample(5:22, 1)

  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("What is the R command to compute the <b>critical value</b> of 
  a %s-tailed t-test at $\\alpha = %d%%$ with %d degrees of freedom, and what is its
  numerical value (all decimals)? If there is more than one critical value, provide 
  the <b>highest</b> one. (1 point)", iSide[2], iAlpha, iDf),
             "<b>R command:</b><br />[Rcomm]",
             "<b>(highest) critical value (all decimals):</b><br />[quant]"
    )
  vAnswerNames <- c("Rcomm", "quant")
  vAnswers <- c(sprintf("qt(%1.3f, iDf, lower.tail = %s)", 0.01*iAlpha/(1 + (iSide[1]==0)), iDf,
                        c("TRUE","FALSE")[1 + (iSide[1] >= 0)]),
                qt(0.01*iAlpha/(1+(iSide[1]==0)), iDf, lower.tail = (iSide[1]>=0)))

  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_3d <- function() {
  
  iMean <- sample(5:25, 1) * 10
  iSd <- sample(5:30, 1)
  iLow <- round(iMean + iSd * qnorm(runif(1, min = 0.1, max = 0.4)))
  iUp <- sample(0:1,1)
  
  sType <- "mb"
  sq <- ""
  sText <- c(sprintf("If $X$ is normally distributed with mean %d and variance
    %d, what is the R command to compute the probability that $X$ lies %s %d,
    and what is the numerical value of this probability (all decimals)?
    (1 point)", iMean, iSd*iSd, c("below", "above")[iUp+1], iLow),
             "<b>R command(s):</b><br />[Rcomm]",
             "<b>Numerical value (all decimals):</b><br />[prob]")
  vAnswerNames <- c("Rcomm", "prob")
  vAnswers <- c(sprintf("pnorm(%d, mean = %d, sd = %d, lower.tail = %s)", iLow, iMean, iSd, 
                        c("TRUE","FALSE")[iUp+1]), pnorm(iLow, mean = iMean, sd = iSd, lower.tail = (iUp == 0)))
  
  return (list(type=sType, q=sq, text=sText, answernames=vAnswerNames, correct=vAnswers))
}



############################################################################
############################################################################
############################################################################


Q_Selfie <- function() {
  
  Selfie <- matrix(c(
    "your hand holds the ID card index finger at top, thumb at bottom, no other fingers.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2a.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2b.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger and pinky up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2c.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and pinky.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2d.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb (left/right or right/left).",
    "https://personal.vu.nl/a.lucas/garbage/aapje2e.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger and ring finger up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2f.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb at one of the UPPER two corners.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2g.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb at one of the LOWER two corners.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2h.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb across one of the two diagonals.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2i.jpg",
    1200, 638
  ), ncol = 4, byrow = TRUE)
  i1 <- sample(1:nrow(Selfie), 1)
  
  sType <- "upl"
  sq <- "Selfie"
  sText <- c("Upload a selfie such that:",
                        HtmlUlList(c(
                          "your face is clearly visible",
                          "your ID with photo is clearly visible",
                          Selfie[i1,1])),
                        sprintf("<p><img src=\"%s\" alt=\"\" width=\"%s\" height=\"%s\" data-decorative=\"true\"></p>",
                                Selfie[i1,2], Selfie[i1,3], Selfie[i1,4]))
  
  return (list(type=sType, q=sq, text=sText))
}


############################################################################
############################################################################
############################################################################


Q_scriptupload <- function() {
  
  sType <- "upl"
  sq <- ""
  sText <- c("Please upload your R script file.")
  return (list(type=sType, q=sq, text=sText))
}



