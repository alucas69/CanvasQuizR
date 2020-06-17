########################################
## q3_core
##
## Purpose:
##   Generate data for Q3
##
## Author:
##   Charles Bos
##
## Version:
##   a    first start
##   b    implementing in the scripts
###  c    update
##
## Date:
##   2020/4/20
########################################

# Imports
library(xlsx)
library("stringi")
library("crayon")

# source("../../aidfunctions.R")
# source("../../RoundAnswer.R")

########################################
# gendata(iN, vP1, vP2)
#
# Purpose:
#   Generate data looking like the price data
#
# Inputs:
#   iN    integer, number of observations
#   vP1   vector of size 4, with regressors and sdev of creating area
#   vP2   vector of size 5, with regressors and sdev of creating price
#   lNames   list of strings, names of regressand, continuous regressor, and two dummy regressors
#
# Return value:
#   df    data frame, data
gendata <- function(iN, vP1, vP2, lNames){
  df= data.frame(matrix(rep(0, 4*iN), iN, 4))
  colnames(df)=lNames   # c("Price", "Area", "Rural", "City")
  df[3]= as.integer((1:iN) < iN/3)
  df[4]= as.integer(((1:iN) > iN/3) & ((1:iN) < 2*iN/3))

  df[2]= vP1[1]+vP1[2]*df[3] + vP1[3]*df[4] + vP1[4]*rnorm(iN)
  df[1]= vP2[1]+vP2[2]*df[2] + vP2[3]*df[3] + vP2[4]*df[4] + vP2[5]*rnorm(iN)

  # df[1]
  return (df)
}

########################################
# estdata(df)
#
# Purpose:
#   Estimate data looking like the price data
#
# Inputs:
#   df    data frame, data
#
# Return value:
#   d$LM0, d$LM1, d$S0, d$S1  dictionary with small and large model, and summaries
estdata <- function(df){
  # print (summary(df))

  asC= colnames(df)
  f0= as.formula(sprintf("df$%s~df$%s", asC[1], asC[2]))
  f1= as.formula(sprintf("df$%s~df$%s+df$%s+df$%s", asC[1], asC[2], asC[3], asC[4]))

  r0= lm(f0)
  s0= summary(r0)     # From here, set vP1

  r1= lm(f1)
  s1= summary(r1)

  return (list(LM0=r0, LM1=r1))
}

#############################################################################
# lac= Qselect(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
# Purpose:
#   Select iAlt of the answers, ensuring that at least mincorrect of the correct answers
#   are included, and at most maxcorrect answers.
#   If requested, add a 'none of the above'
Qselect <- function(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=NULL, mincorrect=NULL){
  iA= length(vAnswers)

  if (is.null(maxcorrect))
    maxcorrect= iA
  if (is.null(mincorrect))
    mincorrect= 0

  if (sum(as.integer(vCorrect)) < mincorrect){
    print ("Warning: Not sufficient correct answers")
    return (NULL)
  }
  if (iAlt < mincorrect){
    print ("Warning: Not sufficient draws from correct answers")
    return (NULL)
  }
  if (maxcorrect < mincorrect)
    maxcorrect= mincorrect
  if (mincorrect > maxcorrect)
    mincorrect= maxcorrect

  iC= -1
  while ((iC < mincorrect) | (iC > maxcorrect)){
    vI= sample(1:iA, iAlt)
    iC= sum(as.integer(vCorrect[vI]))
  }
  vAnswers= vAnswers[vI]
  vCorrect= vCorrect[vI]
  if (addnone){
    vAnswers= c(vAnswers, "None of the above")
    vCorrect= c(vCorrect, iC == 0)
  }

  return (list(answer=vAnswers, correct=vCorrect))
}


#' Write a pretty-looking p-value
#'
#' This function writes a pretty looking p-value, based on F statistics
#' @param f is a structure with elements for the f-statistic
#' @keywords regression summary f-statistic
#' @export sPv holds the output string, p-value
#' @examples
#' myprettyr2fprint(summary(lm0.r)$fstatistic)
myprettypvf <- function(f){
  dPv= pf(f[1], f[2], f[3], lower.tail=FALSE)
  sPv= sprintf("%.4g", dPv)
  if (dPv < 2.22e-16)
    sPv= "&lt; 2.22e-16"
  return (sPv)
}

#' Write a pretty-looking regression summary
#'
#' This function writes a pretty looking regression summary to a string
#' @param s is a list structure with elements from the summary
#' @keywords regression summary
#' @export sOut holds the output string, 3-lines
#' @examples
#' myprettyr2fprint(summary(lm0.r))
myprettyr2fprint <- function(s){
  sPv= myprettypvf(s$fstatistic)
  sOut= sprintf('Residual standard error: %.1f on %i degrees of freedom\nMultiple R-squared: %.4g,	Adjusted R-squared: %.4g\nF-statistic: %.3f on %i and %i DF,  p-value: %s',
      s$sigma, s$df[2], s$r.squared, s$adj.r.squared, s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], sPv)

  return (sOut)
}

Q20200605_3core <- function() {

  #
  # generate the random numbers
  #
  # NB: Original numbers from datafile data/tentamenBS-Stat2020-03-25data.xlsx, see q3.R
  iN= 36 + sample(-5:5, 1)
  vP1= c(200, -4.2, 3, 30)
  vP2= c(14, 1, -112, 212, 25)

  Lots= list(YX=c("Price", "Area", "Rural", "City", "Rural"),
             Variables= c("Price (in k$)", "Area (in m2)", "Rural location, dummy", "City location, dummy", "Suburban location, dummy"),
             Description= "prices of lots. The price may be linked to the area of lot, and its location, in either a rural zone, suburban or a city",
             Zone= "California",
             Person= "A real estate agent",
             Topic= "lot prices",
             TopicEx= "These lots (bouwkavels) are empty pieces of land where a new house may be build.",
             Extra= "m2 of lot size")
  Sales= list(YX=c("Sales", "Area", "Rural", "City", "Rural"),
              Variables= c("Sales (in k$)", "Surface area of parking lot (in number of places)", "Rural location, dummy", "City location, dummy", "Suburban location, dummy"),
              Description= "sales of supermarkets. The sales volume may be linked to the area of the parking lot, and the location of the supermarket, in either a rural zone, suburban or a city",
              Zone= "a number of US states",
              Person= "A marketing analyst",
              Topic= "supermarket sales",
              TopicEx= "",
              Extra= "parking spot")
  Corona= list(YX=c("Infected", "Inhabitants", "Rural", "City", "Rural"),
               Variables= c("Number of infected patients", "Number of inhabitants (x 1000)", "Rural location, dummy", "City location, dummy", "Suburban location, dummy"),
               Description= "number of inhabitants infected by the corona virus. The number of infected people may be linked to the population in the county (in '000s of people), and the location of the county, in either a rural zone, suburban or a city",
               Zone= "a number of counties in the US",
               Person= "An epidemiologist",
               Topic= "corona infections",
               TopicEx= "",
               Extra= "1000 inhabitants")

  ######### Extend
  Settings <- list(Lots, Sales, Corona)

  # Choose setting
  iSet= sample(1:length(Settings), 1)
  # iSet= 1
  # print (sprintf("Using set %i", iSet))
  Setting <- Settings[[iSet]]

  # Generate the data
  df= gendata(iN, vP1, vP2, Setting$YX[1:4])
  # hist(df[1])
  # hist(df[2])

  # Estimate the regressions
  dEst= estdata(df)
  # Get text of question
  sIntro= sprintf("In %s, we investigate the %s. %s\nFor this purpose, we have taken a random sample of size %i of %s. \nBoth %s and %s are measured as continuous variables, whereas %s and %s are dummy variables related to the location.\nThe data were processed in R, with output for a restricted and an unrestricted regression model in Tables 1 and 2.\n<pre>%s\n%s</pre>\n<pre>%s\n%s</pre>",
                Setting$Zone, Setting$Topic, Setting$TopicEx, iN, Setting$Description, Setting$YX[1], Setting$YX[2], Setting$YX[3], Setting$YX[4],
                myprettytableprint(summary(dEst$LM0)$coefficients, title="Table 1: Restricted model"),
                myprettyr2fprint(summary(dEst$LM0)),
                myprettytableprint(summary(dEst$LM1)$coefficients, title="Table 2: Unrestricted model"),
                myprettyr2fprint(summary(dEst$LM1))
              )

  # Q3a1
  iTab= sample(1:2, 1)
  iMod= sample(1:2, 1)
  q3a1= list(iTab=iTab, iMod= iMod)

  return (list(N=iN, Setting=Setting, Est=dEst, Intro=sIntro, q3a1=q3a1))
}

Q_3a1 <- function() {
    #
    # though asking 3 decimals, we are more generous ...
    #
    iDigits <- 0
    iAa= 6  # Number of answers (excluding 'none-of-the-above' to select)
    dP= .5 # Probability of having the correct answer in the list

    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()
    #
    # formulate the question
    #
    # iTab= sample(1:2, 1)
    # iMod= sample(1:2, 1)
    iTab= Core$q3a1$iTab
    iMod= Core$q3a1$iMod
    sMod= c("theoretical", "estimated")[iMod]
    sTab= c("restricted", "unrestricted")[iTab]

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sprintf("What is the %s model corresponding to the output for the %s model?", sMod, sTab)
    )

    #
    # answers
    #
    mM0= summary(Core$Est$LM0)$coefficients
    mM1= summary(Core$Est$LM1)$coefficients
    vB0= mM0[,"Estimate"]
    vB1= mM1[,"Estimate"]
    vS0= mM0[,"Std. Error"]
    vS1= mM1[,"Std. Error"]

    # print (mM0)
    # print (mM1)
    Answers= c(0, "$y = \\beta_0 + \\epsilon$",
               as.integer((iMod == 1) && (iTab == 1)), "$y = \\beta_0 + \\beta_1 x_1 + \\epsilon$",
               0, "$y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\epsilon$",
               as.integer((iMod == 1) && (iTab == 2)), "$y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\beta_3 x_3 + \\epsilon$",
               0, "$\\hat y = \\beta_0 + \\beta_1 x_1$",
               0, "$\\hat y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2$",
               0, "$\\hat y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\beta_3 x_3$",
               0, sprintf("$\\hat y = %.3f + %.3f x_1 + \\hat e$", vB0[1], vB0[2]),
               as.integer((iMod == 2) && (iTab == 1)), sprintf("$\\hat y = %.3f + %.3f x_1$", vB0[1], vB0[2]),
               0, sprintf("$\\hat y = %.3f + %.3f x_1 + %.3f x_2  + %.3f x_3 + \\hat e$", vB1[1], vB1[2], vB1[3], vB1[4]),
               as.integer((iMod == 2) && (iTab == 2)), sprintf("$\\hat y = %.3f + %.3f x_1 + %.3f x_2  + %.3f x_3$", vB1[1], vB1[2], vB1[3], vB1[4]),
               0, sprintf("$\\hat y = %.3f + %.3f x_1 + \\hat e$", vS0[1], vS0[2]),
               0, sprintf("$\\hat y = %.3f + %.3f x_1$", vS0[1], vS0[2]),
               0, sprintf("$\\hat y = %.3f + %.3f x_1 + %.3f x_2  + %.3f x_3 + \\hat e$", vS1[1], vS1[2], vS1[3], vS1[4]),
               0, sprintf("$\\hat y = %.3f + %.3f x_1 + %.3f x_2  + %.3f x_3$", vS1[1], vS1[2], vS1[3], vS1[4])
             )
    Answers <- matrix(Answers, nrow = 2)

    iA= dim(Answers)[2]
    vP= (1-dP)*rep(1, iA)/(iA-1)
    vP[as.logical(as.integer(Answers[1,]))]= dP

    vI= sample(1:iA, iAa, prob= vP)
    AnswersSel= Answers[,vI]
    # for (i in 1:iAa){
    #   AnswersSel[2,i]= tex2math(AnswersSel[2,i])
    # }

    # add other option
    AnswersSel <- cbind(AnswersSel, c(1-sum(as.integer(AnswersSel[1,])), "None of the above"))

    # Counters <- MCquestion(QuizFile, "3a1", QuestionText,
    #                        AnswersSel[2,], as.numeric(AnswersSel[1,]), Counters, texify=TRUE)

  return (list(type='mc', q='3a1', text=QuestionText, answer=AnswersSel[2,], correct=as.numeric(AnswersSel[1,])))
}


Q_3a2 <- function() {
  {
    #
    # though asking 3 decimals, we are more generous ...
    #
    iDigits <- 0
    iAa= 8  # Number of answers (excluding 'none-of-the-above' to select)

    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()
    #
    # formulate the question
    #
    iTab= Core$q3a1$iTab
    sTab= c("restricted", "unrestricted")[iTab]
    lYX= Core$Setting$YX
    lVars= Core$Setting$Variables

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sprintf("What descriptions of variables belong to the model description for the %s model? Select *all* that apply.", sTab)
    )

    #
    # answers
    #
    Answers= c(1, sprintf("$y$: %s", lVars[1]),
               1, sprintf("$x_1$: %s", lVars[2]),
               as.integer(iTab == 2), sprintf("$x_2$: %s", lVars[3]),
               as.integer(iTab == 2), sprintf("$x_3$: %s", lVars[4]),
               0, sprintf("$x_4$: %s", lVars[5]),
               0, "$\\beta_0$: intercept",
               0, sprintf("$\\beta_0$: %s", lVars[1]),
               0, sprintf("$\\beta_0$: coefficient of %s", lVars[1]),
               0, sprintf("$\\beta_1$: coefficient of %s", lVars[2]),
               0, sprintf("$\\beta_2$: coefficient of %s", lVars[3]),
               0, sprintf("$\\beta_3$: coefficient of %s", lVars[4]),
               0, sprintf("$\\beta_0$: expected increase in %s", lVars[1]),
               0, sprintf("$\\beta_1$: expected increase in %s, when %s increases by 1 unit", lVars[1], lVars[2]),
               0, sprintf("$\\beta_2$: expected increase in %s, when %s increases by 1 unit", lVars[1], lVars[3]),
               0, sprintf("$\\beta_3$: expected increase in %s, when %s increases by 1 unit", lVars[1], lVars[4]),
               0, sprintf("$\\beta_4$: expected increase in %s, when %s increases by 1 unit", lVars[1], lVars[5])
             )
    Answers <- matrix(Answers, nrow = 2)

    # Select iAa out of iA answers, ordered randomly, including all correct ones
    lac= Qselect(Answers[2,], as.numeric(Answers[1,]), iAa, addnone=FALSE, mincorrect=2*iTab)

    # Counters <- MAquestion(QuizFile, "3a2", QuestionText,
    #                        AnswersSel[2,], vCorrect, Counters, texify=TRUE)
  }

  return (list(type='ma', q='3a2', text=QuestionText, answer=lac$answer, correct=lac$correct))
  # return(Counters)
}

Q_3a3 <- function() {

  {
    #
    # though asking 3 decimals, we are more generous ...
    #
    iDigits <- 3
    dYLim= 5

    dYPred= -1
    while (dYPred < dYLim){
      #
      # generate the random numbers and question core
      #
      Core <- Q20200605_3core()
      #
      # formulate the question
      #
      iTab= Core$q3a1$iTab
      lYX= Core$Setting$YX
      lVars= Core$Setting$Variables

      sTab= c("restricted", "unrestricted")[iTab]
      vX= c(1, sample(seq(50, 150, 5), 1), 0, 0, 0)

      iD= sample(3:5, 1)    # Choose rural, city or suburban
      vB= Core$Est$LM0$coefficients
      if (iTab == 2){
        vX[iD]= 1       # Only use the location for the unrestricted model
        vB= Core$Est$LM1$coefficients
      }

      # Calculate the answer:
      iB= length(vB)
      vB= round(vB, 3)        # Get at the same footing as the students...
      dYPred= (vX[1:iB] %*% vB)[1,1]

      if (dYPred < dYLim)
        print (sprintf("Warning: y= %g < %g found, try again...", dYPred, dYLim))
    }

    sQ= sprintf("Use the output in the %s model in order to predict the value of '%s' given that '%s'=%g when the location is %s", sTab, lYX[1], lYX[2], vX[2], lYX[iD])
    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sQ,
      sprintf("Give your answer with a precision of %d decimals after the decimal point.", iDigits)
    )

    #
    # Round the answers
    #
    Answers <- RoundAnswer(dYPred, iDigits)
    #
    # pose the question
    #
    # Counters <- NUMquestion(QuizFile, "3a3", QuestionText,
    #                         Answers, Counters, texify=TRUE)
  }

  return (list(type='num', q='3a3', text=QuestionText, correct=Answers))
  # return(Counters)
}


Q_3b1 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()
    #
    # formulate the question
    #
    dA= 0.05    # Critical value
    dR= 0.30    # Limit for relevance
    i0= 5       # Number of incorrect answers
    sQ= sprintf("%s wants to use the simplest possible model for the purpose of predicting a *realistic* %s.", Core$Setting$Person, Core$Setting$Variables[1])

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sQ,
      sprintf("Which one of the two models would you advise her/him to use? Select the reasoning which describes the *best* choice. If you need a critical value, use $\\alpha=%i$%%, or use a level of %i%% for relevance.", dA*100, dR*100)
    )

    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
    p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)
    ar20= s0$adj.r.squared
    ar21= s1$adj.r.squared

    # print (sprintf("M0: r2= %g, F p-value=%g", ar20, p0))
    # print (sprintf("M1: r2= %g, F p-value=%g", ar21, p1))

    #
    # Prepare the answers
    #
    Answers= c(0,                              "Model 1, as it is the simplest model",
               1*(p0 < dA),                    "Model 1, as it is the simplest model but still significant according to the $F$ statistic",
               0,                              "Model 1, as it is the simplest model but still significant according to the $R^2$ statistic",
               1*(ar20 > dR),                  "Model 1, as it is the simplest model and relevant according to the $R^2$ statistic",
               1*((ar20 > dR) & (ar21 < dR)),  "Model 1, as it is the only relevant model according to the $R^2$ statistic",
               0,                              "Model 1, as it is the only relevant model according to the $F$ statistic",
               0,                              "Model 1, as it is the only significant model according to the $R^2$ statistic",
               1*((p0 < dA) & (p1 > dA)),      "Model 1, as it is the only significant model according to the $F$ statistic",
               0,                              "Model 1, as it is the simplest model and relevant according to the $F$ statistic",
               1*((p0 < dA) & (p1 > dA)),      "Model 1, as it is only significant model",
               0, "Model 2, as it is the simplest model",
               0, "Model 2, as it is the simplest model but still significant according to the $F$ statistic",
               0, "Model 2, as it is the simplest model but still significant according to the $R^2$ statistic",
               0, "Model 2, as it is the simplest model and relevant according to the $R^2$ statistic",
               2*((ar20 < dR) & (ar21 > dR)) , "Model 2, as it is the only relevant model according to the $R^2$ statistic",
               0,                              "Model 2, as it is the only relevant model according to the $F$ statistic",
               0,                              "Model 2, as it is only significant model according to the $R^2$ statistic",
               3*((p0 > dA) & (p1 < dA)),      "Model 2, as it is only significant model according to the $F$ statistic",
               0,                              "Model 2, as it is the simplest model which is relevant according to the $F$ statistic"
              )
    Answers <- matrix(Answers, nrow = 2)


    vI= as.numeric(Answers[1,])
    vI0= which(vI == 0)           # Check what answers are considered WRONG
    vI1= which(vI > 0)            # Check what answers are considered RIGHT (to some extend at least)
    vP= vI[vI1]/sum(vI[vI1])      # Get probabilities: The more right, the higher the probability of selecting it?

    v0Sel= sample(vI0, size=i0)
    i1Sel= sample(vI1, size=1, prob=vP)

    vSel= c(v0Sel, i1Sel)
    AnswersSel= Answers[,vSel]
    AnswersSel[1,]= as.numeric(AnswersSel[1,]) > 0

    # Reshuffle selected answers
    vI= sample(1:(i0+1), i0+1)
    AnswersSel= AnswersSel[,vI]

    #
    # pose the question
    #
    # Counters <- MCquestion(QuizFile, "3b1", QuestionText,
                           # AnswersSel[2,], AnswersSel[1,], Counters, texify=TRUE)
  }

  return (list(type='skip', q='3b1', text=QuestionText, answer=AnswersSel[2,], correct=AnswersSel[1,]))
  # return(Counters)
}


Q_3b2 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()
    #
    # formulate the question
    #
    iDigits= 3
    i0= 4
    iSP= sample(1:2, 1)
    sSP= c("statistical significance", "practical relevance")[iSP]

    iMod= sample(1:2, 1)
    sMod= c("restricted model", "unrestricted model")[iMod]
    sQ= sprintf("What numeric value(s) describe(s) the %s of the %s?", sSP, sMod)

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      "(Select *ALL* answers that apply)"
    )

    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
    p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)
    r20= s0$r.squared
    r21= s1$r.squared
    ar20= s0$adj.r.squared
    ar21= s1$adj.r.squared

    sP0= myprettypvf(s0$fstatistic)
    sP1= myprettypvf(s1$fstatistic)

    # print (sprintf("M0: F= %g, p-value=%s, r2= %g, ar2= %.4g", s0$fstatistic[1], sP0, r20, ar20))
    # print (sprintf("M1: F= %g, p-value=%s, r2= %g, ar2= %.4g", s1$fstatistic[1], sP1, r21, ar21))

    #
    # Prepare the answers
    #
    Answers= c(FALSE,                          sprintf("%.2f", s0$sigma),
               FALSE,                          sprintf("%.2f", s1$sigma),
               FALSE,                          sprintf("%i", s0$df[2]),
               FALSE,                          sprintf("%i", s1$df[2]),
               FALSE,                          sprintf("%i", s0$fstatistic[2]),
               FALSE,                          sprintf("%i", s1$fstatistic[2]),
               FALSE,                          sprintf("%i", s0$fstatistic[3]),
               FALSE,                          sprintf("%i", s1$fstatistic[3]),
               (iMod == 1) & (iSP == 1),       sprintf("%.3f", s0$fstatistic[1]),
               (iMod == 2) & (iSP == 1),       sprintf("%.3f", s1$fstatistic[1]),
               (iMod == 1) & (iSP == 1),       sP0,
               (iMod == 2) & (iSP == 1),       sP1,
               (iMod == 1) & (iSP == 2),       sprintf("%.4g", r20),
               (iMod == 2) & (iSP == 2),       sprintf("%.4g", r21),
               (iMod == 1) & (iSP == 2),       sprintf("%.4g", ar20),
               (iMod == 2) & (iSP == 2),       sprintf("%.4g", ar21)
              )
    Answers <- matrix(Answers, nrow = 2)
    iA= ncol(Answers)

    # Find indices of non-duplicated answers
    vI= !duplicated(Answers[2,])
    Answers= Answers[,vI]   # Get rid of duplicates

    vI= as.logical(Answers[1,])
    vI0= which(!vI)           # Check what answers are considered WRONG
    vI1= which(vI)            # Check what answers are considered RIGHT (to some extend at least)

    # Select some wrong answers
    vI= sample(vI0, i0)
    # Reshuffle wrong + right answers
    vI= sample(c(vI, vI1))
    vAnswers= Answers[2,vI]
    vCorrect= as.logical(Answers[1,vI])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3b2", QuestionText,
    #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='skip', q='3b2', text=QuestionText, answer=vAnswers, correct=vCorrect))
  # return(Counters)
}

Q_3c1 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iAa= 8

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    iH= sample(1:2, 1)
    sH= c("null hypothesis", "alternative hypothesis")[iH]
    if (iMod == 1){
      dB1= Core$Est$LM0$coefficients[2]
    } else {
      dB1= Core$Est$LM1$coefficients[2]
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)

    sQ= sprintf("John claims that every extra %s will increase the %s with %s %.1f. Can John's claim be proven by the results of the %s model?", Core$Setting$Extra, Core$Setting$Topic, sLim, dB10, sMod)

    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sHCorrect= c(sH0, sH1)[iH]

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      sprintf("What is the relevant %s?", sH)
      # sprintf("[Check thoroughly, did I put the right answer here? Should be %s...]", sHCorrect)
    )

    #
    # Prepare the answers
    #
    asCmp= list(c("=", "\\ge", "\\le"), c("\\not=", ">", "<"))
    Answers= list()
    for (iH in 0:1){
      for (b in 0:1){
        for (sC in asCmp[[iH+1]]){
          sHi= sprintf("$H_%i: \\beta_%i %s %.1f$", iH, b, sC, dB10)
          Answers= c(Answers, sHCorrect == sHi, sHi)
        }
      }
    }

    Answers= matrix(Answers, nrow=2)
    iA= ncol(Answers)

    iI1= which(as.logical(Answers[1,]))
    vI0= which(!(as.logical(Answers[1,])))

    # Get some wrong answers, plus correct answer
    vI= c(sample(vI0, iAa-1), iI1)

    # Reshuffle
    vI= sample(vI)
    AnswersSel= Answers[,vI]

    # Add in fake 'none of the above'
    AnswersSel= cbind(AnswersSel, rbind(FALSE, "None of the above"))
    vCorrect= as.logical(AnswersSel[1,])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3c1", QuestionText,
    #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='mc', q='3c1', text=QuestionText, answer=AnswersSel[2,], correct=vCorrect))
  # return(Counters)
}


Q_3c2 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iAa= 8

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    if (iMod == 1){
      dB1= Core$Est$LM0$coefficients[2]
    } else {
      dB1= Core$Est$LM1$coefficients[2]
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)

    sQ= sprintf("John claims that every extra %s will increase the %s with %s %.1f in the %s model. To try and prove his point, he wants to test the null hypothesis %s against the alternative %s. What is the corresponding test statistic?", Core$Setting$Extra, Core$Setting$Topic, sLim, dB10, sMod, sH0, sH1)

    QuestionText <- c(
      "In the following questions, the numbers and setting may
      change, though questions are related. Use the numbers of the *question on screen*
      to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      "Select *all* possible answers"
      #, "TRIPLE CHECK: Option $b_1$ is also considered a correct answer, the non-standardised test statistic... IBA does not know this?"
      # Confusing option has been removed...
    )

    #
    # Prepare the answers
    #
    Answers= list()
    for (i in 0:1){
      for (sE in c("\\beta", "b")){
        if ((i == 0) | (sE != "b"))  # Do not add a too confusing option...
          Answers= c(Answers, (sE == "b") & (i == 1), sprintf("$%s_%i$", sE, i))
        if ((i == 1) | (sE != "b"))  # Do not add a too confusing option...
          Answers= c(Answers, (sE == "b") & (i == 1), sprintf("$t= (%s_%i - %g)/s_{%s_%i}$", sE, i, dB10, sE, i))
        Answers= c(Answers, FALSE, sprintf("$t= %s_%i/s_{%s_%i}$", sE, i, sE, i))
      }
      Answers= c(Answers, FALSE, sprintf("$t= (b_%i - %g)/s^2_{b_%i}$", i, dB10, i))
      Answers= c(Answers, FALSE, sprintf("$t= b_%i/s^2_{b_%i}$", i, i))
    }

    Answers= matrix(Answers, nrow=2)
    iA= ncol(Answers)

    # Get some wrong answers, plus correct answer
    vI0= which(!(as.logical(Answers[1,])))
    vI1= which(as.logical(Answers[1,]))

    # Get some wrong answers, plus correct answers
    vI= c(sample(vI0, iAa-2), vI1)

    # Shuffle
    vI= sample(1:iA, iAa)
    AnswersSel= Answers[,vI]

    # Add in fake 'none of the above'
    AnswersSel= cbind(AnswersSel, rbind(FALSE, "None of the above"))
    vCorrect= as.logical(AnswersSel[1,])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3c2", QuestionText,
    #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='ma', q='3c2', text=QuestionText, answer=AnswersSel[2,], correct=vCorrect))
  # return(Counters)
}

Q_3c3 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iAa= 4

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    if (iMod == 1){
      dB1= Core$Est$LM0$coefficients[2]
    } else {
      dB1= Core$Est$LM1$coefficients[2]
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("$t= (b_1 - %g)/s_{b_1}$", dB10)

    sQ= sprintf("John wants to test the null hypothesis %s in the %s model, with test statistic %s.", sH0, sMod, sTest)

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      "In this case, the test should reject:"
    )

    #
    # Prepare the answers
    #
    Answers= c(
      (iLim == 1), "on the left side",
      (iLim == 2), "on the left and right side",
      (iLim == 3), "on the right side",
      FALSE, "in the middle",
      FALSE, "never",
      FALSE, "always",
      FALSE, "cannot tell given the information"
      # FALSE, "none of the above",
    )

    Answers= matrix(Answers, nrow=2)
    iA= ncol(Answers)

    # Get some answers
    vI= sample(1:iA, iAa)
    AnswersSel= Answers[,vI]

    # Add none of the above
    iCorr= sum(as.logical(AnswersSel[1,]))
    AnswersSel= cbind(AnswersSel, rbind((iCorr == 0), "None of the above"))
    vCorrect= as.logical(AnswersSel[1,])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3c3", QuestionText,
    #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='ma', q='3c3', text=QuestionText, answer=AnswersSel[2,], correct=vCorrect))
  # return(Counters)
}


Q_3c4 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iAa= 8

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    if (iMod == 1){
      dB1= Core$Est$LM0$coefficients[2]
      iDfCorrect= Core$Est$LM0$df.residual
    } else {
      dB1= Core$Est$LM1$coefficients[2]
      iDfCorrect= Core$Est$LM1$df.residual
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("$t= (b_1 - %g)/s_{b_1}$", dB10)

    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    # p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
    # p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)

    sQ= sprintf("John wants to test the null hypothesis %s in the %s model, with test statistic %s.", sH0, sMod, sTest)

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      "In this case, if the null hypothesis is true, the test statistic $t$ is distributed as:"
    )

    #
    # Prepare the answers
    #
    Answers= c(
        FALSE, "a standard Normal distribution",
        FALSE, sprintf("an F(%i,%i) distribution", s0$fstatistic[2], s0$fstatistic[3]),
        FALSE, sprintf("an F(%i,%i) distribution", s1$fstatistic[2], s1$fstatistic[3])
      )
    for (df in (Core$N-4):Core$N){
      Answers= c(Answers,
        (df == iDfCorrect), sprintf("a $t_{%i}$ distribution", df),
        FALSE, sprintf("a $\\chi^2_{%i}$ distribution", df)
      )
    }

    Answers= matrix(Answers, nrow=2)
    iA= ncol(Answers)

    # Get some answers
    vI= sample(1:iA, iAa)
    vI= sort(vI)
    AnswersSel= Answers[,vI]

    # Add none of the above
    iCorr= sum(as.logical(AnswersSel[1,]))
    AnswersSel= cbind(AnswersSel, rbind((iCorr == 0), "None of the above"))
    vCorrect= as.logical(AnswersSel[1,])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3c4", QuestionText,
    #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='ma', q='3c4', text=QuestionText, answer=AnswersSel[2,], correct=vCorrect))
  # return(Counters)
}


Q_3c5 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iAa= 4

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    if (iMod == 1){
      dB1= Core$Est$LM0$coefficients[2]
      iDfCorrect= Core$Est$LM0$df.residual
    } else {
      dB1= Core$Est$LM1$coefficients[2]
      iDfCorrect= Core$Est$LM1$df.residual
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("$t= (b_1 - %g)/s_{b_1}$", dB10)
    sY= Core$Setting$YX[1]
    sX= Core$Setting$YX[2]

    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    # p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
    # p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)

    sQ= sprintf("John wants to test the null hypothesis %s in the %s model, with test statistic %s.", sH0, sMod, sTest)

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      #DigitDecimalWarning(iDigits)
      "This test statistic will follow a Student-$t$ distribution if the following assumptions hold (select *ALL* which are relevant):"
      # , "[To discuss: Drop this question? Does the randomisation make sense? Disputable whether one could/should add linearity etc.]"
    )

    #
    # Prepare the answers
    #
    Answers= c(
        FALSE, sprintf("%s is normally distributed", sX),
        FALSE, sprintf("%s is normally distributed", sY),
        TRUE, "The error term is normally distributed",
        FALSE, "The error term is Student-$t$ distributed",
        FALSE, "The parameters in the model are normally distributed",
        TRUE, "The model is well specified",
        TRUE, "The number of observations is at least 30",
        FALSE, "The number of observations is at least 15",
        FALSE, "The number of regressors is at least 30",
        FALSE, "The number of regressors is at least 15"
      )

    Answers= matrix(Answers, nrow=2)
    iA= ncol(Answers)

    # Get some answers
    vI= sample(1:iA, iAa)
    vI= sort(vI)
    AnswersSel= Answers[,vI]

    # Add none of the above
    iCorr= sum(as.logical(AnswersSel[1,]))
    AnswersSel= cbind(AnswersSel, rbind((iCorr == 0), "Other"))
    vCorrect= as.logical(AnswersSel[1,])
    #
    # pose the question
    #
    # Counters <- MAquestion(QuizFile, "3c5", QuestionText,
                           # AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
  }

  return (list(type='skip', q='3c5', text=QuestionText, answer=AnswersSel[2,], correct=vCorrect))
  # return(Counters)
}


Q_3c6 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iDigits= 3

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    if (iMod == 1){
      dB1= s0$coefficients[2,1]
      dsB1= s0$coefficients[2,2]
      iDfCorrect= Core$Est$LM0$df.residual
    } else {
      dB1= s1$coefficients[2,1]
      dsB1= s1$coefficients[2,2]
      iDfCorrect= Core$Est$LM1$df.residual
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("$t= (b_1 - %g)/s_{b_1}$", dB10)
    dTest= (round(dB1,3) - round(dB10, 3))/round(dsB1, 3)

    sQ= sprintf("John wants to test the null hypothesis %s in the %s model, with test statistic %s.", sH0, sMod, sTest)

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      sprintf("What is the standardized value of the test statistic (in the *%s* model)?", sMod),
      DigitDecimalWarning(iDigits)
    )

    #
    # Prepare the answers
    #
    #
    # Round the answers
    #
    Answers <- RoundAnswer(dTest, iDigits)
    Answers <- matrix(Answers, ncol= 3)
    #
    # pose the question
    #
    # Counters <- NUMquestion(QuizFile, "3c6", QuestionText,
    #                         Answers, Counters, texify=TRUE)
  }

  return (list(type='skip', q='3c6', text=QuestionText, correct=Answers))
  # return(Counters)
}


Q_3c7 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iDigits= 3

    dAlpha= sample(c(0.01, 0.05, 0.10), 1)

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    if (iMod == 1){
      dB1= s0$coefficients[2,1]
      dsB1= s0$coefficients[2,2]
      iDfCorrect= Core$Est$LM0$df.residual
    } else {
      dB1= s1$coefficients[2,1]
      dsB1= s1$coefficients[2,2]
      iDfCorrect= Core$Est$LM1$df.residual
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("t= (b_1 - %g)/s_{b_1}", dB10)
    dTest= (round(dB1,3) - round(dB10, 3))/round(dsB1, 3)

    sQ= sprintf("John tested the null hypothesis %s in the %s model, and finds $%s=%g$.", sH0, sMod, sTest, dTest)

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      sprintf("What is the corresponding critical value, at level $\\alpha=%g$?", dAlpha),
      DigitDecimalWarning(iDigits),
      "(If there are two critical values, report the highest one)"
    )

    #
    # Prepare the answers
    #
    vCrit= qt(c(dAlpha, 1-dAlpha/2, 1-dAlpha), lower.tail= TRUE, df=FloorDfT(iDfCorrect))
    dCrit= vCrit[iLim]
    #
    # Round the answers
    #
    Answers <- RoundAnswer(dCrit, iDigits)

    #
    # pose the question
    #
    # Counters <- NUMquestion(QuizFile, "3c7", QuestionText,
    #                         Answers, Counters, texify=TRUE)
  }

  return (list(type='num', q='3c7', text=QuestionText, correct=Answers))
  # return(Counters)
}


Q_3c8 <- function() {

  {
    #
    # generate the random numbers and question core
    #
    Core <- Q20200605_3core()

    #
    # formulate the question
    #
    iDigits= 3

    dAlpha= sample(c(0.01, 0.05, 0.10), 1)

    iMod= sample(1:2, 1)
    sMod= c("restricted", "unrestricted")[iMod]
    iLim= sample(1:3, 1)
    sLim= c("less than", "something different from", "more than")[iLim]
    s0= summary(Core$Est$LM0)
    s1= summary(Core$Est$LM1)
    if (iMod == 1){
      dB1= s0$coefficients[2,1]
      dsB1= s0$coefficients[2,2]
      iDfCorrect= Core$Est$LM0$df.residual
    } else {
      dB1= s1$coefficients[2,1]
      dsB1= s1$coefficients[2,2]
      iDfCorrect= Core$Est$LM1$df.residual
    }
    dB10= round(dB1, 1) + 0.1*sample(-2:2, 1)
    sH0= sprintf("$H_0: \\beta_1 %s %.1f$", c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_1 %s %.1f$", c("<", "\\not=", ">")[iLim], dB10)
    sTest= sprintf("t= (b_1 - %g)/s_{b_1}", dB10)
    dTest= (round(dB1,3) - round(dB10, 3))/round(dsB1, 3)
    vCrit= qt(c(dAlpha, 1-dAlpha/2, 1-dAlpha), lower.tail= TRUE, df=FloorDfT(iDfCorrect))
    dCrit= vCrit[iLim]

    sQ= sprintf("John tested the null hypothesis %s in the %s model, and finds $%s=%g$.", sH0, sMod, sTest, dTest)
    if (iLim == 1){
      # sQ2= sprintf("As critical value, he finds $t_crit=%.3f$, and previously he had decided to reject for low values.", dCrit)
      sQ2= sprintf("As critical value, he finds $t_crit=%.3f$.", dCrit)
      bRej= (dTest <= dCrit)
    } else if (iLim == 2){
      # sQ2= sprintf("As critical values, he finds $t_L=%.3f$ and $t_U=%.3f$, and previously he had decided to reject for high and low values.", -dCrit, dCrit)
      sQ2= sprintf("As critical values, he finds $t_L=%.3f$ and $t_U=%.3f$.", -dCrit, dCrit)
      bRej= (dTest <= -dCrit) | (dTest >= dCrit)
    } else {
      # sQ2= sprintf("As critical value, he finds $t_crit=%.3f$, and previously he had decided to reject for high values.", dCrit)
      sQ2= sprintf("As critical value, he finds $t_crit=%.3f$.", dCrit)
      bRej= (dTest >= dCrit)
    }

    QuestionText <- c(
      "In the following questions, the numbers and setting may change, though questions are related. Use the numbers of the *question on screen* to answer the question.",
      Core$Intro,
      sQ,
      sQ2,
      "What is his conclusion?"
    )

    #
    # Prepare the answers
    #
    Answers <- c(bRej, "reject", FALSE, "accept", !bRej, "do not reject", FALSE, "None of the above")
    Answers <- matrix(Answers, nrow= 2)

    #
    # pose the question
    #
    # Counters <- MCquestion(QuizFile, "3c8", QuestionText,
    #                        Answers[2,], as.logical(Answers[1,]), Counters, texify=TRUE)
  }

  return (list(type='mc', q='3c8', text=QuestionText, answer=Answers[2,], correct=as.logical(Answers[1,])))
  # return(Counters)
}
