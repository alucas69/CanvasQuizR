########################################
## q3_core
##
## Purpose:
##   Generate data for Q3, 2020/6/29
##
## Author:
##   Charles Bos
##
## Version:
##   a    first start, using 20200605_3core
##
## Date:
##   2020/6/24
########################################

# Imports
# library("stringi")
# library("car")
#

source("../questions/Q_5step_regression_aid.R")
# source("../engine/RoundAnswer.R")

# Regression question (5 step plan) with 3 setups

########################################
# Q_5step_regression.1.1_gendata()
#
# Purpose:
#   Generate data for the regression
#
# Inputs:
#   iN    integer, number of observations
#   lSetting    list, with
#     $Vars     list, variable names
#     $Genr     list, with vectors used in generating the data
#     $Pars     list, standard error and beta's
#
# Return value:
#   df    data frame, data
Q_5step_regression.1.gendata <- function(iN, lSetting){
  asNames= lSetting$VarsSh
  avGenr= lSetting$Genr
  vP= lSetting$Pars

  iYX= length(asNames)
  df= data.frame(matrix(rep(1, iYX*iN), iN, iYX))

  # Replace variable names
  # asVars= 'y'
  # for (i in 2:iYX)
  #   asVars= c(asVars, sprintf('x%i', i-2))
  colnames(df)=asNames

  for (i in 1:iYX){
    vGenr= avGenr[[i]]
    iG= length(vGenr)
    if (iG == 1)
      df[i]= rep(vGenr, iN)
    else if (iG == 2)
      df[i]= round(runif(iN, min=vGenr[1], max= vGenr[2])) + .1*rnorm(iN)
    else if (iG == 3)
      df[i]= round(runif(iN, min=vGenr[1], max= vGenr[2]))/ vGenr[3]
    else if (iG == 4){
      dRho= vGenr[3]
      dS= vGenr[4]
      df[i]= dRho * df[i+vGenr[1]] * df[i+vGenr[2]] + dS*rnorm(iN)
    }
  }
  dS= vP[1]
  vBeta= vP[-1]
  df[1]= data.matrix(df[-1]) %*% vBeta + dS*rnorm(iN)

  return (df)
}

########################################
# setup()
#
# Purpose:
#   Prepare the setup of the regression question
#
# Inputs:
#
# Return value:
#   list, containing
#     $sample    data frame, data
#     $N         integer, number of observations
#     $results   list, for the moment empty
Q_5step_regression.1.setup <- function(){
  iN= 10*sample(0:5, 1) + 40

  Range= list(Vars= c('Range', 'Constant', 'Capacity battery', 'Charging speed', 'Width', 'Height', 'Front surface area'),
              VarsSh= c('Range', 'C', 'Cap', 'Charge', 'W', 'H', 'Surf'),
              Units= c('km', '-', 'kWh', 'kW', 'm', 'm', 'm2'),
              Genr= list(0, 1, c(40, 105), c(2, 30), c(160, 200, 100), c(130, 165, 100), c(-1, -2, .9, .4)),
              Pars= c(15, 4, 6, 0, -.01, -.03, -.6),
              Object= 'car',
              Extra= 'square meter of front surface area',
              Topic= 'Range of Battery Electric Vehicles')

  Water= list(Vars= c('Water outflow', 'Constant', 'Average rainfall', 'Density of vegetation', 'Width of area', 'Length of area', 'Surface'),
              VarsSh= c('Outflow', 'C', 'Rain', 'Dens', 'W', 'L', 'Surf'),
              Units= c('m3/month', '-', 'mm/month', 'kg/m3', 'km', 'km', 'km2'),
              Genr= list(0, 1, c(40, 105), c(2, 30), c(160, 200, 100), c(130, 165, 100), c(-1, -2, .9, .4)),
              Pars= c(15, 4, 6, -.5, .01, .03, .6),
              Object= 'nature reserve',
              Extra= 'square kilometer of nature reserve',
              Topic= 'Water outflow of nature reserves in Russia')

  Corona= list(Vars= c('Detected cases', 'Constant', 'Average temperature', 'Height above sealevel', 'Width of province', 'Length of province', 'Surface'),
               VarsSh= c('Counts', 'C', 'Temp', 'Height', 'W', 'L', 'Surf'),
               Units= c('thousands', '-', 'C', 'm', 'km', 'km', 'km2'),
               Genr= list(0, 1, c(5, 25), c(0, 300), c(160, 200, 100), c(130, 165, 100), c(-1, -2, .9, .4)),
               Pars= c(15, 200, 2, 0, .01, .03, .8),
               Object= 'province',
               Extra= 'square kilometer in the province',
               Topic= 'detected corona cases in provinces in France')

  # lSetting= sample(list(Range, Water, Corona), 1)[[1]]

  # Don't randomise
  lSetting= Range

  df= Q_5step_regression.1.gendata(iN, lSetting)

  asC= colnames(df)
  aiX0= c(3, 4, 7)-2
  aiX1= (3:7) - 2
  f0= as.formula(sprintf("%s~%s+%s+%s", asC[1], asC[3], asC[4], asC[7]))
  f1= as.formula(sprintf("%s~%s+%s+%s+%s+%s", asC[1], asC[3], asC[4], asC[5], asC[6], asC[7]))

  lm.0= lm(f0, data=df)
  summary(lm.0)

  lm.1= lm(f1, data=df)
  summary(lm.1)

  # vif(lm.1)
  #
  # plot(df)
  # dim(df)

  lSetting$N= iN
  lSetting$f0= f0
  lSetting$f1= f1
  lSetting$X0= aiX0
  lSetting$X1= aiX1
  lSetting$Person= sample(c('Maria', 'Ursula', 'Margarita', 'Ana', 'Nina'), 1)

  l_tables= list(lm0= myprettylmprint(lm.0, skipcall=TRUE), lm1= myprettylmprint(lm.1, skipcall=TRUE))

  return (list(sample= df, setting=lSetting, results=list(lm0= lm.0, lm1= lm.1), tables=l_tables))
}

########################################
# core()
#
# Purpose:
#   Prepare the text and output of the regression question
#
# Inputs:
#
# Return value:
#   list, containing
#     $sample    data frame, data
#     $N         integer, number of observations
#     $results   list, for the moment empty
Q_5step_regression.1.core <- function(){

  lSetup= Q_5step_regression.1.setup()
  Setting= lSetup$setting

  # generate test settings
  dAlpha= sample(c(0.1, 0.05, 0.01), 1)

  # Write intro
  Setting$Intro= c(
    # paste("In the following questions, the numbers and setting may change, though questions are related.
    #         Use the numbers of the", ColorBold("question on screen"), "to answer the question."),
    sprintf("Studying the %s, you obtained information on %i %ss of the %s.", Setting$Topic, Setting$N, Setting$Object, tolower(Setting$Vars[1])))
  Setting$Variables= sprintf("We explain '%s' ($y$, in %s) by a constant and the variables '%s' ($x_1$=%s, in %s), '%s' ($x_2$=%s, in %s), '%s' ($x_3$=%s, in %s), '%s' ($x_4$=%s, in %s) and '%s' ($x_5$=%s, in %s).", Setting$Vars[1], Setting$Units[1], Setting$Vars[3], Setting$VarsSh[3], Setting$Units[3], Setting$Vars[4], Setting$VarsSh[4], Setting$Units[4], Setting$Vars[5], Setting$VarsSh[5], Setting$Units[5], Setting$Vars[6], Setting$VarsSh[6], Setting$Units[6], Setting$Vars[7], Setting$VarsSh[7], Setting$Units[7])
  Setting$Alpha= dAlpha

  Setting$LM01= sprintf("The data were processed in R, with output for a restricted and an unrestricted regression model in Tables 1 and 2.\n<pre>%s\n%s</pre>\n<pre>%s\n%s</pre>",
                myprettytableprint(summary(lSetup$results$lm0)$coefficients, title="Table 1: Restricted model"),
                myprettyr2fprint(summary(lSetup$results$lm0)),
                myprettytableprint(summary(lSetup$results$lm1)$coefficients, title="Table 2: Unrestricted model"),
                myprettyr2fprint(summary(lSetup$results$lm1))
              )
  Setting$LMsep= c(sprintf("The data were processed in R, with output for a restricted regression model in Table 1.\n<pre>%s\n%s</pre>",
          myprettytableprint(summary(lSetup$results$lm0)$coefficients, title="Table 1: Restricted model"), myprettyr2fprint(summary(lSetup$results$lm0))),
          sprintf("The data were processed in R, with output for a unrestricted regression model in Table 2.\n<pre>%s\n%s</pre>", myprettytableprint(summary(lSetup$results$lm1)$coefficients, title="Table 2: Unrestricted model"), myprettyr2fprint(summary(lSetup$results$lm1))))
  iTab= sample(1:2, 1)
  sTab= c("restricted", "unrestricted")[iTab]
  iMod= sample(1:2, 1)
  sMod= c("theoretical", "estimated")[iMod]
  iLim= sample(1:3, 1)
  sLim= c("less than", "something different from", "more than")[iLim]
  sLimNeg= c("more than", "something different from", "less than")[iLim]

  Select= list(iTab= iTab, iMod= iMod, sTab= sTab, sMod= sMod, iLim= iLim, sLim= sLim, sLimNeg= sLimNeg)

  return (list(setting= Setting, sample= lSetup$sample, results=lSetup$results, select=Select))
}

Q_5step_regression.1.a1 <- function() {
    iAa= 6  # Number of answers (excluding 'none-of-the-above' to select)
    iC= 1   # Number of correct answers: Do include the correct answer

    #
    # generate the random numbers and question core
    #
    Core <- Q_5step_regression.1.core()
    #
    # formulate the question
    #
    iTab= Core$select$iTab
    iMod= Core$select$iMod
    sMod= c("theoretical", "estimated")[iMod]
    sTab= c("restricted", "unrestricted")[iTab]

    QuestionText <- c(
      Core$setting$Intro,
      Core$setting$Variables,
      Core$setting$LM01,
      ColorBold(sprintf("What is the **%s** model corresponding to the output for the **%s** model?", sMod, sTab))
    )

    #
    # answers
    #
    mM0= summary(Core$results$lm0)$coefficients
    mM1= summary(Core$results$lm1)$coefficients
    vB0= mM0[,"Estimate"]
    vB1= mM1[,"Estimate"]
    vS0= mM0[,"Std. Error"]
    vS1= mM1[,"Std. Error"]

    # summary(Core$results$lm1)

    vB= vB0
    vI= Core$setting$X0
    if (iTab == 2){
      vB= vB1
      vI= Core$setting$X1
    }

    sCorrect= c(Q_theormodel(vI), Q_empmodel(vI, vB))[iMod]

    vAnswers= list()
    for (x in list(Core$setting$X0, Core$setting$X1))
      for (bHat in c(FALSE, TRUE))
        for (sPar in c('\\beta', 'b'))
          vAnswers= c(vAnswers, Q_theormodel(x, hat=bHat, par=sPar))

    for (x in 1:2)
      for (p in 1:2)
        for (bHat in c(FALSE, TRUE))
          for (bRes in c(FALSE, TRUE)){
            vP= list(vB0, vS0, vB1, vS1)[[p+(x-1)*2]]
            cX= list(Core$setting$X0, Core$setting$X1)[[x]]
            length(vP)
            length(cX)
            vAnswers= c(vAnswers, Q_empmodel(cX, vP, hat=bHat, res=bRes))
          }
    sCorrect= Q_doubleminus(sCorrect)
    vAnswers= Q_doubleminus(vAnswers)

    vCorrect= vAnswers == sCorrect

    lAC= answer_select(vAnswers, vCorrect, iAlt=iAa, addnone=TRUE, maxcorrect=NULL, mincorrect=iC, alphorder=FALSE)

  return (list(type='mc', q='r1a1', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.a2 <- function() {
    iAa= 12 # Number of answers (excluding 'none-of-the-above' to select)

    #
    # generate the random numbers and question core
    #
    Core <- Q_5step_regression.1.core()
    #
    # formulate the question
    #
    iTab= Core$select$iTab
    sTab= c("restricted", "unrestricted")[iTab]

    QuestionText <- c(
      Core$setting$Intro,
      Core$setting$Variables,
      Core$setting$LM01,
      ColorBold(sprintf("What descriptions of variables belong to the model description for the **%s** model? Select *all* that apply.", sTab))
    )

    vI= Core$setting$X0
    if (iTab == 2)
      vI= Core$setting$X1
    # asVar=Core$setting$Vars; units= Core$setting$Units; var='x'; coef=''
    vAnswers= Q_descvar(Core$setting$Vars, Core$setting$Units, var='x')
    vICorrect= c(1, vI+2)   # Remember indices of correct answers: Y and the x's

    # Throw in wrong answers, without repeating the answer for y
    vAnswers= c(vAnswers, Q_descvar(Core$setting$Vars, var='b', coef='Estimated coefficient of')[c(-1, -2)], '$e$: residuals', '$\\epsilon$: disturbance term')

    iA= length(vAnswers)
    if (length(unique(vAnswers)) != iA)
      warning("Length of Answer incorrect in a2, some duplicate?")

    vCorrect= rep(FALSE, iA)
    vCorrect[vICorrect]= TRUE

    iC= length(vICorrect)   # Minimal number of correct answers: Do include ALL correct answers
    lAC= answer_select(vAnswers, vCorrect, iAlt=iAa, addnone=FALSE, maxcorrect=NULL, mincorrect=iC, alphorder=FALSE)

  return (list(type='ma', q='r1a2', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.a3 <- function() {
    iDigits= 3

    #
    # generate the random numbers and question core
    #
    Core <- Q_5step_regression.1.core()
    #
    # formulate the question
    #
    iTab= Core$select$iTab
    sTab= Core$select$sTab
    # lYX= Core$setting$YX
    lVars= Core$setting$Vars

    iN= Core$setting$N
    j= sample(1:iN, 1)
    vYX= data.matrix(Core$sample)[j,]
    for (i in 1:length(vYX))
      vYX[i]= round(vYX[i], digits= c(0, 0, 0, 0, 2, 2, 2)[i])
    mM= summary(Core$results$lm0)$coefficients
    vI= Core$setting$X0
    if (iTab == 2){
      mM= summary(Core$results$lm1)$coefficients
      vI= Core$setting$X1
    }
    vB= round(mM[,"Estimate"], 3)

    QuestionText <- c(
      Core$setting$Intro,
      Core$setting$Variables,
      Core$setting$LMsep[iTab],
      ColorBold(sprintf("Use the output for the **%s** model to predict the %s of a %s, given that %s=%g, %s= %g, %s= %g, %s= %g, and %s= %g.", sTab, tolower(lVars[1]), Core$setting$Object,
        lVars[3], vYX[3], lVars[4], vYX[4], lVars[5], vYX[5], lVars[6], vYX[6], lVars[7], vYX[7])),
      sprintf("Give your answer with a precision of %d decimals after the decimal point.", iDigits)
    )

    dYPred= (vYX[c(2, 2+vI)] %*% vB)[1,1]
    #
    # Round the answers
    #
    # Be friendly
    vAnswers <- RoundAnswer(dYPred, iDigits-1)

    return (list(type='num', q='r1a3', text=QuestionText, answer=vAnswers))
}

# Q_5step_regression.1.b1 <- function() {
#     #
#     # generate the random numbers and question core
#     #
#     Core <- Q_5step_regression.1.core()
#
#     warning("Do not use this year... relevance/significance")
#     #
#     # formulate the question
#     #
#     iDigits= 3
#     i0= 4
#     iSP= sample(1:2, 1)
#     sSP= c("statistical significance", "practical relevance")[iSP]
#
#     iMod= sample(1:2, 1)
#     sMod= c("restricted model", "unrestricted model")[iMod]
#     sQ= sprintf("What numeric value(s) describe(s) the %s of the %s?", sSP, sMod)
#
#     QuestionText <- c(
#       "In the following questions, the numbers and setting may
#       change, though questions are related. Use the numbers of the *question on screen*
#       to answer the question.",
#       Core$setting$Intro,
#       sQ,
#       #DigitDecimalWarning(iDigits)
#       "(Select *ALL* answers that apply)"
#     )
#
#     s0= summary(Core$Est$LM0)
#     s1= summary(Core$Est$LM1)
#     p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
#     p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)
#     r20= s0$r.squared
#     r21= s1$r.squared
#     ar20= s0$adj.r.squared
#     ar21= s1$adj.r.squared
#
#     sP0= myprettypvf(s0$fstatistic)
#     sP1= myprettypvf(s1$fstatistic)
#
#     # print (sprintf("M0: F= %g, p-value=%s, r2= %g, ar2= %.4g", s0$fstatistic[1], sP0, r20, ar20))
#     # print (sprintf("M1: F= %g, p-value=%s, r2= %g, ar2= %.4g", s1$fstatistic[1], sP1, r21, ar21))
#
#     #
#     # Prepare the answers
#     #
#     Answers= c(FALSE,                          sprintf("%.2f", s0$sigma),
#                FALSE,                          sprintf("%.2f", s1$sigma),
#                FALSE,                          sprintf("%i", s0$df[2]),
#                FALSE,                          sprintf("%i", s1$df[2]),
#                FALSE,                          sprintf("%i", s0$fstatistic[2]),
#                FALSE,                          sprintf("%i", s1$fstatistic[2]),
#                FALSE,                          sprintf("%i", s0$fstatistic[3]),
#                FALSE,                          sprintf("%i", s1$fstatistic[3]),
#                (iMod == 1) & (iSP == 1),       sprintf("%.3f", s0$fstatistic[1]),
#                (iMod == 2) & (iSP == 1),       sprintf("%.3f", s1$fstatistic[1]),
#                (iMod == 1) & (iSP == 1),       sP0,
#                (iMod == 2) & (iSP == 1),       sP1,
#                (iMod == 1) & (iSP == 2),       sprintf("%.4g", r20),
#                (iMod == 2) & (iSP == 2),       sprintf("%.4g", r21),
#                (iMod == 1) & (iSP == 2),       sprintf("%.4g", ar20),
#                (iMod == 2) & (iSP == 2),       sprintf("%.4g", ar21)
#               )
#     Answers <- matrix(Answers, nrow = 2)
#     iA= ncol(Answers)
#
#     # Find indices of non-duplicated answers
#     vI= !duplicated(Answers[2,])
#     Answers= Answers[,vI]   # Get rid of duplicates
#
#     vI= as.logical(Answers[1,])
#     vI0= which(!vI)           # Check what answers are considered WRONG
#     vI1= which(vI)            # Check what answers are considered RIGHT (to some extend at least)
#
#     # Select some wrong answers
#     vI= sample(vI0, i0)
#     # Reshuffle wrong + right answers
#     vI= sample(c(vI, vI1))
#     vAnswers= Answers[2,vI]
#     vCorrect= as.logical(Answers[1,vI])
#     #
#     # pose the question
#     #
#     # Counters <- MAquestion(QuizFile, "3b2", QuestionText,
#     #                        AnswersSel[2,], as.logical(AnswersSel[1,]), Counters, texify=TRUE)
#   }
#
#   return (list(type='skip', q='r1b1', text=QuestionText, answer=vAnswers, correct=vCorrect))
#   # return(Counters)
# }

Q_5step_regression.1.c1 <- function() {
    #
    # generate the random numbers and question core
    #
    Core <- Q_5step_regression.1.core()

    #
    # formulate the question
    #
    iAa= 8

    iTab= Core$select$iTab    # Restricted/unrestricted
    sTab= Core$select$sTab
    iLim= Core$select$iLim
    sLim= Core$select$sLim
    iH= sample(1:2, 1)
    sH= c("null hypothesis", "alternative hypothesis")[iH]

    mM= summary(Core$results$lm0)$coefficients
    vI= Core$setting$X0
    if (iTab == 2){
      mM= summary(Core$results$lm1)$coefficients
      vI= Core$setting$X1
    }
    i= length(Core$setting$Vars)
    sVar= (Core$setting$VarsSh)[i]
    vB= round(mM[,"Estimate"], 3)
    vS= mM[,"Std. Error"]

    dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

    sQ1= sprintf("%s claims that every extra %s will increase the %s with %s %.1f.", Core$setting$Person, Core$setting$Extra, Core$setting$Vars[1], sLim, dB10)
    if (dB10 < 0){
      sQ1= sprintf("%s claims that every extra %s will decrease the %s with %s %.1f.", Core$setting$Person, Core$setting$Extra, Core$setting$Vars[1], Core$select$sLimNeg, -dB10)
    }
    sQ2= sprintf("Can %s's claim be proven by the results of the %s model?",  Core$setting$Person, sTab)

    sH0= sprintf("$H_0: \\beta_%i %s %.1f$", (i-2), c("\\ge", "=", "\\le")[iLim], dB10)
    sH1= sprintf("$H_1: \\beta_%i %s %.1f$", (i-2), c("<", "\\not=", ">")[iLim], dB10)
    sHCorrect= c(sH0, sH1)[iH]

    QuestionText <- c(
      Core$setting$Intro,
      Core$setting$Variables,
      Core$setting$LMsep[iTab],
      sQ1, sQ2,
      ColorBold(sprintf("What is the relevant **%s**?", sH))
    )

    #
    # Prepare the answers
    #
    asCmp= list(c("=", "\\ge", "\\le"), c("\\not=", ">", "<"))
    vAnswers= sHCorrect
    for (iH in 0:1){
      for (sC in asCmp[[iH+1]]){
        for (j in 1:3){
          b= sample(0:(i-2), 1)
          sHi= sprintf("$H_%i: \\beta_%i %s %.1f$", iH, b, sC, dB10)
          vAnswers= c(vAnswers, sHi)
        }
      }
    }
    vAnswers= unique(vAnswers)
    vCorrect= vAnswers == sHCorrect

    lAC= answer_select(vAnswers, vCorrect, iAlt=iAa, addnone=TRUE, maxcorrect=NULL, mincorrect=1, alphorder=FALSE)

  return (list(type='mc', q='3c1', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}


Q_5step_regression.1.c2 <- function() {
  #
  # generate the random numbers and question core
  #
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 8

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim
  mM= summary(Core$results$lm0)$coefficients
  vI= Core$setting$X0
  if (iTab == 2){
    mM= summary(Core$results$lm1)$coefficients
    vI= Core$setting$X1
  }
  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]
  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)

  sQ1= sprintf("%s claims that every extra %s will increase the %s with %s %.1f.", Core$setting$Person, Core$setting$Extra, Core$setting$Vars[1], sLim, dB10)
  if (dB10 < 0){
    sQ1= sprintf("%s claims that every extra %s will decrease the %s with %s %.1f.", Core$setting$Person, Core$setting$Extra, Core$setting$Vars[1], Core$select$sLimNeg, -dB10)
  }
  sQ2= sprintf("To try and prove her point, %s wants to test the null hypothesis %s against the alternative %s, in the **%s** model. ", Core$setting$Person, sH0, sH1, sTab)

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$Variables,
    Core$setting$LMsep[iTab],
    sQ1, sQ2,
    ColorBold("What is the corresponding test statistic?")
  )

  #
  # Prepare the answers
  #
  sCorrect= sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB)

  vAnswers= list()
  for (sE in c("\\beta", "b")){
      # vAnswers= c(vAnswers, sprintf("$%s_%i$", sE, iB))    # Confusing
      vAnswers= c(vAnswers, sprintf("$t= (%s_%i - %g)/s_{%s_%i}$", sE, iB, dB10, sE, iB))
      vAnswers= c(vAnswers, sprintf("$t= %s_%i/s_{%s_%i}$", sE, iB, sE, iB))
      vAnswers= c(vAnswers, sprintf("$t= (%s_%i - %g)/s^2_{%s_%i}$", sE, iB, dB10, sE, iB))
      vAnswers= c(vAnswers, sprintf("$t= %s_%i/s^2_{%s_%i}$", sE, iB, sE, iB))
  }
  vAnswers= c(vAnswers, "$F$-ANOVA", "$F$-Levene", "$t= \\frac{\\overline x - \\mu}{s/\\sqrt{n}}$")

  # Adapt double minusses
  sCorrect= Q_doubleminus(sCorrect)
  vAnswers= Q_doubleminus(unique(vAnswers))

  vCorrect= vAnswers == sCorrect
  if (sum(vCorrect) != 1)
    warning("Cannot find the correct answer...")

  # CORRECTED CODE?
  # Select iAa out of iA answers, ordered randomly, including all correct ones
  lAC= answer_select(vAnswers, vCorrect, iAa, addnone=TRUE, mincorrect=1, maxcorrect= 1)

  return (list(type='ma', q='r1c2', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.c3 <- function() {
  #
  # generate the random numbers and question core
  #
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 4

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim
  mM= summary(Core$results$lm0)$coefficients
  vI= Core$setting$X0
  if (iTab == 2){
    mM= summary(Core$results$lm1)$coefficients
    vI= Core$setting$X1
  }
  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]
  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))

  sQ1= sprintf("%s wants to test the null hypothesis %s in the %s model, with test statistic %s.", Core$setting$Person, sH0, sTab, sTest)
  sQ2= "In this case, the test should reject:"

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$Variables,
    Core$setting$LMsep[iTab],
    sQ1,
    ColorBold(sQ2)
  )

  #
  # Prepare the answers
  #
  vAnswers= c("on the left side", "on the left and right side", "on the right side", "in the middle",
    "never", "always", "cannot tell given the information")
  sCorrect= vAnswers[iLim]
  vCorrect= vAnswers == sCorrect

  # Select iAa out of iA answers, ordered randomly, including all correct ones
  lAC= answer_select(vAnswers, vCorrect, iAa, addnone=TRUE, mincorrect=1, maxcorrect= 1)

  return (list(type='ma', q='r1c3', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.c4 <- function() {
  #
  # generate the random numbers and question core
  #
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 8

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim

  iN= Core$setting$N

  lmi= Core$results$lm0
  vI= Core$setting$X0
  if (iTab == 2){
    lmi= Core$results$lm1
    vI= Core$setting$X1
  }
  mM= summary(lmi)$coefficients
  iDf= lmi$df.residual

  s0= summary(Core$results$lm0)
  s1= summary(Core$results$lm1)
  # p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
  # p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)

  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]

  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))

  sQ1= sprintf("%s wants to test the null hypothesis %s in the %s model, with test statistic %s.", Core$setting$Person, sH0, sTab, sTest)
  sQ2= "In this case, if the null hypothesis is true, the test statistic is distributed as:"

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$LMsep[iTab],
    sQ1,
    ColorBold(sQ2)
  )
  #
  # Prepare the answers
  #
  sCorrect= sprintf("a $t_{%i}$ distribution", iDf)
  vAnswers= c("a standard Normal distribution",
              sprintf("an F(%i,%i) distribution", s0$fstatistic[2], s0$fstatistic[3]),
              sprintf("an F(%i,%i) distribution", s1$fstatistic[2], s1$fstatistic[3])
             )
  for (df in c(iDf-1, iDf, iDf+1, iN)){
     vAnswers= c(vAnswers,
                 sprintf("a $t_{%i}$ distribution", df),
                 sprintf("a $\\chi^2_{%i}$ distribution", df)
                )
  }
  vCorrect= vAnswers == sCorrect

  # Select iAa out of iA answers, ordered randomly, including all correct ones
  lAC= answer_select(vAnswers, vCorrect, iAa, addnone=TRUE, mincorrect=1, maxcorrect= 1)

  return (list(type='mc', q='r1c4', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.c5 <- function() {
  ########### Skip this question: Questionable what the answer should be
  #
  # generate the random numbers and question core
  #
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 8

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim

  iN= Core$setting$N

  lmi= Core$results$lm0
  vI= Core$setting$X0
  if (iTab == 2){
    lmi= Core$results$lm1
    vI= Core$setting$X1
  }
  mM= summary(lmi)$coefficients
  iDf= lmi$df.residual

  s0= summary(Core$results$lm0)
  s1= summary(Core$results$lm1)
  # p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
  # p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)

  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]

  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))

  sQ1= sprintf("%s wants to test the null hypothesis %s in the %s model, with test statistic %s.", Core$setting$Person, sH0, sTab, sTest)
  sQ2= "This test statistic will follow a Student-$t$ distribution if the following assumptions hold (select *ALL* which are relevant):"

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$LM01,
    sQ1,
    ColorBold(sQ2)
  )

  #
  # Prepare the answers
  #
  vCorrectAnswers= c(
                      "The error term is normally distributed",
                      "The model is well specified",
                      "The number of observations is at least 30"
                     )
  vAnswers= c(
              sprintf("The variable $x_%i=$ %s is normally distributed", iB, Core$setting$VarsSh[i]),
              sprintf("The variable $y=$ %s is normally distributed", Core$setting$VarsSh[1]),
              "The error term is normally distributed",
              "The error term is Student-$t$ distributed",
              "The parameters in the model are normally distributed",
              "The model is well specified",
              "The number of observations is at least 30",
              "The number of observations is at least 15",
              "The number of regressors is at least 30",
              "The number of regressors is at least 15"
             )
  iA= length(vAnswers)
  vCorrect= Q_detect(vAnswers, vCorrectAnswers)

  # Select iAa out of iA answers, ordered randomly, including all correct ones
  lAC= answer_select(vAnswers, vCorrect, iAa, addnone=FALSE, mincorrect=sum(vCorrect))

  return (list(type='skip', q='r1c5', text=QuestionText, answer=lAC$answer, correct=lAC$correct))
}

Q_5step_regression.1.c6 <- function() {
  #
  # generate the random numbers and question core
  #
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 8
  iDigits= 3

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim

  iN= Core$setting$N

  lmi= Core$results$lm0
  vI= Core$setting$X0
  if (iTab == 2){
    lmi= Core$results$lm1
    vI= Core$setting$X1
  }
  mM= summary(lmi)$coefficients
  iDf= lmi$df.residual

  s0= summary(Core$results$lm0)
  s1= summary(Core$results$lm1)
  # p0= pf(s0$fstatistic[1], s0$fstatistic[2], s0$fstatistic[3], lower.tail=FALSE)
  # p1= pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], lower.tail=FALSE)

  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]

  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))
  dTest= (round(vB[sVar],3) - dB10)/round(vS[sVar], 3)

  sQ1= sprintf("%s wants to test the null hypothesis %s in the %s model, with test statistic %s.", Core$setting$Person, sH0, sTab, sTest)
  sQ2= sprintf("What is the value of the test statistic $t_{calc}$ in the %s model?", sTab)

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$LMsep[iTab],
    sQ1,
    ColorBold(sQ2),
    DigitDecimalWarning(iDigits)
  )

  #
  # Round the answers
  #
  Answers <- RoundAnswer(dTest, iDigits)
  Answers <- matrix(Answers, ncol= 3)

  return (list(type='num', q='r1c6', text=QuestionText, answer=Answers))
}


Q_5step_regression.1.c7 <- function() {
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iAa= 8
  iDigits= 3

  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim

  iN= Core$setting$N
  dAlpha= Core$setting$Alpha

  lmi= Core$results$lm0
  vI= Core$setting$X0
  if (iTab == 2){
    lmi= Core$results$lm1
    vI= Core$setting$X1
  }
  mM= summary(lmi)$coefficients
  iDf= lmi$df.residual

  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]

  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))
  dTest= (round(vB[sVar],3) - dB10)/round(vS[sVar], 3)
  dTest= round(dTest, 3)

  sQ1= sprintf("%s tested the null hypothesis %s in the %s model, and finds %s=%g.", Core$setting$Person, sH0, sTab, sTest, dTest)
  sQ2= sprintf("What is the corresponding critical value, at level $\\alpha=%g$?", dAlpha)
  sQ3= "(If there are two critical values, report the highest one)"

  QuestionText <- c(
    Core$setting$Intro,
    Core$setting$LMsep[iTab],
    sQ1,
    ColorBold(sQ2),
    sQ3,
    DigitDecimalWarning(iDigits)
  )

  #
  # Prepare the answers, using either the df from the table, or the true one.
  #
  vCrit= qt(c(dAlpha, 1-dAlpha/2, 1-dAlpha), lower.tail= TRUE, df=FloorDfT(iDf))
  mAnswers= RoundAnswer(vCrit[iLim], iDigits)

  if (FloorDfT(iDf) != iDf){
    vCrit= qt(c(dAlpha, 1-dAlpha/2, 1-dAlpha), lower.tail= TRUE, df=iDf)
    mAnswers= rbind(mAnswers, RoundAnswer(vCrit[iLim], iDigits))
  }

  return (list(type='num', q='r1c7', text=QuestionText, answer=mAnswers))
  # return(Counters)
}


Q_5step_regression.1.c8 <- function() {
  Core <- Q_5step_regression.1.core()

  #
  # formulate the question
  #
  iTab= Core$select$iTab    # Restricted/unrestricted
  sTab= Core$select$sTab

  iLim= Core$select$iLim
  sLim= Core$select$sLim

  iN= Core$setting$N
  dAlpha= Core$setting$Alpha

  lmi= Core$results$lm0
  vI= Core$setting$X0
  if (iTab == 2){
    lmi= Core$results$lm1
    vI= Core$setting$X1
  }
  mM= summary(lmi)$coefficients
  iDf= lmi$df.residual

  i= length(Core$setting$Vars)      # Choose last parameter
  iB= i-2   # Note that this is x_{i-2}, or beta_{i-2}
  sVar= (Core$setting$VarsSh)[i]
  vB= round(mM[,"Estimate"], 3)
  vS= mM[,"Std. Error"]

  dB10= round(vB[sVar] + sample(-2:2, 1)*vS[sVar], 1)

  sH0= sprintf("$H_0: \\beta_%i %s %.1f$", iB, c("\\ge", "=", "\\le")[iLim], dB10)
  sH1= sprintf("$H_1: \\beta_%i %s %.1f$", iB, c("<", "\\not=", ">")[iLim], dB10)
  sTest= Q_doubleminus(sprintf("$t= (b_%i - %g)/s_{b_%i}$", iB, dB10, iB))
  dTest= (round(vB[sVar],3) - dB10)/round(vS[sVar], 3)
  dTest= round(dTest, 3)

  vCrit= qt(c(dAlpha, 1-dAlpha/2, 1-dAlpha), lower.tail= TRUE, df=iDf)
  dCrit= vCrit[iLim]

  sQ1= sprintf("%s tested the null hypothesis %s in the %s model, and finds %s=%g.", Core$setting$Person, sH0, sTab, sTest, dTest)
  sQ2= sprintf("As critical value, at $\\alpha= %g$, %s  finds $t_{crit}=%.3f$.", dAlpha, Core$setting$Person, dCrit)

  if (iLim == 1){
    bRej= (dTest <= dCrit)
  } else if (iLim == 2){
    sQ2= sprintf("As critical values, at $\\alpha= %g$, %s finds $t_L=%.3f$ and $t_U=%.3f$.", dAlpha, Core$setting$Person, -dCrit, dCrit)
    bRej= abs(dTest) >= dCrit
  } else {
    bRej= (dTest >= dCrit)
  }

  QuestionText <- c(
    "In the following questions, the numbers and setting may
    change, though questions are related. Use the numbers of the *question on screen*
    to answer the question.",
    Core$setting$Intro,
    Core$setting$LMsep[iTab],
    sQ1,
    sQ2,
    ColorBold("What is the correct conclusion?")
  )

  #
  # Prepare the answers
  #
  vAnswers= c("reject", "accept", "do not reject", "None of the above")
  sCorrect= c("do not reject", "reject")[1+bRej]
  vCorrect= vAnswers == sCorrect

  return (list(type='mc', q='r1c8', text=QuestionText, answer=vAnswers, correct=vCorrect))
}
