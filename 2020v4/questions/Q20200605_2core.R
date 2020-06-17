########################################
## Q20200605_2core.R
##
## Purpose:
##   Provide main elements of Q2
##
## Author:
##   Charles Bos
##
## Version:
##   a    first start
##   c    cleaned out...
##
## Date:
##   2020/4/20
########################################

# Imports
library(xlsx)
library("stringi")
library("crayon")
# source ("../../RoundAnswer.R")

########################################
# Q2_gendata()
#
# Purpose:
#   Generate data, fulfilling restrictions that i12 >=5 etc, and |i12 - i11| >= 5
#
# Inputs:
#
# Return value:
#   vSample     list, with sample size in both regions, and 'yes' answers in both regions
Q2_gendata <- function(){
  iSample= sample(seq(40,70, by=5), 1)
  i11= i12= iMin= 0
  while ((abs(i11-i12) < 5) | (iMin < 5)){  # Get results at least 5 apart
    i11= round((runif(1)*.2 + .4)*iSample)
    i12= round((runif(1)*.2 + .4)*iSample)
    iMin= min(i11, i12, iSample-i11, iSample-i12)
  }
  vSample= c(iSample, i11, i12)

  return (vSample)
}

########################################
Q20200605_2core <- function() {
  Pres= list(Topic="outcome of the presidential election",
             Text1= "In order to predict the outcome of the presidential election, a poll is held in the states of %s (%s) and %s (%s). In both states, a random sample of $n_{%s}= n_{%s}= %i$ voters was asked if they would vote for the %s (%s) or the %s (%s) candidate. In %s, $x_{%s,%s}= %i$ voters preferred %s, against $x_{%s,%s}= %i$ who chose %s. In %s however, $x_{%s,%s}=%i$ preferred the %s, whereas $x_{%s,%s}=%i$ had a preference for the %s.",
             Text2= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results show that indeed the %s party is %s popular in %s than in %s?",
             Text2b= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results indeed show a difference in popularity of the %s party between %s and %s?",
             TextPi= "Define $\\pi_{%s,%s}$ and $\\pi_{%s,%s}$ as the probabilities that a voter prefers the %s candidate over the %s candidate in %s, or in the second state, %s, respectively. These parameters are estimated by the fractions $p_{%s,%s}$ and $p_{%s,%s}$ in the sample.",
             TextB= "Likewise, population $(\\beta)$ and sample $(b)$ regression parameters use the same indices.",
             TextX= "Likewise $x_{%s,%s}$ is the number of voters that prefer the %ss in state %s, and $n_{%s}$ the number of people asked for their opinion here, with similar notation for the other preference/state.",
             Zone= "states",
             Zones= c("California", "Florida", "Texas", "Washington", "New York", "Montana"),
             ZonesA= c("C", "Fl", "T", "W", "NY", "M"),
             Choices= c("Independent", "Republican", "Democrat"),
             ChoicesA= c("I", "R", "D")
           )

  Farm= list(Topic="efficacy of crop planting",
             Text1= "In order to predict the efficacy of crop planting, a poll is held among the owners of farms in %s (%s) and %s (%s). In both provinces, a random sample of $n_{%s}= n_{%s}= %i$ individuals were asked if they planned to increase their use of %s (%s), or preferred to use more %s (%s). In %s, $x_{%s,%s}=%i$ preferred %s, against $x_{%s,%s}=%i$ who chose %s. In %s however, $x_{%s,%s}=%i$ preferred the %s, whereas $x_{%s,%s}=%i$ had a preference of using the %s.",
             Text2= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results show that indeed %s is %s popular in %s than in %s?",
             Text2b= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results show that indeed a difference in popularity of %s exists between %s and %s?",
             TextPi= "Define $\\pi_{%s,%s}$ and $\\pi_{%s,%s}$ as the probabilities that a farmer prefers %s over the use of %s in %s, or in the second province, %s, respectively. These parameters are estimated by the fractions $p_{%s,%s}$ and $p_{%s,%s}$ in the sample.",
             TextB= "Likewise, population $(\\beta)$ and sample $(b)$ regression parameters use the same indices.",
             TextX= "Likewise $x_{%s,%s}$ is the number of farmers that prefer using %s in province %s, and $n_{%s}$ the number of farmers asked for their opinion here, with similar notation for the other preference/province.",
             Zone= "farms",
             Zones= c("North Holland", "South Holland", "Noord Brabant", "Zeeland", "Friesland"),
             ZonesA= c("NH", "SH", "NB", "Z", "Fr"),
             Choices = c("fertilizer", "manure"),
             ChoicesA = c("F", "M")
           )
  Stud= list(Topic= "Study-support at the Statistics exam",
             Text1= "In order to predict the efficacy of student learning, a poll is held among the students of Business Administration at the %s (%s) and %s (%s). At both universities, a random sample of $n_{%s}= n_{%s}= %i$ students were asked if they used study-help of %s (%s), or preferred the advice of %s (%s). At the %s, $x_{%s,%s}=%i$ preferred %s, against $x_{%s,%s}=%i$ who chose %s. At the %s however, $x_{%s,%s}=%i$ preferred %s, whereas $x_{%s,%s}=%i$ had a preference for using %s.",
             Text2= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results show that indeed help from %s is %s popular at the %s than at the %s?",
             Text2b= "%s says that the sample sizes were too small to draw any conclusions. On the other hand, %s is quite convinced by the difference. You are now asked to be the referee in this debate. Do these sample results show that indeed there is a difference in popularity of %s between %s and %s?",
             TextPi= "Define $\\pi_{%s,%s}$ and $\\pi_{%s,%s}$ as the probabilities that a student prefers help from %s over the aide of %s at the %s, or at the second university, %s, respectively. These parameters are estimated by the fractions $p_{%s,%s}$ and $p_{%s,%s}$ in the sample.",
             TextB= "Likewise, population $(\\beta)$ and sample $(b)$ regression parameters use the same indices.",
             TextX= "Likewise $x_{%s,%s}$ is the number of students that prefer %s at the %s, and $n_{%s}$ the number of students asked for their opinion here, with similar notation for the other preference/university.",
             Zone= "universities",
             Zones= c("University of Amsterdam", "Vrije Universiteit", "Erasmus University Rotterdam", "Tilburg University"),
             ZonesA= c("UvA", "VU", "EUR", "TU"),
             Choices= c("Sowiso", "Sophistes", "Aureus", "Friends"),
             ChoicesA= c("SW", "SO", "A", "F")
            )

  asPeople= c("Jim", "Joe", "Piet", "Anne", "Floor", "Stijn", "Jackie", "Wouter")

  ######### Extend
  Settings <- list(Pres, Farm, Stud)
  # Choose setting
  # iSet= sample(1:length(Settings), 1)
  # S <- Settings[[iSet]]
  # S <- Settings[[1]]
  S <- sample(Settings, 1)[[1]]

  # Sample size, plus yes-answers per region
  vSample= Q2_gendata()
  iSample= vSample[1]
  i11= vSample[2]
  i12= vSample[3]
  if (i11 > i12){
    sComp= "more"
    bAltLarge= TRUE
  } else {
    sComp= "less"
    bAltLarge= FALSE
  }

  asCmp= list(c("\\ge", "\\le", "="), c("<", ">", "\\not ="))
  asH= c('$H_0$', '$H_1$')

  People= sample(asPeople, 2)
  vI= sample(1:length(S$Zones), 2)
  Zones= S$Zones[vI]
  ZonesA= S$ZonesA[vI]
  vI= sample(1:length(S$Choices), 2)
  Choices= S$Choices[vI]
  ChoicesA= S$ChoicesA[vI]
  dAlpha= sample(c(.1, .05, .01), 1)

  iAlt= as.numeric(bAltLarge)+1
  asHyp= list()
  for (h in 1:2){
    asHyp= c(asHyp, sprintf("$H_%i$: $\\pi_{%s,%s} %s \\pi_{%s,%s}$", (h-1), ChoicesA[1], ZonesA[1], asCmp[[h]][iAlt], ChoicesA[1], ZonesA[2]))
  }

  s= sprintf("{%s,%s}", ChoicesA[1], ZonesA)
  sTest= sprintf("z=\\frac{p_%s-p_%s-(\\pi_%s-\\pi_%s)}{\\sqrt{p_%s(1-p_%s)/n_{%s}+p_%s(1-p_%s)/n_{%s}}}", s[1], s[2], s[1], s[2], s[1], s[1], ZonesA[1], s[2], s[2], ZonesA[2])

  sText1= sprintf(S$Text1, Zones[1], ZonesA[1], Zones[2], ZonesA[2], ZonesA[1], ZonesA[2], iSample, Choices[1], ChoicesA[1], Choices[2], ChoicesA[2], Zones[1], ChoicesA[1], ZonesA[1], i11, Choices[1], ChoicesA[2], ZonesA[1], (iSample-i11), Choices[2], Zones[2], ChoicesA[1], ZonesA[2], i12, Choices[1], ChoicesA[2], ZonesA[2], (iSample-i12), Choices[2])

  # sText2= sprintf(S$Text2, People[1], People[2], People[2], Choices[1], sComp, Zones[1], Zones[2], Choices[1], sComp, Zones[1], Zones[2])
  sText2= sprintf(S$Text2, People[1], People[2], Choices[1], sComp, Zones[1], Zones[2])
  sText2b= sprintf(S$Text2b, People[1], People[2], Choices[1], Zones[1], Zones[2])

  sTextPi= sprintf(S$TextPi, ChoicesA[1], ZonesA[1], ChoicesA[1], ZonesA[2], Choices[1], Choices[2], Zones[1], Zones[2], ChoicesA[1], ZonesA[1], ChoicesA[1], ZonesA[2])
  # sTextB= sprintf(S$TextB, ChoicesA[1], ZonesA[1], Choices[1], Zones[1], ChoicesA[1], ZonesA[1], ZonesA[2], ChoicesA[1], ZonesA[1], ChoicesA[1], ZonesA[2])
  sTextB= S$TextB
  # sTextX= sprintf(S$TextX, ChoicesA[1], ZonesA[1], Choices[1], Zones[1], ZonesA[1])
  sTextX= ""
  sTextS= sprintf("The standard deviations are indicated by $\\sigma_{%s,%s}, \\sigma_{%s,%s}$, and their estimates by $s_{%s,%s}, s_{%s,%s}$.", ChoicesA[1], ZonesA[1], ChoicesA[1], ZonesA[2], ChoicesA[1], ZonesA[1], ChoicesA[1], ZonesA[2])

  sIntro= sText1

  ### Calculate answer, one-sided, non-equal variances
  dp1= i11/iSample
  dp2= i12/iSample
  dS2z= dp1*(1-dp1)/iSample + dp2*(1-dp2)/iSample
  dZ= (dp1 - dp2)/sqrt(dS2z)
  if (i11 > i12){
    dPval= 1-pnorm(dZ)
    dZcrit= qnorm(1-dAlpha)
  } else {
    dPval= pnorm(dZ)
    dZcrit= qnorm(dAlpha)
  }
  # dBound= (dp1 - dp2) + dZcrit*sqrt(dS2z)

  # Two-sided CI
  dZcrit2= qnorm(1-dAlpha/2)
  vBound= c((dp1 - dp2) - dZcrit2*sqrt(dS2z), (dp1 - dp2) + dZcrit2*sqrt(dS2z))

  # print (sIntro)
  lCore= list(Intro=sIntro, TextPi=sTextPi, TextB=sTextB,
              TextX=sTextX, TextS=sTextS, Comp=sText2, CompB=sText2b, People=People, Test=sTest, Sel=c(s[1], s[2]),
              Zones=Zones, Choices=Choices, ZonesA=ZonesA, ChoicesA=ChoicesA,
              Sample=vSample, AltLarge=bAltLarge, Hyp=asHyp, p1=dp1, p2=dp2, s2z= dS2z, zcalc=dZ, pval=dPval, alpha=dAlpha, zcrit=dZcrit, bound=vBound)
  return (lCore)
}


#############################################################################
# Qselect(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
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

#############################################################################
# Q= Q_2a1()
# Purpose:
#   Prepare single question for 2a1, what is the correct H0/H1
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a1 <- function(){

  iAlt= 6         # Number of alternatives
  lCore= Q20200605_2core()
  sType= 'mc'
  sq= '2a1'

  iH= sample(1:2, 1)  # H0/H1
  iAltLarge= as.numeric(lCore$AltLarge)+1
  # sH= c('$H_0$', '$H_1$')[iH]
  sQ1= sprintf("What is the $H_%i$ hypothesis for this situation?", (iH-1))

  asCmp= list(c("\\ge", "\\le", "="), c("<", ">", "\\not ="))
  asPars= c("\\beta", "\\pi", "p", "b")
  sHypCorrect= lCore$Hyp[iH]
  sHypCorrectShort= sprintf("$%s_{%s,%s} %s %s_{%s,%s}$", '\\pi', lCore$ChoicesA[1], lCore$ZonesA[1], asCmp[[iH]][iAltLarge], '\\pi', lCore$ChoicesA[1], lCore$ZonesA[2])

  vAnswers= list()
  for (h in 1:2){
    for (sPar in asPars){
      for (sCmp in asCmp[[h]]){
        sAnswer= sprintf("$%s_{%s,%s} %s %s_{%s,%s}$", sPar, lCore$ChoicesA[1], lCore$ZonesA[1], sCmp, sPar, lCore$ChoicesA[1], lCore$ZonesA[2])
        vAnswers= c(vAnswers, sAnswer)
      }
    }
  }
  vCorrect= vAnswers == sHypCorrectShort
  lAC= Qselect(vAnswers, vCorrect, iAlt, addnone=TRUE, mincorrect=1)

  sText= c(lCore$Intro, lCore$TextPi, lCore$TextB, lCore$Comp, sQ1)
  return (list(type=sType, q=sq, text=sText, answer=lAC$answer, correct=lAC$correct))
}

#############################################################################
# Q= Q_2a2()
# Purpose:
#   Prepare single question for 2a2, what is the test statistic
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a2 <- function(){

  lCore= Q20200605_2core()
  sType= 'mc'
  iAlt= 6
  sq= '2a2'

  asRej= c('small', 'large', 'small and large')
  iAltLarge= as.numeric(lCore$AltLarge)+1
  sRej= asRej[iAltLarge]

  s= sprintf("{%s,%s}", lCore$ChoicesA[1], lCore$ZonesA)

  asTest= list()
  asTest= c(asTest, sprintf("z=\\frac{p_%s-p_%s-(\\pi_%s-\\pi_%s)}{\\sqrt{p_%s(1-p_%s)/n_{%s}+p_%s(1-p_%s)/n_{%s}}}", s[1], s[2], s[1], s[2], s[1], s[1], lCore$ZonesA[1], s[2], s[2], lCore$ZonesA[2]))
  for (i in 1:2){
    asTest= c(asTest, sprintf("z=\\frac{p-\\pi_%s}{\\sqrt{\\pi_%s(1-\\pi_%s)/n_{%s}}}", s[i], s[i], s[i], lCore$ZonesA[i]))
  }
  asTest= c(asTest, sprintf("t=\\frac{\\overline x_%s-\\overline x_%s-(\\mu_%s-\\mu_%s)}{\\sqrt{s_%s^2/n_{%s}+s_%s^2/n_{%s}}}", s[1], s[2], s[1], s[2], s[1], lCore$ZonesA[1], s[2], lCore$ZonesA[2]))
  asTest= c(asTest, sprintf("F= \\frac{s_%s^2}{s_%s^2}", s[1], s[2]))

  sCorrect= sprintf("Test statistic: $%s$, reject for %s values", asTest[1], sRej)

  vAnswers= list()
  for (sRej in asRej){
    for (sTest in asTest){
        sAnswer= sprintf("Test statistic: $%s$, reject for %s values", sTest, sRej)
        vAnswers= c(vAnswers, sAnswer)
      }
    }
  vCorrect= vAnswers == sCorrect

  sQ1= sprintf('If you want to test %s against %s, what would be your test statistic and rejection region?', lCore$Hyp[1], lCore$Hyp[2])
  lAC= Qselect(vAnswers, vCorrect, iAlt, addnone=TRUE, mincorrect=1)

  # sQ2= sprintf('*** Triple check the question: Is the hypothesis indeed supposed to be "%s"?', sCorrect)

  sText= c(lCore$Intro, lCore$TextPi, lCore$TextX, lCore$Comp, sQ1)
  return (list(type=sType, q=sq, text=sText, answer=lAC$answer, correct=lAC$correct))
}

#############################################################################
# Q= Q_2a3()
# Purpose:
#   Prepare single question for 2a3, what is the distribution of the test statistic
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a3 <- function(){

  lCore= Q20200605_2core()
  sType= 'mc'
  iAlt= 6
  sq= '2a3'

  asRej= c('small', 'large', 'small and large')
  iAltLarge= as.numeric(lCore$AltLarge)+1
  sRej= asRej[iAltLarge]
  iSample= lCore$Sample[1]

  sQ1= sprintf('We test %s, using test statistic $%s$, and reject for %s values', lCore$Hyp[1], stri_sub(lCore$Test, 3), sRej)
  sQ2= sprintf('If the null hypothesis is true, and the necessary assumptions fulfilled, the test statistic will be distributed as:')
  vAnswers= "N(0,1)"
  vDf= c(iSample-1, iSample, 2*iSample-2, 2*iSample)
  for (iDf in vDf){
    vAnswers= c(vAnswers, sprintf("$t_{%i}$", iDf), sprintf("$\\chi^2_{%i}$", iDf))
  }
  vCorrect= vAnswers == "N(0,1)"

  sText= c(lCore$Intro, lCore$TextPi, lCore$Comp, sQ1, sQ2)
  lAC= Qselect(vAnswers, vCorrect, iAlt, addnone=TRUE, mincorrect=0)

  return (list(type=sType, q=sq, text=sText, answer=lAC$answer, correct=lAC$correct))
}

#############################################################################
# Q= Q_2a4()
# Purpose:
#   Prepare single question for 2a4, assumptions
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a4 <- function(){

  lCore= Q20200605_2core()
  sType= 'ma'
  iAlt= 6
  sq= '2a3'

  asRej= c('small', 'large', 'small and large')
  iAltLarge= as.numeric(lCore$AltLarge)+1
  sRej= asRej[iAltLarge]
  iSample= lCore$Sample[1]

  sQ1= sprintf('We test %s, using test statistic $%s$, and reject for %s values', lCore$Hyp[1], lCore$Test, sRej)
  sQ2= sprintf('If the null hypothesis is true, the test statistic is distributed as a N(0,1), if the following assumptions hold:')
  sQ3= sprintf('(select *ALL* options that apply; in the notation, an $i$ stands for either %s or %s, and $j$ for %s or %s, so $i,j$ stands for all possible combinations of these)', lCore$ChoicesA[1], lCore$ChoicesA[2], lCore$ZonesA[1], lCore$ZonesA[2])

  sText= c(lCore$Intro, lCore$TextPi, lCore$TextX, lCore$Comp, sQ1, sQ2, sQ3)

  vAnswers= c("All $x_{i,j} \\ge 10$",    # True
              "All $\\pi_{i,j} \\ge 5$",
              "All $n_{j} \\ge 5$",
              "All $n_{j} \\ge 10$",
              "All $n_{j} \\ge 30$",
              "All $x$ are normally distributed",
              "All $x$ are symmetric in distribution",
              "All $p$ are normally distributed",
              "All $n$ are normally distributed",
              "The model is well specified")
  vCorrect= c(TRUE, rep(FALSE, length(vAnswers)-1))

  lAC= Qselect(vAnswers, vCorrect, iAlt, addnone=TRUE, mincorrect=1)

  return (list(type=sType, q=sq, text=sText, answer=lAC$answer, correct=lAC$correct))
}

#############################################################################
# Q= Q_2a5()
# Purpose:
#   Prepare single question for 2a5, get zCalc
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a5 <- function(){

  lCore= Q20200605_2core()
  sType= 'num'
  iDigit= 3
  sq= '2a5'

  sQ1= sprintf('We test %s, using test statistic $%s$.', lCore$Hyp[1], lCore$Test)
  sQ2= sprintf('Calculate the test statistic $z_{calc}$, up to a precision of at least %i digits after the decimal point.', iDigit)

  sText= c(lCore$Intro, lCore$TextPi, lCore$TextX, lCore$Comp, sQ1, sQ2)

  ### Calculate answer
  vCorrect <- RoundAnswer(lCore$zcalc, iDigit)
  vCorrect <- matrix(vCorrect, ncol= 3)

  return (list(type=sType, q=sq, text=sText, correct=vCorrect))
}

#############################################################################
# Q= Q_2a6()
# Purpose:
#   Prepare single question for 2a6, get pVal or zCrit
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a6 <- function(){

  lCore= Q20200605_2core()
  sType= 'num'
  sq= '2a6'
  bZcrit= runif(1) > .5

  sQ1= sprintf('We test %s, using test statistic $%s$, and with $\\alpha=%g$.', lCore$Hyp[1], lCore$Test, lCore$alpha)
  sQ2= sprintf('%s and %s found that $z_{calc}=%.3f$.', lCore$People[1], lCore$People[2], lCore$zcalc)

  if (bZcrit) {
    iDigit= 2
    sQ3= sprintf('What is the critical value of the test? Use a precision of at least %i digits after the decimal point.', iDigit)
    z= lCore$zcrit
    iFac= 10^iDigit
    # Take care with table of Doane. 'Correct' would be to take zCrit more extreme, but one might also round. Allow whatever direction ceil/floor.
    # vCorrect <- c(z, floor(z*iFac)/iFac, ceiling(z*iFac)/iFac)
    # vCorrect <- matrix(vCorrect, ncol= 3)
    vCorrect <- RoundAnswer(z, iDigit)
  } else {
    iDigit= 3
    sQ3= sprintf('What is the $p$-value of the test? Use a precision of at least %i digits after the decimal point.', iDigit)
    vCorrect <- RoundAnswer(lCore$pval, iDigit)
  }
  sText= c(lCore$Intro, lCore$TextPi, lCore$Comp, sQ1, sQ2, sQ3)

  return (list(type=sType, q=sq, text=sText, correct=vCorrect))
}

#############################################################################
# Q= Q_2a7()
# Purpose:
#   Prepare single question for 2a7, conclusion
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2a7 <- function(){

  lCore= Q20200605_2core()
  sType= 'mc'
  sq= '2a7'
  iAlt= 8
  bZcrit= runif(1) > .5

  bRej= lCore$pval < lCore$alpha
  iRej= as.numeric(bRej)+1
  iAltLarge= as.numeric(lCore$AltLarge)+1
  asRej= c("Do not reject", "Reject")
  asCmp= list(c("\\ge", "\\le", "="), c("<", ">", "\\not ="))

  sQ1= sprintf('We test %s, using test statistic $%s$, and with $\\alpha=%g$.', lCore$Hyp[1], lCore$Test, lCore$alpha)

  sQ2z= sprintf('%s and %s found that $z_{calc}=%.3f$, and $z_{crit}= %.3f$.', lCore$People[1], lCore$People[2], lCore$zcalc, lCore$zcrit)
  ss= asCmp[[3-iRej]][3-iAltLarge]
  sCorrectZ= sprintf("%s $H_0$, as $%s %s %s$", asRej[iRej], "z_{calc}", ss, "z_{crit}")
  sCorrectCheckZ= sprintf("%s %s, as $%s=%g %s %g=%s$", asRej[iRej], lCore$Hyp[1], "z_{calc}", lCore$zcalc, ss, lCore$zcrit, "z_{crit}")

  sQ2p= sprintf('%s and %s found that $z_{calc}=%.3f$, with a $p$-value of %.3f.', lCore$People[1], lCore$People[2], lCore$zcalc, lCore$pval)
  ss= c(">", "\\le")[iRej]
  sCorrectP= sprintf("%s $H_0$, as $p$-value $ %s %s$", asRej[iRej], ss, "\\alpha")
  sCorrectCheckP= sprintf("%s %s, as $p$-value= $%g %s %g= %s$", asRej[iRej], lCore$Hyp[1], lCore$pval, ss, lCore$alpha, "\\alpha")

  if (bZcrit) {
    sQ2= sQ2z
    sCorrect= sCorrectZ
    sCorrectCheck= sCorrectCheckZ
    sSkip= sCorrectP
  } else {
    sQ2= sQ2p
    sCorrect= sCorrectP
    sCorrectCheck= sCorrectCheckP
    sSkip= sCorrectZ
  }
  sQ3= sprintf('What is the conclusion they should draw?')
  # sQ4= sprintf("Triple check ***, supposedly correct is '%s'", sCorrectCheck)

  # Get answers
  vAnswers= c()
  for (cc in asRej)
    for (h in 0:1)
      for (rr in c(list(c("p$-value $", "\\alpha"), c("z_{calc}", "z_{crit}"))))
        for (ss in c("<", "\\le", ">", "\\ge")){
          sAnswer= sprintf("%s $H_%i$, as $%s %s %s$", cc, h, rr[1], ss, rr[2])
          if (sAnswer != sSkip)
            vAnswers= c(vAnswers, sAnswer)
        }

  # vAnswers= matrix(vAnswers, ncol=1)
  vCorrect= vAnswers == sCorrect

  sText= c(lCore$Intro, lCore$TextPi, lCore$Comp, sQ1, sQ2, sQ3)
  lAC= Qselect(vAnswers, vCorrect, iAlt, addnone=TRUE, mincorrect=1)

  return (list(type=sType, q=sq, text=sText, answer=lAC$answer, correct=lAC$correct))
}

#############################################################################
# Q= Q_2b1()
# Purpose:
#   Prepare single question for 2b, confidence interval
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2b1_org <- function(){

  lCore= Q20200605_2core()
  sType= 'num'
  sq= '2b1'
  iDigits= 3

  lPar= Q20200605_2par(lCore, 1, par='\\pi', addH=TRUE)
  sH0= lPar$hyp[1]

  sBound= "lower"
  sCorrect=sprintf('$[%.3f, \\infty)$', lCore$bound)
  if (lCore$AltLarge)
    sBound= "upper"
    sCorrect=sprintf('$(\\infty, %.3f]$', lCore$bound)

  sPar= 'p'
  sTest= sprintf("%s_{%s,%s} - %s_{%s,%s}", sPar, lCore$ChoicesA[1], lCore$ZonesA[1], sPar, lCore$ChoicesA[1], lCore$ZonesA[2])
  # sTestStand= sprintf("$z= \\frac{%s}{\\sqrt{\\frac {\\overline p (1-\\overline p)}{n_{%s}} + \\frac {\\overline p (1-\\overline p)}{n_{%s}}}}$", sTest, lCore$ZonesA[1], lCore$ZonesA[2])
  sTestStand= sprintf("$z= (%s) / \\sqrt{\\frac {\\overline p (1-\\overline p)}{n_{%s}} + \\frac {\\overline p (1-\\overline p)}{n_{%s}}}$", sTest, lCore$ZonesA[1], lCore$ZonesA[2])
  sP= sprintf("$\\overline p= \\frac {x_{%s,%s} + x_{%s,%s}}{n_{%s} + n_{%s}}$", lCore$ChoicesA[1], lCore$ZonesA[1], lCore$ChoicesA[1], lCore$ZonesA[2], lCore$ZonesA[1], lCore$ZonesA[2])

  sQ1= sprintf('We test %s, using the difference $d_p= %s= %.3f$, with $\\alpha=%g$.', sH0, sTest, lCore$p1-lCore$p2, lCore$alpha)
  sQ2= sprintf('This difference is found to have variance $\\sigma^2_d= var(d_p)= \\frac {\\overline p (1-\\overline p)}{n_{%s}} + \\frac {\\overline p (1-\\overline p)}{n_{%s}}= %.6g$', lCore$ZonesA[1], lCore$ZonesA[2], lCore$s2z)
  sQ3= sprintf('Give the %s bound of the %g%% confidence interval, up to %i digits after the decimal point.', sBound, 100*(1-lCore$alpha), iDigits)

  sQ4= sprintf('*** Triple check the question: Is the CI indeed supposed to be %s?', sCorrect)

  ### Get answer
  vCorrect <- RoundAnswer(lCore$bound, iDigits)
  vCorrect <- matrix(vCorrect, ncol= 3)

  sText= c(lCore$Intro, lCore$Comp, sQ1, sQ2, sQ3, sQ4)

  return (list(type=sType, q=sq, text=sText, correct=vCorrect))
}

#############################################################################
# Q= Q_2b1()
# Purpose:
#   Prepare single question for 2b, two-sided confidence interval, hence starting with H_0: pi1=pi2. This still does not imply that one needs to use piAvg, as the confidence interval still assumes there may be a difference.
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_2b1 <- function(){

  lCore= Q20200605_2core()
  sType= 'num'
  sq= '2b1'
  iDigits= 3

  # Random lower/upper
  iBound= sample(1:2, 1)

  dS2z= round(lCore$s2z, 6)   # Fix precision in s2z
  vBounds= (lCore$p1 - lCore$p2) + c(-1, 1)*qnorm(1-lCore$alpha/2)*sqrt(dS2z)

  asBounds= c("lower", "upper")
  s= sprintf("{%s,%s}", lCore$ChoicesA[1], lCore$ZonesA)
  sH0= sprintf('$H_0: \\pi_%s = \\pi_%s$', s[1], s[2])

  sQ1= sprintf('Instead of testing %s, using the test statistic $%s$, %s and %s now would like to see a two-sided confidence interval for the difference $d= \\pi_%s - \\pi_%s$.', sH0, lCore$Test, lCore$People[1], lCore$People[2], s[1], s[2])
  sQ2= sprintf('For this purpose, they calculated the variance used in the test statistic $\\sigma^2_d= p_%s(1-p_%s)/n_{%s}+p_%s(1-p_%s)/n_{%s}= %f$', s[1], s[1], lCore$ZonesA[1], s[2], s[2], lCore$ZonesA[2], dS2z)
  sQ3= sprintf('Give the %s bound of the %g%% **two-sided** confidence interval, up to %i digits after the decimal point.', asBounds[iBound], 100*(1-lCore$alpha), iDigits)
  # sQ4= sprintf('*** Triple check the question: Is the CI indeed supposed to be [%g, %g]?', vBounds[1], vBounds[2])

  ### Get answer
  vCorrect <- RoundAnswer(vBounds[iBound], iDigits)

  sText= c(lCore$Intro, lCore$CompB, sQ1, sQ2, sQ3)

  return (list(type=sType, q=sq, text=sText, correct=vCorrect))
}
