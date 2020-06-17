###############################
###############################
## reset
###############################
###############################
rm(list = ls())

###############################
###############################
## load R libraries (required)
###############################
###############################
library("stringi")
library("crayon")

###############################
###############################
## load questions code
###############################
###############################
source("questions/Q20200612_all.R")
source("../aidfunctions.R")
source("../WriteQuizCanvas.R")
source("../WriteQuizDataframe.R")


###############################
###############################
## Excuus, moet nog even als 
## global ...
###############################
###############################
# sEmailaddress = "c.s.bos@vu.nl"
sEmailaddress = "a.lucas@vu.nl"
if (!exists("sEmailaddress")) stop("Set the email address as a global first !!")

###############################
main <- function(NumberOfDataFiles = 50) {
  # Magics
  EnginePath = "../"
  ExamDate= "20200612"
  NumberOfVariations= 30
  mySeed= round(1e5 * runif(1))
  set.seed(mySeed)
  TestName= "R Digi Test T12 (=T3) Business Statistics 2020/06/12"
  IntroText = sprintf("This is the R re-take test from 2020/06/12. Once opened, its score will replace both T1 and T2. Use all decimals provided by R. If you need more than one R command, type the answer in the quiz box separating the commands by a semicolon (\";\") like during T1 or T2. Emergency questions during the exam: mail %s.\n Process date: %d.", sEmailaddress, mySeed)
  
  #######################
  #######################
  ## UNCOMMENT FOR TEST QUIZ
  ## FOR STUDENTS (4 Qs)
  #######################
  #######################
  # sLinknamefront = "http://personal.vu.nl/a.lucas/Rtest/D123test.csv"
  # sLinknameback = ""
  # mQ <- rbind(
  #   cbind(c("0a", "0b", "0ctest"), 1),
  #   cbind(c("Selfie", "2ftest"), NumberOfVariations))

  #######################
  #######################
  ## UNCOMMENT FOR REAL VERSION
  ## WITH SELECTED QUESTIONS
  ## TAKE CARE OF NRDATAFILES WITH
  ## CORRECT LINK
  #######################
  #######################
  sLinknamefront = "http://personal.vu.nl/a.lucas/Rtest/D2"
  sLinknameback = "3.csv"
  mQ <- rbind(
    c("0a", 1),
    c("0c", NumberOfDataFiles),
    cbind(c("Selfie", "1a", "1b", "1c", "1d", "2a", "2b", "2d", "2e",
            "3a", "3d"), NumberOfVariations))

  
  ExamQ= list()
  iQuestionCounter= 0
  for (i0 in 1:nrow(mQ)){
    sQ= mQ[i0,1]
    QV= list()
    if (sQ == "0ctest") {
      sQi= sprintf("Qi <- Q_%s(\"%s%s\")", sQ, sLinknamefront, sLinknameback)
      eval(parse(text=sQi))
      Qi[["questionnumber"]]= sprintf("Q%d.%d", iQuestionCounter, j)
      QV= c(QV, list(Qi))
    } else if (sQ == "0c") {
      for (j in 1:as.numeric(mQ[i0,2])) {
        sQi= sprintf("Qi <- Q_%s(\"%s%d%s\")", sQ, sLinknamefront, j+10, sLinknameback)
        eval(parse(text=sQi))
        Qi[["questionnumber"]]= sprintf("Q%d.%d", iQuestionCounter, j)
        QV= c(QV, list(Qi))
      } 
    } else {
      for (j in 1:as.numeric(mQ[i0,2])) {
        sQi= sprintf("Qi <- Q_%s()", sQ)
        eval(parse(text=sQi))
        Qi[["questionnumber"]]= sprintf("Q%d.%d", iQuestionCounter, j)
        QV= c(QV, list(Qi))
      } 
    }
    ExamQ= c(ExamQ, list(QV))
    iQuestionCounter= iQuestionCounter + 1
  }

  WriteQuizCanvas(ExamDate, TestName, IntroText, ExamQ, htmlesc = TRUE)
  tmp= WriteQuizDataframe(ExamQ)
  
  print(sprintf("all is mailed to %s", sEmailaddress))
}


iNumberofdatafiles= 50
# DGP(sprintf("D2%d3.csv", 10 + (1:iNumberofdatafiles)))
main(iNumberofdatafiles)
