###############################
###############################
## Set my working directory
###############################
###############################
# setwd("C:/Users/me/surfdrive/BSTAT/exams/May2020/")
# EnginePath = "/home/cbs310/vu/busstat20/writtenexams/ElectronicMarkingTest"
# getwd()
# setwd("..")
EnginePath = ".."
ExamPath = paste(EnginePath, "2020resitv_1", sep="/")
ExamDate= "20200629"


###############################
###############################
## Set email address for 
## emergency
###############################
###############################
# sEmailaddress = "c.s.bos@vu.nl"
sEmailaddress = "a.lucas@vu.nl"
if (!exists("sEmailaddress")) stop("Set the email address as a global first !!")


###############################
###############################
## load R libraries (required)
###############################
###############################
library("stringi")
library("crayon")
library("car")
library("psych")

###############################
###############################
## Load Canvas Quiz R code
###############################
###############################
source(sprintf("%s/%s",EnginePath,"aidfunctions.R"))
source(sprintf("%s/%s",EnginePath,"WLprint.R"))
source(sprintf("%s/%s",EnginePath,"imsmanifest.R"))
source(sprintf("%s/%s",EnginePath,"assessment_meta.R"))
source(sprintf("%s/%s",EnginePath,"testheader.R"))
source(sprintf("%s/%s",EnginePath,"opennewquizfile.R"))
source(sprintf("%s/%s",EnginePath,"startnewblock.R"))
source(sprintf("%s/%s",EnginePath,"mathformats.R"))
source(sprintf("%s/%s",EnginePath,"MAquestion.R"))
source(sprintf("%s/%s",EnginePath,"MCquestion.R"))
source(sprintf("%s/%s",EnginePath,"ALPHquestion.R"))
source(sprintf("%s/%s",EnginePath,"NUMquestion.R"))
source(sprintf("%s/%s",EnginePath,"UPLOADquestion.R"))
source(sprintf("%s/%s",EnginePath,"closeblock.R"))
source(sprintf("%s/%s",EnginePath,"quizzip.R"))
source(sprintf("%s/%s",EnginePath,"closequizfile.R"))
source(sprintf("%s/%s",EnginePath,"tex2math.R"))
source(sprintf("%s/%s",EnginePath,"RoundAnswer.R"))
source(sprintf("%s/%s",EnginePath,"FloorDfT.R"))
source(sprintf("%s/%s",EnginePath,"WriteQ.R"))
source(sprintf("%s/%s",EnginePath,"Qselect.R"))
#source(sprintf("%s/%s%s%s", ExamPath, "questions/Q", ExamDate, "_0core.R"))
source(sprintf("%s/%s%s%s", ExamPath, "questions/Q", ExamDate, "_1nw-core.R"))
source(sprintf("%s/%s%s%s", ExamPath, "questions/Q", ExamDate, "_2core.R"))
source(sprintf("%s/%s%s%s", ExamPath, "questions/Q", ExamDate, "_3core.R"))
source(sprintf("%s/%s%s", ExamPath, "questions/", "ID_question.R"))

###############################
###############################
## Set my working subdirectory
## to save the quiz archive in.
## Also initialize the counters
## for questions and answers.
###############################
###############################
mydir = sprintf("%s/%s", EnginePath, "tmp")
Counters <- list(AnswerCounter = 100000, QuestionCounter = 1)



###############################
###############################
## Open the new quiz file
###############################
###############################
QuizFile <- OpenNewQuizFile(subdir = mydir,
                            TestName = "TEST Business Statistics 2020/06/29",
#                            TestName = "NORMAL TIME Business Statistics 2020/06/29",
#                            TestName = "EXTRA TIME Business Statistics 2020/06/29",
#                            TestName = "CRASH VERSION Business Statistics 2020/06/29",
                            IntroText = c(sprintf("For emergency questions during the exam, send an email to %s", sEmailaddress)
                                          )
                            )
BaseFileName <- sprintf("Q%s", ExamDate)


#######################################################################################
#######################################################################################
#######################################################################################
####
####  HERE FOLLOW THE ACTUAL EXAM QUESTIONS (PER BLOCK)
####
#######################################################################################
#######################################################################################
#######################################################################################

#
# Initialize question counter and
# set number of variations per question
#
QuestionNumber <- 1
NumberOfVariations <- 2
WhichQuestions <- NULL
# WhichQuestions <- c(WhichQuestions,
#                     "1a", "1b", "1c", "1d", "1e", "1g", "1h", "1i", "1j",
#                     "1l", "1m", "1n", "1o", "1p", "1q", "1r", "1s", "1t",
#                     "1u", "1v", "1w", "1x", "1y", "1z")
WhichQuestions <- c(WhichQuestions,
                    "1b", "1e", "1h", "1j", "1m", "1o", 
                    "1q", "1t", "1u", "1w", "1y", "1r")
# WhichQuestions <- c(WhichQuestions,
#                     "2core")
WhichQuestions <- c(WhichQuestions, "Selfie")
# WhichQuestions <- c(WhichQuestions,
#                     sprintf('2a%i', 1:7), '2b1')
# WhichQuestions <- c(WhichQuestions,
#                     "3a1", "3a2", "3a3",
#                     "3b1", "3b2",
#                     "3c1", "3c2", "3c3", "3c4", "3c5", "3c6", "3c7", "3c8")
WhichQuestions <- c(WhichQuestions, "IntegrityExit")

# integrity question with email address
blockkey <- startnewblock(QuizFile, "Integrity check")
Counters <- WriteQ(Q_Integrity(sEmailaddress), Counters)
closeblock(QuizFile)



# #
# # Build the exam part 1
# #
for (i1 in 1:length(WhichQuestions)) {
  print(sprintf("Question %s", WhichQuestions[i1]))
  # QuestionTitle <- sprintf("Question %d", i1)
  QuestionTitle <- sprintf("Question %s", WhichQuestions[i1])
  blockkey <- startnewblock(QuizFile, QuestionTitle)

  if ((stri_sub(WhichQuestions[i1], 1, 1) == "1")){
    # import the question code
    source(sprintf("%s/questions/%s_%s.R", ExamPath, BaseFileName, WhichQuestions[i1]))
    # evaluate and write the question
    eval(parse(text = sprintf("Counters <- %s_%s(QuizFile, Counters, NumberOfVariations)", BaseFileName, WhichQuestions[i1])))
    # drop the question code from memory
    eval(parse(text = sprintf("rm(%s_%s)", BaseFileName, WhichQuestions[i1])))
    # drop the question code from memory
    # eval(parse(text = sprintf("rm(%s_%s)", BaseFileName, WhichQuestions[i1])))
  } else {
    for (j in 1:NumberOfVariations){
      sQi= sprintf("Qi= Q_%s()", WhichQuestions[i1])
      eval(parse(text=sQi))
      Counters <- WriteQ(Qi, Counters)
    }
  }
  closeblock(QuizFile)
}


#######################################################################################
#######################################################################################
#######################################################################################
####
####  END OF THE ACTUAL EXAM QUESTIONS (PER BLOCK)
####
#######################################################################################
#######################################################################################
#######################################################################################




###############################
###############################
## Close the quiz file, zip it,
## and deletet the original files.
###############################
###############################
CloseQuizFile(QuizFile)
quizzip(mydir, QuizFile$key, deleteold = TRUE)

warnings()

sprintf("\n\nEmails are sent to %s", sEmailaddress)
