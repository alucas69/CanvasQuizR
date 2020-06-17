########################################
## Q20200605_0core.R
##
## Purpose:
##   Provide question on declaration: Answers are my own, without outside help
##
## Author:
##   Charles Bos
##
## Version:
##   a    first start
##
## Date:
##   2020/4/20
########################################

# Imports

#############################################################################
# Q= Q_0()
# Purpose:
#   Prepare single question for 0, will you do this alone
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_0 <- function(){
  sType= 'mc'
  sq= '0'

  sQ1= "I promise to do this Statistics exam by myself, and only by myself, with help only from the admitted materials (books, sheets, Rstudio, excel, calculator, google), without help from any person, either directly or through email, whatsapp or other means of communication."
  sQ2= "This above statement is:"

  vAnswers= c("True", "False")
  vCorrect= c(TRUE, FALSE)

  sText= c(sQ1, sQ2)
  return (list(type=sType, q=sq, text=sText, answer=vAnswers, correct=vCorrect))
}

#############################################################################
# Q= Q_id()
# Purpose:
#   Prepare single question for 'id', leave placeholder for uploading the ID
#
# Return value:
#   Q     list, with
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
Q_id <- function(){
  sType= 'num'
  sq= 'id'

  sText= "At this question, take a picture of yourself with your student-ID, and upload it at this location"
  # vCorrect= matrix(c(-1, -1, -1), ncol= 3)
  vCorrect= -1

  return (list(type=sType, q=sq, text=sText, correct=vCorrect))
}
