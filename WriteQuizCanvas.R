##############################################################
# WriteQuizCanvas(ExamDate, TestName, IntroText, ExamQ){
#
# Purpose:
#   Write the quiz file
#
# Inputs:
#   ExamDate  string, date of exam
#   TestName  string, text for test
#   IntroText string, explanation
#   ExamQ     array of (versions of an) array of exam entries, each with components
#     $type     string, either 'num', 'ma', 'mc', type of question
#     $q        string, question indicator ('2a1')
#     $text     string, question text
#     $answer   array of sorts, possible answers (depends on type of question)
#     $correct  array of sorts, correct answer
WriteQuizCanvas <- function(ExamDate, TestName, IntroText, ExamQ, htmlesc = FALSE){
  # Magics
  EnginePath = "../"
  # ExamDate= "20200605_q2"
  
  ###############################
  ###############################
  ## Load Canvas Quiz Rcode
  ###############################
  ###############################
  lQuiz= c("aidfunctions.R", "closequizfile.R", "quizzip.R", "ALPHquestion.R", "MCquestion.R", "RoundAnswer.R", "tex2math.R", "FloorDfT.R", "MAquestion.R", "NUMquestion.R", "startnewblock.R", "WLprint.R", "closeblock.R", "imsmanifest.R", "mathformats.R", "opennewquizfile.R", "testheader.R", "assessment_meta.R", "MBquestion.R", "FMquestion.R", "UPLOADquestion.R")
  for (r in lQuiz) {
    source(sprintf("%s/%s", EnginePath, r))
  }
  
  ###############################
  ###############################
  ## Set my working subdirectory
  ## to save the quiz archive in.
  ## Also initialize the counters
  ## for questions and answers.
  ###############################
  ###############################
  mydir= sprintf("%s/%s", EnginePath, "tmp")
  Counters <- list(AnswerCounter= 100000, QuestionCounter= 1)
  
  ###############################
  ###############################
  ## Open the new quiz file
  ###############################
  ###############################
  QuizFile <- OpenNewQuizFile(subdir= mydir,
                              TestName= TestName,
                              IntroText = IntroText)
  BaseFileName <- sprintf("Q%s", ExamDate)
  
  # #
  # # Build the exam part
  # #
  iQ= length(ExamQ)
  for (i in 1:iQ) {
    QuestionTitle <- sprintf("Question %d", i)
    blockkey <- startnewblock(QuizFile, QuestionTitle)
    # # import the question code
    # source(sprintf("%s/questions/%s_%s.R", ExamPath, BaseFileName, WhichQuestions[i1]))
    
    QV= ExamQ[[i]]
    for (Qi in QV){
      if (Qi$type == "mc"){
        Counters <- MCquestion(QuizFile, Qi$q, Qi$text,
                               Qi$answer, Qi$correct, Counters, texify=TRUE, htmlesc = htmlesc)
      } else if (Qi$type == "ma"){
        Counters <- MAquestion(QuizFile, Qi$q, Qi$text,
                               Qi$answer, Qi$correct, Counters, texify=TRUE, htmlesc = htmlesc)
      } else if (Qi$type == "mb"){
        Counters <- MBquestion(QuizFile, Qi$q, Qi$text,
                               Qi$answernames, Qi$answer, Counters, texify=TRUE, htmlesc = htmlesc)
      } else if (Qi$type == "num"){
        Counters <- NUMquestion(QuizFile, Qi$q, Qi$text,
                                Qi$correct, Counters, texify=TRUE, htmlesc = htmlesc)
      } else if (Qi$type == "upl"){
        Counters <- UPLOADquestion(QuizFile, Qi$q, Qi$text,
                                   Counters, texify=TRUE, htmlesc = htmlesc, pointspossible=0)
      }}
    # drop the question code from memory
    # eval(parse(text = sprintf("rm(%s_%s)", BaseFileName, WhichQuestions[i1])))
    closeblock(QuizFile)
  }
  
  ###############################
  ###############################
  ## Close the quiz file, zip it,
  ## and deletet the original files.
  ###############################
  ###############################
  CloseQuizFile(QuizFile)
  quizzip(mydir, QuizFile$key, deleteold = TRUE)
}

