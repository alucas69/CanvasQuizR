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
WriteQuizDataframe <- function(ExamQ, mapname = "tmp", quizname = ""){
  # Magics
  EnginePath = "../"

  ###############################
  ###############################
  ## Load Canvas Quiz Rcode
  ###############################
  ###############################
  # lQuiz= c("aidfunctions.R", "closequizfile.R", "quizzip.R", "ALPHquestion.R", "MCquestion.R", "RoundAnswer.R", "tex2math.R", "FloorDfT.R", "MAquestion.R", "NUMquestion.R", "startnewblock.R", "WLprint.R", "closeblock.R", "imsmanifest.R", "mathformats.R", "opennewquizfile.R", "testheader.R", "assessment_meta.R", "MBquestion.R", "FMquestion.R")
  # for (r in lQuiz) {
  #   source(sprintf("%s/%s", EnginePath, r))
  # }
  
  ###############################
  ###############################
  ###############################
  ## Set my working subdirectory
  ## to save the quiz archive in.
  ## Also initialize the counters
  ## for questions and answers.
  ###############################
  ###############################
  mydir= sprintf("%s/%s", EnginePath, mapname)

  # #
  # # Build the exam part
  # #
  iQ= length(ExamQ)
  iQq= 0
  for (i in 1:iQ) iQq= iQq + length(ExamQ[[i]])
  eval(parse(text= sprintf("dfQuiz= data.frame(ID = c(1:%d))", iQq)))
  sQuestionnumber= ExamQ[[1]][[1]][["questionnumber"]]
  iQuestioncounter= 1
  
  for (i in 1:iQ) {
    
    for (i0 in 1:length(ExamQ[[i]])) {

      # get the question
      Q= ExamQ[[i]][[i0]]
      sQ= names(Q)
      
      # go over the different elements of this question
      for (i1 in 1:length(sQ)) {
        
        sQuestionname = sQ[i1]
        sQuestionvector= c(Q[[sQuestionname]])

        for (i2 in 1:length(sQuestionvector)) {
          
          # make name
          sColumnname= sprintf("%s.%d", sQuestionname, i2)
          
          # if name does not exist in dataframe, add column
          if (!(sColumnname %in% names(dfQuiz))) {
            # make column
            eval(parse(text = sprintf("dfQuiz$`%s` = NA", sColumnname)))            
          }
          
          # fill column
          if (typeof(sQuestionvector[i2]) == "character") {
            eval(parse(text = sprintf('dfQuiz$`%s`[%d] = \"%s\"', sColumnname,
                                      iQuestioncounter, 
                                      stri_replace_all(sQuestionvector[i2], 
                                                       "\\\\\"", regex = "\\\"")
            )))
          } else {
            eval(parse(text = sprintf("dfQuiz$`%s`[%d] = %s", sColumnname,
                                      iQuestioncounter, sQuestionvector[i2])))            
          }
          
        }
      }
      
      iQuestioncounter= iQuestioncounter + 1
    }
  }
  
  ###############################
  ###############################
  ## Close the quiz file, zip it,
  ## and deletet the original files.
  ###############################
  ###############################
  dfOut= dfQuiz
  drop(dfQuiz)
  if (quizname == "") quizname = sprintf("%s/%s", mydir, "dfOut.csv")
  else quizname = sprintf("%s/%s", mydir, quizname)
  write.csv(dfOut[ sort.list(names(dfOut)) ], file = quizname)
  return(dfOut)
}

