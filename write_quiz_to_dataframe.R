##############################################################
# write_quiz_to_dataframe(exam, mapname, quizname){
#
write_quiz_to_dataframe <- function(exam, mapname = ".", filename = NULL, zipit=FALSE){

  # initialize
  tmpname = paste("df", generate_key(10))
  iQ= length(exam)
  iQq= 0

  # count total number of questions
  for (i in 1:iQ) iQq= iQq + length(exam[[i]])
  eval(parse(text= sprintf("%s= data.frame(ID = c(1:%d))", tmpname, iQq)))
  sQuestionnumber= exam[[1]][[1]][["questionnumber"]]
  iQuestioncounter= 1

  # loop over quiz sections
  for (i in 1:iQ) {
    
    # look over questions within section
    for (i0 in 1:length(exam[[i]])) {

      # get the question
      Q= exam[[i]][[i0]]
      sQ= names(Q)
      
      # go over the different elements of this question
      for (i1 in 1:length(sQ)) {
        
        sQuestionname = sQ[i1]
        sQuestionvector= c(Q[[sQuestionname]])

        for (i2 in 1:length(sQuestionvector)) {
          
          # make name
          sColumnname= sprintf("%s.%d", sQuestionname, i2)
          
          # if name does not exist in dataframe, add column
          if (!(sColumnname %in% eval(parse(text = sprintf("names(%s)", tmpname))))) {
            # make column
            eval(parse(text = sprintf("%s$`%s` = NA", tmpname, sColumnname)))            
          }
          
          # fill column
          if (typeof(sQuestionvector[i2]) == "character") {
            eval(parse(text = sprintf('%s$`%s`[%d] = \"%s\"', tmpname, sColumnname,
                                      iQuestioncounter, 
                                      stri_replace_all(sQuestionvector[i2], 
                                                       "\\\\\"", regex = "\\\"")
            )))
          } else {
            eval(parse(text = sprintf("%s$`%s`[%d] = %s", tmpname, sColumnname,
                                      iQuestioncounter, sQuestionvector[i2])))            
          }
          
        }
      }
      
      iQuestioncounter= iQuestioncounter + 1
    }
  }

  # store result in local variable and delete the global
  dfOut= eval(parse(text = sprintf("%s", tmpname)))
  eval(parse(text = sprintf("drop(%s)", tmpname)))
  
  # write the output  
  if (is.null(quizname)) quizname = sprintf("%s/%s", mydir, "dfCanvasQuiz.csv")
  else quizname = sprintf("%s/%s", mydir, quizname)
  write.csv(dfOut[ sort.list(names(dfOut)) ], file = quizname)
  return(dfOut)
}

