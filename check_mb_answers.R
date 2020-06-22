check_mb_answers= function(question, questionvariables=NULL) {
  if (question$type != "mb") stop(sprintf("ERROR: check_mb_answers() for \"%s\" question", question$type))
  if ((length(question$answer) != length(question$answernames)) | (length(question$answer) != length(question$correct))) {
    stop(sprintf("ERROR IN MB QUESTION \"%s\": WRONG LENGTHS OF ANSWER VECTORS\nanswer:\n%s\nanswernames:\n%s\ncorrect:\n%s", question$q, paste(question$answer, collapse = ", "), paste(question$answernames, collapse = ", "), paste(question$correct, collapse = ", ")))    
  }
  if (is.null(questionvariables)) questionvariables= write_quiz_html_mb_highlight_variables(question$text)$variables
  output=list(warningmessage=NULL, rowwarnings=rep("", length(question$answer)), notused=NULL)
  
  correct= rep(TRUE, length(question$answernames))
  notused= which(!(question$answernames %in% questionvariables))
  notdefined= which(!(questionvariables %in% question$answernames))
  if (length(notused) > 0) output$rowwarnings[notused]= write_in_wrapper("WARNING: NOT USED!!","strong")
  if (length(notdefined) > 0) 
    output$notused= paste(
      write_in_wrapper("WARNING: NOT DEFINED!!", "strong"), 
      write_quiz_html_mb_highlight_variables(questionvariables[notdefined])$text
    )
  if ((max(stri_length(output$rowwarnings)) > 0) | (!is.null(output$notused)))
    output$warningmessage= write_in_wrapper("WARNING: UNUSED OR UNDEFINED VARIABLES IN MB QUESTION", "strong", "style=\"color:red\"")
  return(output)
}

