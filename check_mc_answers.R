check_mc_answers= function(question, minimumcorrect=1, maximumcorrect=1) {
  if (length(question$answer) != length(question$correct))
    stop(sprintf("ERROR in question \"%s\": answers and correct have unequal lenghts for ma/mc question\nanswer:\n%s\ncorrect:\n%s", question$q, paste(question$answer, collapse = ", "), paste(question$correct, collapse = ", ")))
  if (sum(as.integer(question$correct)) < minimumcorrect) 
    output= write_in_wrapper("WARNING: TOO FEW CORRECT ANSWERS!!", "strong", s_wrappertag = "style=\"color:red\"")
  else if (sum(as.integer(question$correct)) > maximumcorrect) 
    output= write_in_wrapper("WARNING: TOO MANY CORRECT ANSWERS!!", "strong", s_wrappertag = "style=\"color:red\"")
  else output= NULL
  return(list(warningmessage=output))
}
