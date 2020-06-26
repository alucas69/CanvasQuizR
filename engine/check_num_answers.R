check_num_answers= function(question) {
  if (question$type != "num") stop(sprintf("ERROR: check_num_answers() for \"%s\" question", question$type))
  answer= matrix(as.double(c(question$answer)), ncol=3)
  is_false= (answer[ , 2] > answer[ , 1]) | (answer[ , 1] > answer[ , 3])
  output= list(warningmessage=NULL, rowwarnings=rep("", nrow(answer)))
  if (sum(as.integer(is_false)) > 0) {
    output$warningmessage= write_in_wrapper("WARNING: NUMERICAL QUESTION BOUND VIOLATION", "strong", "style=\"color:red\"")
    output$rowwarnings= c("", write_in_wrapper("WARNING: BOUND VIOLATION", "strong", "style=\"color:red\""))[1+is_false]
  }
  return(output)
}


