check_num_answers= function(question) {
  answer= matrix(c(question$answer), ncol=3)
  correct= (answer[ , 2] <= answer[ , 1]) & (answer[ , 1] <= answer[ , 3])
  i_false= sum(as.integer(correct))
  output= list(warningmessage=NULL, rowwarnings=rep("", nrow(answer)))
  if (sum(as.integer(correct)) > 0) {
    output$warningmessage= write_in_wrapper("WARNING: NUMERICAL QUESTION BOUND VIOLATION", "strong", "style=\"color:red\"")
    output$rowwarnings= c(write_in_wrapper("WARNING: BOUND VIOLATION", "strong", "style=\"color:red\""), "")[1+correct]
  }
  return(output)
}


