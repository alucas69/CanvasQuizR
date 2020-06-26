# l_quiz is a quiz list of blocks, each block a list of questions
write_quiz_html= function(l_quiz, s_filename="write_quiz_html.html", subdir=".", texify=TRUE) {

  # print the identifier and emergency message
  if (is.null(l_quiz$seed)) output= write_in_wrapper("Exam ID: (not set)", "h1", s_wrappertag = "style=\"color:red\"")
  else output= write_in_wrapper(paste("Exam ID:", l_quiz$seed), "h1")
  if (is.null(l_quiz$emergency_html) | (stri_length(l_quiz$emergency_html)==0)) 
    output= c(output, write_in_wrapper("Emergency message: (not set)", "h1", s_wrappertag = "style=\"color:red\""))
  else 
    output= c(output, write_in_wrapper("Emergency message:", "h1"),
              write_in_wrapper(l_quiz$emergency_html, "p"))
  
  # compile the quiz
  for (i0 in 1:length(l_quiz$blocks)) {
    block= l_quiz$blocks[[i0]]
    output= c(output, write_in_wrapper(write_in_wrapper(sprintf("Here starts block %d", i0), "strong"), "h1"))
    for (i1 in 1:length(block$questions)) {
      question= block$questions[[i1]]
      if (texify) {
        question$text= tex2math(question$text)
        if (("answer" %in% names(question)) & (question$type != "num")) question$answer= tex2math(question$answer)
      }
      if (question$type == "mc") output= c(output, write_quiz_html_mc(question))
      else if (question$type == "ma") output= c(output, write_quiz_html_ma(question))
      else if (question$type == "mb") output= c(output, write_quiz_html_mb(question))
      else if (question$type == "num") output= c(output, write_quiz_html_num(question))
      else if (question$type == "alph") output= c(output, write_quiz_html_alph(question))
      else if (question$type == "upl") output= c(output, write_quiz_html_upl(question))
      output= c(output, "&nbsp;<br>&nbsp;<br>")
    }
  }
  write_quiz_html_file(s_filename, output, subdir)  
}


