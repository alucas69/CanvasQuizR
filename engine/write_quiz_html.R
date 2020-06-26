write_quiz_html_h1warning= function(x, message, below=FALSE) {
  if (is.null(x)) output= write_in_wrapper(paste(message, "(not set)"), "h1", s_wrappertag = "style=\"color:red\"")
  else if (below) output= c(write_in_wrapper(message, "h1"), write_in_wrapper(paste("", x), "p"))
  else output= write_in_wrapper(paste(message, x), "h1")
  return(output)
}
                                     

# l_quiz is a quiz list of blocks, each block a list of questions
write_quiz_html= function(l_quiz, s_filename="", subdir=".", texify=TRUE) {
  
  # set filename
  if (stri_length(s_filename) > 0) s_filename= paste(l_quiz$key, s_filename, sep="_")
  else if (!is.null(l_quiz$key)) s_filename= paste(l_quiz$key, ".html", sep="")
  else s_filename= "write_quiz.html"

  # print the name
  output= NULL
  output= c(output, write_quiz_html_h1warning(l_quiz$name, "Quiz name:"))
  output= c(output, write_quiz_html_h1warning(l_quiz$seed, "Quiz rnd seed:"))
  output= c(output, write_quiz_html_h1warning(l_quiz$key, "Quiz canvas identifier key:"))
  output= c(output, write_quiz_html_h1warning(l_quiz$emergency_html, "Quiz canvas emergency message:", below=TRUE))

  # compile the quiz
  for (i0 in 1:length(l_quiz$blocks)) {
    block= l_quiz$blocks[[i0]]
    output= c(
      output, 
      write_in_wrapper(
        write_in_wrapper(sprintf("Here starts block %s", paste("\"", block$name, "\"", "(number ", i0, ")")), "strong"), 
        "h1"))
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


