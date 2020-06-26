# l_quiz is a quiz list of blocks, each block a list of questions
write_quiz_html= function(l_quiz, s_filename="write_quiz_html.html", subdir=".", texify=TRUE) {
  # compile the quiz
  output= NULL
  for (i0 in 1:length(l_quiz)) {
    block= l_quiz[[i0]]
    output= c(output, write_in_wrapper(write_in_wrapper(sprintf("Here starts block %d", i0), "strong"), "h1"))
    for (i1 in 1:length(block)) {
      question= block[[i1]]
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


