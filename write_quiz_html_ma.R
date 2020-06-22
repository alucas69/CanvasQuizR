write_quiz_html_ma= function(question) {
  return(write_quiz_html_mc(question, minimumcorrect = 1, maximumcorrect = (length(question$answer)-1) ))
}
