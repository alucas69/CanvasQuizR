write_quiz_html_mb= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  mb_core= write_quiz_html_mb_highlight_variables(question$text)
  output$question= write_in_wrapper(mb_core$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # write answer part
  check= check_mb_answers(question, mb_core$variables)
  output$answer= write_as_html_ul_color(
    c(paste(check$rowwarnings,
            write_quiz_html_mb_highlight_variables(paste(
              question$answernames, ": ", question$answer, sep=""))$text
    ),
    check$notused),
    c((stri_length(check$rowwarnings) == 0), rep(FALSE, length(check$notused))),
    truecolor = "black"
  )
  output$answer= c(check$warningmessage, output$answer)
  
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}


