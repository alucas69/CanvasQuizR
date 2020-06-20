write_quiz_html_mc= function(question, minimumcorrect=1, maximumcorrect=1) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # write color coded answers
  output$answer= c(
    check_mc_answers(question, minimumcorrect=1, maximumcorrect=1)$warningmessage,
    write_as_html_ul_color(question$answer, as.logical(question$correct))
  )
  
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}

