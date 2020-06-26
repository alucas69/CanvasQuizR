write_quiz_html_num= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # check html
  simple_html_checker(question$text)

  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # check answers
  check= check_num_answers(question)
  # write answers in table
  output$answer= c(
    check$warningmessage,
    write_as_html_table(
      rbind(
        c("Value", "Lower", "Upper", "Check"),
        cbind(
          matrix(c(question$answer), ncol = 3),
          check$rowwarnings
        )
      ),
      firstrowheader = TRUE
    )
  )
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}

