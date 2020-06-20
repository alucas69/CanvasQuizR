write_quiz_html_alph= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  
  # output the table
  output= write_three_part_table(output$heading, output$question)
  return(output)
}
