#' construct Canvas xml code for a multiple answer question
#'
#' This function constructs Canvas xml code for a multiple answer question
#' @param question is a list structure with $text the question text,  $points_possible the number of points for this question (if absent, then 1pt), $question_key (if absent, randomly geneerated).
#' @param answer_counter is an integer, used to identify subanswers in other type questions.
#' @keywords multiple answer question for canvas quiz
#' @export list A list structure holding $output a vector of strings with the <xml> code for the question, and $answer_counter the updated answer_counter.
write_quiz_canvas_mc= function(question, answer_counter) {
  
  # check html correctness of question text
  simple_html_checker(question$text)
  simple_html_checker(question$answer)
  # check answer number
  check_mc_answers(question, 1, length(question$answer)-1)
  
  # initialize identifiers
  if (is.null(question$question_key)) question_key= generate_key()
  if (is.null(question$points_possible)) points_possible= 1 else points_possible= question$points_possible
  iNumberOfAnswers <- length(question$answer)
  v_answer_identifiers= paste(answer_counter + (1:i_number_of_answers))
  answer_counter= answer_counter + i_number_of_answers

  # initialize output
  output= write_quiz_canvas_question_preamble("multiple_answers_question", points_possible, v_answer_identifiers, question_key)

  # write the presentation part: material, response_lid
  # html the material (=question) in <div> wrapper, and then escape it
  output2= html_escape(
    write_in_wrapper(
      write_in_wrapper(question$text, "p"),
      "div", block=TRUE
    )
  )
  # add wrappers for canvas
  output2= write_in_wrapper(output2, "mattext", s_wrappertag="texttype=\"text/html\"", block=TRUE)
  output2= write_in_wrapper(output2, "material", block=TRUE)
  # add responses part
  output3= NULL
  for (i1 in 1:iNumberOfAnswers) {
    output3= c(output3,
               write_in_wrapper(
                 write_in_wrapper(
                   write_in_wrapper(html_escape(question$answer[i1]), "mattext", s_wrappertag="texttype=\"text/html\""),
                   "material", block=TRUE
                 ), "response_label", s_wrappertag=sprintf("ident=\"%s\"", vAnswerIdentifiers[i1])
               ))
  }
  output3= write_in_wrapper(output3, "render_choice", block=TRUE)
  output2= c(output2, write_in_wrapper(output3, "response_lid", s_wrappertag="ident=\"response1\" rcardinality=\"Multiple\"", block=TRUE))
  # complete presentation part and add to output
  output2= write_in_wrapper(output2, "presentation", block=TRUE)
  output= c(output, output2)
  
  # construct the answer processing part
  output2= write_in_wrapper("<decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>", "outcomes", block=TRUE)
  # add the correct answer indicator and add canvas wrappers
#  output3= write_in_wrapper(answer_tmp[1], "varequal", s_wrappertag="respident=\"response1\"")
  output3= NULL
  for (i1 in 1:iNumberOfAnswers) 
    output3= c(output3,
               write_in_wrapper(
                 write_in_wrapper(v_answer_identifiers[i1], "varequal", s_wrappertag="respident=\"response1\""),
                 c("not","")[1+question$correct[i1]]))
  output3= write_in_wrapper(output3, "and")
  output3= write_in_wrapper(output3, "conditionvar")
  output3= c(output3, write_in_wrapper("100", "setvar", s_wrappertag="action=\"Set\" varname=\"SCORE\""))
  output3= write_in_wrapper(output3, "respcondition")
  output2= c(output2, output3)
  output2= write_in_wrapper(output2, "resprocessing", block=TRUE)
  output= c(output, output2)
  
  # add total question wrapper
  output= write_in_wrapper(output, "item", s_wrappertag=sprintf("ident=\"%s\" title=\"%s\"", question_key, question$q), block=TRUE)
  return(list(output=output, answer_counter=answer_counter))
}


