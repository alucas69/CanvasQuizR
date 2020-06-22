#' construct Canvas xml code for a numerical answer question
#'
#' This function constructs Canvas xml code for a numerical answer question
#' @param question is a list structure with $text the question text,  $points_possible the number of points for this question (if absent, then 1pt), $question_key (if absent, randomly geneerated).
#' @param answer_counter is an integer, used to identify subanswers in other type questions.
#' @keywords numerical answer question for canvas quiz
#' @export list A list structure holding $output a vector of strings with the <xml> code for the question, and $answer_counter the updated answer_counter.
write_quiz_canvas_num= function(question, answer_counter) {
  
  # check html correctness of question text
  simple_html_checker(question$text)
  # check answers
  check_num_answers(question)
  
  # initialize identifiers
  if (is.null(question$question_key)) question_key= generate_key()
  if (is.null(question$points_possible)) points_possible= 1 else points_possible= question$points_possible
  iNumberOfAnswers <- nrow(answers)
  v_answer_identifiers= paste(answer_counter + (1:i_number_of_answers))
  answer_counter= answer_counter + i_number_of_answers

  # initialize output
  output= write_quiz_canvas_question_preamble("numerical_question", points_possible, v_answer_identifiers, question_key)

  # write the presentation part: material, response_str
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
  # add response part
  output3= write_in_wrapper("<response_label ident=\"answer1\"/>", "render_fib", s_wrappertag="fibtype=\"Decimal\"", block=TRUE)
  output2= c(output2, write_in_wrapper(output3, "response_str", s_wrappertag="ident=\"response1\" rcardinality=\"Single\"", block=TRUE))
  # complete presentation part and add to output
  output2= write_in_wrapper(output2, "presentation", block=TRUE)
  output= c(output, output2)
  
  # construct the answer processing part
  output2= write_in_wrapper("<decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>", "outcomes", block=TRUE)
  # add the individual answers
  for (i1 in 1:iNumberOfAnswers) {
    output3= write_in_wrapper(
      write_in_wrapper(
        c(
          write_in_wrapper(sprintf("%f",answers[i1,1]), "varequal", s_wrappertag="respident=\"response1\""),
          write_in_wrapper(c(
            write_in_wrapper(sprintf("%f",answers[i1,2]), "vargte", s_wrappertag="respident=\"response1\""),
            write_in_wrapper(sprintf("%f",answers[i1,3]), "varlte", s_wrappertag="respident=\"response1\""),
          ), "and", block=TRUE)
        ), "or", block=TRUE), 
      "conditionvar", block=TRUE)
    output3= c(output3, write_in_wrapper("100", "setvar", s_wrappertag="action=\"Set\" varname=\"SCORE\""))
    output3= write_in_wrapper(output3, "respcondition", s_wrappertag="continue=\"No\"", block=TRUE)
  }
  # put total answer(s) in canvas answer processing wrapper
  output= c(output, write_in_wrapper(output2, "resprocessing", block=TRUE))
  
  # add total question wrapper
  output= write_in_wrapper(output, "item", s_wrappertag=sprintf("ident=\"%s\" title=\"%s\"", question_key, question$q), block=TRUE)
  return(list(output=output, answer_counter=answer_counter))
}


