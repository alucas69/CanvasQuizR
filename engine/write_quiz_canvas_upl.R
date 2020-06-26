#' construct Canvas xml code for an upload question
#'
#' This function constructs Canvas xml code for an upload question
#' @param question is a list structure with $text the question text,  $points_possible the number of points for this question (if absent, then 1pt), $question_key (if absent, randomly geneerated).
#' @param answer_counter is an integer, used to identify subanswers in other type questions.
#' @keywords upload question for canvas quiz
#' @export list A list structure holding $output a vector of strings with the <xml> code for the question, and $answer_counter the updated answer_counter.
write_quiz_canvas_upl= function(question, answer_counter) {
  
  # check html correctness of question text
  simple_html_checker(question$text)
  
  # initialize identifiers
  if (is.null(question$question_key)) question_key= generate_key()
  if (is.null(question$points_possible)) points_possible= 1 else points_possible= question$points_possible
  i_number_of_answers= 1
  v_answer_identifiers= paste(answer_counter + (1:i_number_of_answers))
  answer_counter= answer_counter + i_number_of_answers
  
  # initialize output
  output= write_quiz_canvas_question_preamble("file_upload_question", points_possible, v_answer_identifiers, question_key)

  # write html the question in <div> wrapper, and then escape it
  output2= html_escape(
    write_in_wrapper(
      write_in_wrapper(question$text, "p"),
      "div", block=TRUE
    )
  )
  # add wrappers for canvas
  output2= write_in_wrapper(output2, "mattext", s_wrappertag="texttype=\"text/html\"", block=TRUE)
  output2= write_in_wrapper(output2, "material", block=TRUE)
  output2= write_in_wrapper(output2, "presentation", block=TRUE)
  output= c(output, output2)
  
  # add upload field in canvas wrappers
  output2= "<decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>"
  output2= write_in_wrapper(output2, "outcomes", block=TRUE)
  output2= write_in_wrapper(output2, "resprocessing", block=TRUE)
  output= c(output, output2)
  
  # add total question wrapper
  output= write_in_wrapper(output, "item", s_wrappertag=sprintf("ident=\"%s\" title=\"%s\"", question_key, question$q), block=TRUE)
  return(list(output=output, answer_counter=answer_counter))
}


