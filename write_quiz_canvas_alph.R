#' construct Canvas xml code for an essay question
#'
#' This function constructs Canvas xml code for an essay question
#' @param question is a list structure with $text the question text,  $points_possible the number of points for this question (if absent, then 1pt).
#' @param answer_counter is an integer, used to identify subanswers in other type questions.
#' @param question_key is an identifier for the question. If not present, a random key is generated using generate_key().
#' @keywords essay question for canvas quiz
#' @export list A list structure holding $output a vector of strings with the <xml> code for the question, and $answer_counter the updated answer_counter.
write_quiz_canvas_alph= function(question, answer_counter, question_key=NULL) {
  
  # check html correctness of question text
  simple_html_checker(question$text)
  
  # initialize identifiers
  if (is.null(question_key)) question_key= generate_key()
  i_number_of_answers= 1
  v_answer_identifiers= paste(answer_counter + (1:i_number_of_answers))
  answer_counter= answer_counter + i_number_of_answers
  if (is.null(question$points_possible)) points_possible= 1 else points_possible= question$points_possible
  
  # initialize output
  output= NULL
  
  # write the question type in canvas wrapper
  output2= write_in_wrapper(
    c(
      write_in_wrapper("question_type", "fieldlabel"),
      write_in_wrapper("essay_question", "fieldentry")
    ), "qtimetadatafield", block=TRUE
  )
  output= c(output, output2)
  
  # allocate the points in canvas wrapper
  output2= write_in_wrapper(
    c(
      write_in_wrapper("points_possible", "fieldlabel"),
      write_in_wrapper(sprintf("%1.1f", possible_points), "fieldentry")
    ), "qtimetadatafield", block=TRUE
  )
  output= c(output, output2)
  
  # introduce unique identifiers for the answers step1 in canvas wrapper
  output2= write_in_wrapper(
    c(
      write_in_wrapper("original_answer_ids", "fieldlabel"),
      write_in_wrapper("", "fieldentry")
    ), "qtimetadatafield", block=TRUE
  )
  output= c(output, output2)
  
  # introduce unique identifiers for the answers step2 in canvas wrapper
  output2= write_in_wrapper(
    c(
      write_in_wrapper("assessment_question_identifierref", "fieldlabel"),
      write_in_wrapper(sprintf("%s", paste(question_key, "a", sep = "")), "fieldentry")
    ), "qtimetadatafield", block=TRUE
  )
  output= c(output, output2)
  
  # add wrappers for canvas
  output= write_in_wrapper(output, "qtimetadata", block=TRUE)
  output= write_in_wrapper(output, "itemmetadata", block=TRUE)
  
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
  
  # add response part
  output3= write_in_wrapper("<response_label ident=\"answer1\" rshuffle=\"No\"/>", "render_fib", block=TRUE)
  output2= c(output2, write_in_wrapper(output3, "response_str", s_wrappertag="ident=\"response1\" rcardinality=\"Single\"", block=TRUE))
  output2= write_in_wrapper(output2, "presentation", block=TRUE)
  output= c(output, output2)
  
  # add upload field in canvas wrappers
  output2= "<decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>"
  output2= write_in_wrapper(output2, "outcomes", block=TRUE)
  output2= c(output2, write_in_wrapper(write_in_wrapper("<other/>", "conditionvar", block=TRUE), "respcondition", s_wrappertag="continue=\"No\"", block=TRUE))
  output2= write_in_wrapper(output2, "resprocessing", block=TRUE)
  output= c(output, output2)

  # add total question wrapper
  output= write_in_wrapper(output, "item", s_wrappertag=sprintf("ident=\"%s\" title=\"%s\"", question_key, question$q), block=TRUE)
  return(list(output=output, answer_counter=answer_counter))
}


