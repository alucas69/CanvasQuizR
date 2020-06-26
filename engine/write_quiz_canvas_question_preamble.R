write_quiz_canvas_question_preamble= function(question_type, possible_points, answer_ids, question_key) {

  output=NULL

  # write the question type in canvas wrapper
  output2= write_in_wrapper(
    c(
      write_in_wrapper("question_type", "fieldlabel"),
      write_in_wrapper(question_type, "fieldentry")
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
      write_in_wrapper(paste(answer_ids, collapse = ","), "fieldentry")
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
  
  return(output) 
}