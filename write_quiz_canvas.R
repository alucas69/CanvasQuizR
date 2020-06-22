# l_quiz is a quiz list of blocks, each block a list of questions
write_quiz_canvas= function(l_quiz, quiz_name="R generated quiz", subdir=".", quiz_key=NULL,  
                            zip=TRUE, deletexml=TRUE) {
  # initialize
  if (is.null(quiz_key)) quiz_key= generate_key()
  
  # compile the quiz
  output= NULL
  output_question= list(answer_counter=100000)
  for (i0 in 1:length(l_quiz)) {
    block= l_quiz[[i0]]
    output_block= NULL
    for (i1 in 1:length(block)) {
      question= block[[i1]]
      if (question$type == "mc") output_question= write_quiz_canvas_mc(question, output_question$answer_counter)
      else if (question$type == "ma") output_question= write_quiz_canvas_ma(question, output_question$answer_counter)
      else if (question$type == "mb") output_question= write_quiz_canvas_mb(question, output_question$answer_counter)
      else if (question$type == "num") output_question= write_quiz_canvas_num(question, output_question$answer_counter)
      else if (question$type == "alph") output_question= write_quiz_canvas_alph(question, output_question$answer_counter)
      else if (question$type == "upl") output_question= write_quiz_canvas_upl(question, output_question$answer_counter)
      output_block= c(output_block, output_question$output)
    }
    output= c(output, write_quiz_canvas_blockwrapper(output_block))
  }
  
  # canvas total quiz sections wrapper
  output= write_in_wrapper(output, "section", s_wrappertag="ident=\"root_section\"", block=TRUE)
  
  # precede by header
  output2= c(
    write_in_wrapper("cc_maxattempts", "fieldlabel"),
    write_in_wrapper("1", "fieldentry"))
  output2= write_in_wrapper(output2, "qtimetadatafield", block=TRUE)
  output= c(write_in_wrapper(output2, "qtimetadata", block=TRUE), output)
  
  # conclude by footer"
  output= write_in_wrapper(output, "assessment", sprintf("ident=\"%s\" title=\"%s\"", quiz_key, quiz_name), block=TRUE)
  output= write_in_wrapper(output, "questestinterop", s_wrappertag="xmlns=\"http://www.imsglobal.org/xsd/ims_qtiasiv1p2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.imsglobal.org/xsd/ims_qtiasiv1p2 http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd\"", block=TRUE)
  
  # write the files
  imsmanifest(quiz_key, subdir)
  assessment_meta(quiz_key, subdir=subdir, title=quiz_name)
  f= file(sprintf("%s/%s/%s.xml", subdir, quiz_key, quiz_key), "w")
  writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", f)
  writeLines(output, f)
  close(f)
  if (zip) quizzip(subdir, quiz_key, deleteold=deletexml)
}

