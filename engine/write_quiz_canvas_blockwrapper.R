#' Start a new question block in a quiz file
#'
#' This function writes the lines needed to start a new block of questions in a canvas quiz file.
#' @param vs_text is a vector of strings holding the xml code of the questions in the block for canvas.
#' @param blockname is a string holding the name of the section of the quiz.
#' @param selection_number is the number of questions to be chosen from this section.
#' @param points_per_item is the number of points per selected item for this section.
#' @keywords block canvas quiz section
#' @export vs_output holds a vector of strings with the xml code for this whole section.

write_quiz_canvas_blockwrapper= function(vs_text, blockname=NULL,
                                         selection_number=1, points_per_item=1) {

  # generate random key identifier
  keylength = 33
  block_key = stri_rand_strings(1, keylength)
  if (is.null(blockname)) blockname=""
  
  # start at level 2 (level one is the section wrapper)
  {
    # construct header
    output2= write_in_wrapper(sprintf("%d", selection_number), "selection_number")
    output2= c(output2, write_in_wrapper(write_in_wrapper(sprintf("%1.1f", points_per_item), "points_per_item"), "selection_extension", block=TRUE))
    output2= write_in_wrapper(output2, "selection", block=TRUE)
    output2= write_in_wrapper(output2, "selection_ordering", block=TRUE)
  }
  output1= c(output2, vs_text)
  output1= write_in_wrapper(output1, "section", s_wrappertag=sprintf("ident=\"%s\" title=\"%s\"", block_key, blockname), block=TRUE)
}
