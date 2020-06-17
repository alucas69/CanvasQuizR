#' Close a question block in a quiz file
#'
#' This function writes the lines needed to close a block of questions in a canvas quiz file.
#' @param QuizFile is a list structure with element QuizFile$fileConn the link to the open file where the question can be saved.
#' @keywords block canvas quiz
#' @export NONE
#' @examples closeblock(filehandler, "dkkqk")

closeblock <- function(QuizFile) {
  WLprint(QuizFile$fileConn, "        </section>")
}
