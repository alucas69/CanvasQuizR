#' Write the footer of a quiz file
#'
#' This function writes the footer part of a quiz file for Canvas.
#' @param QuizFile is a list structure with QuizFile$fileConn the file handler that will be closed upon exit.
#' @keywords canvas quiz
#' @export NONE
#' @examples CloseQuizFile( list(fileConn = filehandler) )

CloseQuizFile <- function(QuizFile) {

  #######################################
  #######################################
  ## footer
  #######################################
  #######################################
  fileConn = QuizFile$fileConn
  WLprint(fileConn, "      </section>")
  WLprint(fileConn, "    </assessment>")
  WLprint(fileConn, "  </questestinterop>")
  close(fileConn)

}
