#' Start a new question block in a quiz file
#'
#' This function writes the lines needed to start a new block of questions in a canvas quiz file.
#' @param QuizFile is a list structure with elements QuizFile$fileConn the link to the open file where the question can be saved, and QuizFile$key a string holding the Canvas identifier for the quiz.
#' @param BlockName is a string holding the name of the section of the Canvas quiz.
#' @keywords block canvas quiz
#' @export Blockkey holds a string with the block (= section) identifier.
#' @examples
#' startnewblock(filehandler, "kcl388l1", "Question 1")

startnewblock <- function(QuizFile, BlockName) {

  #######################################
  #######################################
  ## header
  #######################################
  #######################################
  fileConn = QuizFile$fileConn
  key = QuizFile$key
  keylength = 33
  Blockkey = stri_rand_strings(1, keylength)
  WLprint(fileConn, sprintf("        <section ident=\"%s\" title=\"%s\">", Blockkey, BlockName))
  WLprint(fileConn, "          <selection_ordering>
            <selection>
              <selection_number>1</selection_number>
              <selection_extension>
                <points_per_item>1.0</points_per_item>
              </selection_extension>
            </selection>
          </selection_ordering>  ")

  ###############################
  ###############################
  ## return block key
  ###############################
  ###############################
  return(Blockkey)
}
