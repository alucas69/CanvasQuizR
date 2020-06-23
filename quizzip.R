#' Zip the quiz and delete the original files
#'
#' @param subdir is the map where the files are placed, the contents of which need to be zipped and uploaded into CANVAS. The default is the current directory.
#' @param quiz_key is a string holding the key ( = name) of the main quiz file.
#' @param deleteold is a boolean. If true, delete the old files after zipping. Default is FALSE.
#' @export NONE no outputs
#' @examples
#' quizzip("quizidentifier-12345")

quizzip <- function(subdir = ".", quiz_key, deleteold = FALSE) {

  currentdir = getwd()
  setwd(subdir)
  zip(
    sprintf("%s.zip", quiz_key),
    c("imsmanifest.xml", sprintf("%s", quiz_key), "non_cc_assessments")
  )
  if (deleteold == TRUE) {
    unlink("imsmanifest.xml")
    unlink(sprintf("%s", quiz_key), recursive = TRUE, force = TRUE)
    unlink("non_cc_assessments", recursive = TRUE)
  }
  setwd(currentdir)
}
