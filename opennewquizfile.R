#' Prepare canvas quiz file set-up
#'
#' This function creates the quiz wrapper file needed by Canvas.
#' @param subdir is the map where the files are placed, the contents of which need to be zipped and uploaded into CANVAS. The default is the current directory.
#' @param key is a string. If empty, a random key will be generated.
#' @param TestName is a string holding the name of the Canvas test.
#' @param IntroText is a vector of strings holding the general introductory text for the quiz. Each element is in a new paragraph.
#' @param randomizeMC is a Boolean. If true, Canvas randomizes the order of MC and MA questions.
#' @keywords say hello
#' @export list a list holding two elements
#' @export list$key the key that is used is exported.
#' @export list$fileConn the file connection to which text must be written.
#' @examples
#' OpenNewQuizFile(".", "mysteriouskey_1938cqk1", "Exam", "Go go go!!")

OpenNewQuizFile <- function(subdir = ".", key = "", TestName, IntroText = "",
                            randomizeMC = FALSE) {

  #############################################
  #############################################
  ## if no key, then set the key
  #############################################
  #############################################
  keylength = 33
  if (key == "") {
    key = stri_rand_strings(1, keylength)
  }
  if (!dir.exists(subdir)) dir.create(subdir)

  #############################################
  #############################################
  ## write files
  #############################################
  #############################################
  imsmanifest(subdir, key)
  assessment_meta(subdir, key, TestName, IntroText, randomizeMC)
  filehandler <- testheader(subdir, key, TestName)

  #############################################
  #############################################
  ## return the key and filestream
  #############################################
  #############################################
  return( list(fileConn = filehandler, key = key) )
}
