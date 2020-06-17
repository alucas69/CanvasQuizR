#' Auxiliary function for writeing to file without quotes
#'
#' This function combines writeLines and noquote.
#' @param filehandler is the filehandler to which the output must be written.
#' @param text is a string that will be written to the file handler as writeLines(noquote(text), con = filehandler). If empty, a random key will be generated.
#' @keywords canvas quiz
#' @export NONE no output
#' @examples
#' WLprint(filehandler, "hello \"world\"")

WLprint <- function(file, text) writeLines(noquote(text), con = file)
