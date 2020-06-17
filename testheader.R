#' Write the header of a quiz file
#'
#' This function writes the header part of a quiz file for Canvas.
#' @param subdir is a string holding the directory for the file. Default is the current working directory "." Note that within the working directory, the file name will be "%key%/%key%.xml".
#' @param key is a string holding the Canvas identifier for the quiz. This is used as the filename.
#' @param TestName is a string holding the name of the Canvas test.
#' @keywords canvas quiz
#' @export filestream holds the open stream of the quiz file for further writing of questions an close wrapper.
#' @examples testheader(".", "deirvoq", "Exam 2020")

testheader <- function(subdir = ".", key, TestName) {

  #######################################
  #######################################
  ## open the file; make sure the working
  ## directory/key subfolder is already
  ## available.
  #######################################
  #######################################

  # you need to make sure that this subdirectory exists
  fname = sprintf("%s/%s/%s.xml", subdir, key, key)
  fileConn <- file(fname, "w")


  #######################################
  #######################################
  ## header
  #######################################
  #######################################
  WLprint(fileConn, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <questestinterop xmlns=\"http://www.imsglobal.org/xsd/ims_qtiasiv1p2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.imsglobal.org/xsd/ims_qtiasiv1p2 http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd\">  ")
  WLprint(fileConn, sprintf("    <assessment ident=\"%s\" title=\"%s\">", key, TestName))
  WLprint(fileConn, "      <qtimetadata>
        <qtimetadatafield>
          <fieldlabel>qmd_timelimit</fieldlabel>
          <fieldentry>120</fieldentry>
        </qtimetadatafield>
        <qtimetadatafield>
          <fieldlabel>cc_maxattempts</fieldlabel>
          <fieldentry>1</fieldentry>
        </qtimetadatafield>
      </qtimetadata>
      <section ident=\"root_section\">  ")

  ######################################
  ######################################
  ## return filestream
  ######################################
  ######################################
  return(fileConn)
}
