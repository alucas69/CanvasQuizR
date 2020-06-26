#' Write the IMSMANIFEST.XML file for CANVAS
#'
#' This function creates the quiz wrapper file needed by Canvas.
#' @param quiz_key is a string holding the identifier (= file name) of the quiz.
#' @param subdir is the directory where the files are placed. It will be created if it does not already exist.
#' @keywords IMS manifest for canvas quiz export/import package
#' @export NONE
#' @examples imsmanifest("quiz#21", "./tmp")
#' imsmanifest()

imsmanifest= function(quiz_key, subdir=".") {

  # generate additional keys
  keylength = 33
  key2 = stri_rand_strings(1, keylength)
  key3 = stri_rand_strings(1, keylength)

  # start level 1 output: comment
  output1= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  
  # start level 2 output with 3 elements: metadata, organizations, resources
  {
    # initialize
    output2= NULL
    
    # metadata inside (level 3): 3 elements schema, schemaversion, imsmd:lom
    {
      output3= c(
        write_in_wrapper("IMS Content", "schema"),
        write_in_wrapper("1.1.3", "schemaversion"))
      
      # level 4: imsmd:lom inside
      {
        output4= NULL

        output5= write_in_wrapper("QTI Quiz Export for canvas course", "imsmd:string")
        output5= write_in_wrapper(output5, "imsmd:title", block=TRUE)
        output5= write_in_wrapper(output5, "imsmd:general", block=TRUE)
        output4= c(output4, output5)
        
        output5= write_in_wrapper(sprintf("%s",Sys.Date()), "imsmd:dateTime")
        output5= write_in_wrapper(output5, "imsmd:date", block=TRUE)
        output5= write_in_wrapper(output5, "imsmd:contribute", block=TRUE)
        output5= write_in_wrapper(output5, "imsmd:lifeCycle", block=TRUE)
        output4= c(output4, output5)

        output6= write_in_wrapper("yes", "imsmd:value")
        output5= write_in_wrapper(output6, "imsmd:copyrightAndOtherRestrictions", block=TRUE)
        output6= write_in_wrapper("Private (Copyrighted) - http://en.wikipedia.org/wiki/Copyright", "imsmd:string")
        output5= c(output5, write_in_wrapper(output6, "imsmd:description", block=TRUE))
        output4= c(output4, write_in_wrapper(output5, "imsmd:rights", block=TRUE))
      }
      
      output3= c(output3, write_in_wrapper(output4, "imsmd:lom", block=TRUE))
    }
    
    output2= c(output2, write_in_wrapper(output3, "metadata", block=TRUE))
    output2= c(output2, "<organizations/>")
    
    # add resources
    {
      {
        output4= sprintf("<file href=\"%s/%s.xml\"/>", quiz_key, quiz_key)
        output4= c(output4, sprintf("<dependency identifierref=\"%s\"/>", key3))
      }
      
      output3= c(write_in_wrapper(output4, "resource", 
                                  s_wrappertag=sprintf("identifier=\"%s\" type=\"imsqti_xmlv1p2\"", quiz_key),
                                  block=TRUE),
                 write_in_wrapper(sprintf("<file href=\"%s/assessment_meta.xml\"/>", quiz_key), "resource",
                                  s_wrappertag=sprintf("identifier=\"%s\" type=\"associatedcontent/imscc_xmlv1p1/learning-application-resource\" href=\"%s/assessment_meta.xml\">", key3, quiz_key),
                                  block=TRUE))
    }
    
    output2= c(output2, write_in_wrapper(output3, "resources", block=TRUE))
  }
  
  output1= c(output1,
             write_in_wrapper(output2, "manifest", s_wrappertag=sprintf("identifier=\"%s\" xmlns=\"http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1\" xmlns:lom=\"http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource\" xmlns:imsmd=\"http://www.imsglobal.org/xsd/imsmd_v1p2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1 http://www.imsglobal.org/xsd/imscp_v1p1.xsd http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource http://www.imsglobal.org/profile/cc/ccv1p1/LOM/ccv1p1_lomresource_v1p0.xsd http://www.imsglobal.org/xsd/imsmd_v1p2 http://www.imsglobal.org/xsd/imsmd_v1p2p2.xsd\"", key2), 
                              block=TRUE))
  
  # write the file
  if (!dir.exists(subdir)) dir.create(subdir)
  f= file(sprintf("%s/imsmanifest.xml",subdir), "w")
  writeLines(output1, f)
  close(f)
}
