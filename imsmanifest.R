#' Write the IMSMANIFEST.XML file for CANVAS
#'
#' This function creates the quiz wrapper file needed by Canvas.
#' @param subdir is the map where the files are placed, the contents of which need to be zipped and uploaded into CANVAS. The default is the current directory.
#' @param key is a string holding the identifier of the quiz.
#' @keywords say hello
#' @export key the key that is used is exported.
#' @examples
#' imsmanifest()

imsmanifest <- function(subdir = ".", key) {

  #############################################
  #############################################
  ## set additional keys
  #############################################
  #############################################
  keylength = 33
  key2 = stri_rand_strings(1, keylength)
  key3 = stri_rand_strings(1, keylength)

  #############################################
  #############################################
  ## write the file
  #############################################
  #############################################
  if (!dir.exists(sprintf("%s/%s", subdir, key))) dir.create(sprintf("%s/%s", subdir, key))
  fname = sprintf("%s/imsmanifest.xml", subdir)
  fileConn <- file(fname, "w")
  WLprint(fileConn, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  WLprint(fileConn, sprintf("<manifest identifier=\"%s\" xmlns=\"http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1\" xmlns:lom=\"http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource\" xmlns:imsmd=\"http://www.imsglobal.org/xsd/imsmd_v1p2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1 http://www.imsglobal.org/xsd/imscp_v1p1.xsd http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource http://www.imsglobal.org/profile/cc/ccv1p1/LOM/ccv1p1_lomresource_v1p0.xsd http://www.imsglobal.org/xsd/imsmd_v1p2 http://www.imsglobal.org/xsd/imsmd_v1p2p2.xsd\">", key2))
    
  WLprint(fileConn, "  <metadata>
    <schema>IMS Content</schema>
    <schemaversion>1.1.3</schemaversion>
    <imsmd:lom>
      <imsmd:general>
        <imsmd:title>
          <imsmd:string>QTI Quiz Export for canvas course</imsmd:string>
        </imsmd:title>
      </imsmd:general>
      <imsmd:lifeCycle>
        <imsmd:contribute>
          <imsmd:date>")
  WLprint(fileConn, sprintf("            <imsmd:dateTime>%s</imsmd:dateTime>", Sys.Date()))
  WLprint(fileConn, "          </imsmd:date>
        </imsmd:contribute>
      </imsmd:lifeCycle>
      <imsmd:rights>
        <imsmd:copyrightAndOtherRestrictions>
          <imsmd:value>yes</imsmd:value>
        </imsmd:copyrightAndOtherRestrictions>
        <imsmd:description>
          <imsmd:string>Private (Copyrighted) - http://en.wikipedia.org/wiki/Copyright</imsmd:string>
        </imsmd:description>
      </imsmd:rights>
    </imsmd:lom>
  </metadata>
  <organizations/>
  <resources>  ")
  WLprint(fileConn, sprintf("    <resource identifier=\"%s\" type=\"imsqti_xmlv1p2\">", key))
  WLprint(fileConn, sprintf("      <file href=\"%s/%s.xml\"/>", key, key))
  WLprint(fileConn, sprintf("      <dependency identifierref=\"%s\"/>", key3))
  WLprint(fileConn, "    </resource>")
  WLprint(fileConn, sprintf("    <resource identifier=\"%s\" type=\"associatedcontent/imscc_xmlv1p1/learning-application-resource\" href=\"%s/assessment_meta.xml\">", key3, key))
  WLprint(fileConn, sprintf("      <file href=\"%s/assessment_meta.xml\"/>", key))
  WLprint(fileConn, "    </resource>
  </resources>
</manifest>
  ")
  close(fileConn)
}
