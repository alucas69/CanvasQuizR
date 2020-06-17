#' Write an file upload question to the quiz file
#'
#' This function writes an numerical answer question to the quiz file.
#' @param QuizFile is a list structure with elements QuizFile$fileConn the link to the open file where the question can be saved, and QuizFile$key a string holding the Canvas identifier for the quiz.
#' @param qname is a string holding the Canvas name for the question
#' @param intro_text is a vector of strings, holding the paragraphs of the quiz text. There may be multiple questions asked, each having their own numerical answer.
#' @param answers is a p x 3 matrix or a p vector, where p is the number of answers. If a vector, then no margin of error is allowed. Otherwise, the first column contains the answer, the second answer the lower bound for credits, and the third column the upper bound for credits.
#' @param counter is a list with elements $AnswerCounter holding the 5 digit identifier of the answers across all questions, and $QuestionCounter the question counter across all questions.
#' @keywords numerical answer question for canvas quiz
#' @export counter holds the updated counters after the current question
#' @examples
#' UPLOADquestion(QuizFile, "ca,aacooq", "q1", c("this is", "test"), c("yes", "no"), c(TRUE, FALSE), 25)

UPLOADquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       counter,
                       texify=FALSE, htmlesc=FALSE,
                       pointspossible = 0) {

  #######################################
  #######################################
  ## Start question
  #######################################
  #######################################
  if (htmlesc) {
    intro_text <- EscapeHtml(intro_text)
  }
  if (texify){
    intro_text= tex2math(intro_text)
  }
  answer_counter <- counter$AnswerCounter
  key = QuizFile$key
  qkey = paste(key, counter$QuestionCounter, sep = "")
  fileConn = QuizFile$fileConn
  iNumberOfAnswers <- 1
  vAnswerIdentifiers <- paste(answer_counter + (1 : iNumberOfAnswers))
  WLprint(fileConn, sprintf("          <item ident=\"%s\" title=\"%s\">", qkey, qname))
  WLprint(fileConn, "            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>file_upload_question</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>points_possible</fieldlabel>")
  WLprint(fileConn, sprintf("                  <fieldentry>%d.0</fieldentry>", pointspossible))
  WLprint(fileConn,"              </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>original_answer_ids</fieldlabel>  ")
#  WLprint(fileConn, sprintf("                  <fieldentry>%s</fieldentry>", paste(vAnswerIdentifiers, collapse = ",") ))
  WLprint(fileConn, "                  <fieldentry></fieldentry>")
  WLprint(fileConn, "                  </qtimetadatafield>
                  <qtimetadatafield>
                    <fieldlabel>assessment_question_identifierref</fieldlabel>  ")
  WLprint(fileConn, sprintf("                    <fieldentry>%s</fieldentry>", paste(qkey, "a", sep = "") ))
  WLprint(fileConn, "                    </qtimetadatafield>
                  </qtimetadata>
                </itemmetadata>
                <presentation>
                  <material>
                    <mattext texttype=\"text/html\">&lt;div&gt;")
  ######################################
  ## here comes the question
  ######################################
  for (i1 in 1:length(intro_text)) {
    WLprint(fileConn, sprintf("&lt;p&gt;%s&lt;/p&gt;", intro_text[i1]))
  }
  WLprint(fileConn, "            &lt;/div&gt;</mattext>
                  </material>  
          </presentation>
          <resprocessing>
            <outcomes>
              <decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>
            </outcomes>
          </resprocessing>
        </item>")
  WLprint(fileConn, "")

  #######################################
  #######################################
  ## return the updated counter for
  ## answer identification
  #######################################
  #######################################
  return( list(AnswerCounter = answer_counter + iNumberOfAnswers, QuestionCounter = counter$QuestionCounter + 1) )
}
