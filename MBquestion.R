#' Write a multiple blanks question to the quiz file
#'
#' This function writes an numerical answer question to the quiz file.
#' @param QuizFile is a list structure with elements QuizFile$fileConn the link to the open file where the question can be saved, and QuizFile$key a string holding the Canvas identifier for the quiz.
#' @param qname is a string holding the Canvas name for the question
#' @param intro_text is a vector of strings, holding the paragraphs of the quiz text. There may be multiple questions asked, each having their own numerical answer.
#' @param vAnswerNames is a p vector holding the names of the blank fields as used in intro_text.
#' @param answer_text is a p vector, where p is the number of blanks.
#' @param counter is a list with elements $AnswerCounter holding the 5 digit identifier of the answers across all questions, and $QuestionCounter the question counter across all questions.
#' @keywords numerical answer question for canvas quiz
#' @export counter holds the updated counters after the current question
#' @examples
#' NUMquestion(QuizFile, "ca,aacooq", "q1", c("this is", "test"), c("yes", "no"), c(TRUE, FALSE), 25)

MBquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       vAnswerNames,
                       answer_text,
                       counter, texify=FALSE, htmlesc=FALSE) {

  #######################################
  #######################################
  ## Texify the question and html escape
  #######################################
  #######################################
  if (htmlesc) {
    intro_text <- EscapeHtml(intro_text)
    # answer_text <- EscapeHtml(answer_text)
  }
  if (texify){
    intro_text= tex2math(intro_text)
    # answer_text= tex2math(answer_text)
  }
  
  #######################################
  #######################################
  ## Start question
  #######################################
  #######################################
  answer_counter <- counter$AnswerCounter
  key = QuizFile$key
  qkey = paste(key, counter$QuestionCounter, sep = "")
  fileConn = QuizFile$fileConn
  iNumberOfAnswers <- length(answer_text)
  vAnswerIdentifiers <- paste(answer_counter + (1 : iNumberOfAnswers))
  WLprint(fileConn, sprintf("          <item ident=\"%s\" title=\"%s\">", qkey, qname))
  WLprint(fileConn, "            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>fill_in_multiple_blanks_question</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>points_possible</fieldlabel>
                  <fieldentry>1.0</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>original_answer_ids</fieldlabel>  ")
  WLprint(fileConn, sprintf("                  <fieldentry>%s</fieldentry>", paste(vAnswerIdentifiers, collapse = ",") ))
  WLprint(fileConn, "                  </qtimetadatafield>
                  <qtimetadatafield>
                    <fieldlabel>assessment_question_identifierref</fieldlabel>  ")
  WLprint(fileConn, sprintf("                    <fieldentry>%s</fieldentry>", paste(qkey, "a", sep = "") ))
  WLprint(fileConn, "                    </qtimetadatafield>
                  </qtimetadata>
                </itemmetadata>
                <presentation>
                  <material>
                    <mattext texttype=\"text/html\"><div>")
  ######################################
  ## here comes the question
  ######################################
  for (i1 in 1:length(intro_text)) {
    WLprint(fileConn, sprintf("     <p>%s</p>", intro_text[i1]))
  }
  WLprint(fileConn, "            </div></mattext>
          </material>  ")
  ######################################
  ## here come the answer type and answers
  ######################################
  for (i1 in 1:length(vAnswerNames)) {
    WLprint(fileConn, sprintf("          <response_lid ident=\"response_%s\">", vAnswerNames[i1]))
    WLprint(fileConn, "            <material>")
    WLprint(fileConn, sprintf("          <mattext>%s</mattext>", vAnswerNames[i1]))
    WLprint(fileConn, "            </material>
            <render_choice>")
    WLprint(fileConn, sprintf("              <response_label ident=\"%s\">", vAnswerIdentifiers[i1]))
    WLprint(fileConn, sprintf("                <material>
                  <mattext texttype=\"text/html\">%s</mattext>
                </material>
              </response_label>", answer_text[i1]))
    WLprint(fileConn, "            </render_choice>
          </response_lid>")
  }
  WLprint(fileConn, "        </presentation>
        <resprocessing>
          <outcomes>
            <decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>
          </outcomes>")
  for (i1 in 1:length(vAnswerNames)) {
    WLprint(fileConn, sprintf("          <respcondition>
            <conditionvar>
              <varequal respident=\"response_%s\">%s</varequal>", vAnswerNames[i1], vAnswerIdentifiers[i1]))
    WLprint(fileConn, "            </conditionvar>")
    WLprint(fileConn, sprintf("            <setvar varname=\"SCORE\" action=\"Add\">%1.2f</setvar>", round(100 / iNumberOfAnswers, digits = 2)))
    WLprint(fileConn, "      </respcondition>")
  }
  WLprint(fileConn, "    </resprocessing>
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
