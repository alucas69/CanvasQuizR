#' Write an essay question to the quiz file
#'
#' This function writes an essay question to the quiz file.
#' @param QuizFile is a list structure with elements QuizFile$fileConn the link to the open file where the question can be saved, and QuizFile$key a string holding the Canvas identifier for the quiz.
#' @param qname is a string holding the Canvas name for the question
#' @param intro_text is a vector of strings, holding the paragraphs of the quiz text.
#' @param counter is a list with elements $AnswerCounter holding the 5 digit identifier of the answers across all questions, and $QuestionCounter the question counter across all questions.
#' @keywords essay question for canvas quiz
#' @export counter holds the updated counters after the current question
#' @examples
#' ALPHquestion(QuizFile, "ca,aacooq", "q1", c("this is", "test"), 25)

ALPHquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       counter) {

  #######################################
  #######################################
  ## Start question
  #######################################
  #######################################
  answer_counter <- counter$AnswerCounter
  key = QuizFile$key
  qkey = paste(key, counter$QuestionCounter, sep = "")
  fileConn = QuizFile$fileConn
  iNumberOfAnswers <- 1
  vAnswerIdentifiers <- paste(answer_counter + 1)
  WLprint(fileConn, sprintf("          <item ident=\"%s\" title=\"%s\">", qkey, qname))
  WLprint(fileConn, "
            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>essay_question</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>points_possible</fieldlabel>
                  <fieldentry>1.0</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>original_answer_ids</fieldlabel>
                  <fieldentry></fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>assessment_question_identifierref</fieldlabel>
  ")
  WLprint(fileConn, sprintf("                <fieldentry>%s</fieldentry>", paste(qkey, "a", sep = "") ))
  WLprint(fileConn, "
                </qtimetadatafield>
              </qtimetadata>
            </itemmetadata>
            <presentation>
              <material>
                <mattext texttype=\"text/html\"><div>
  ")
  ######################################
  ## here comes the question
  ######################################
  for (i1 in 1:length(intro_text)) {
    WLprint(fileConn, sprintf("                <p>%s</p>", intro_text[i1]))
  }
  WLprint(fileConn, "
                </div></mattext>
              </material>
              <response_str ident=\"response1\" rcardinality=\"Single\">
                <render_fib>
                   <response_label ident=\"answer1\" rshuffle=\"No\"/>
                </render_fib>
              </response_str>
            </presentation>
            <resprocessing>
                <outcomes>
                  <decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>
                </outcomes>
                <respcondition continue=\"No\">
                  <conditionvar>
                    <other/>
                  </conditionvar>
                </respcondition>
              </resprocessing>
            </item>
  ")
  WLprint(fileConn, "")

  #######################################
  #######################################
  ## return the updated counter for
  ## answer identification
  #######################################
  #######################################
  return( list(AnswerCounter = answer_counter + 1, QuestionCounter = counter$QuestionCounter + 1) )
}
