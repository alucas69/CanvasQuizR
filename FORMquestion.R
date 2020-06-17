#' Write a numerical answer question to the quiz file
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
#' NUMquestion(QuizFile, "ca,aacooq", "q1", c("this is", "test"), c("yes", "no"), c(TRUE, FALSE), 25)

NUMquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       answers,
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
  if (!is.matrix(answers)) answers <- cbind(answers, answers, answers)
  iNumberOfAnswers <- nrow(answers)
  vAnswerIdentifiers <- paste(answer_counter + (1 : iNumberOfAnswers))
  WLprint(fileConn, sprintf("          <item ident=\"%s\" title=\"%s\">", qkey, qname))
  WLprint(fileConn, "            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>numerical_question</fieldentry>
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
                  <material>")
  ######################################
  ## here comes the question
  ######################################
  for (i1 in 1:length(intro_text)) {
    WLprint(fileConn, sprintf("     <mattext texttype=\"text/html\">&lt;div&gt;&lt;p&gt;%s&lt;/p&gt;&lt;/div&gt;</mattext>", intro_text[i1]))
  }
  WLprint(fileConn, "          </material>  ")
  ######################################
  ## here come the answer type and answers
  ######################################
  WLprint(fileConn, "
            <response_str ident=\"response1\" rcardinality=\"Single\">
              <render_fib fibtype=\"Decimal\">
                <response_label ident=\"answer1\"/>
              </render_fib>
            </response_str>
          </presentation>
          <resprocessing>
            <outcomes>
              <decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>
            </outcomes>")
  for (i1 in 1:iNumberOfAnswers) {
    WLprint(fileConn, "
            <respcondition continue=\"No\">
              <conditionvar>
                <or>")
    WLprint(fileConn, sprintf("                  <varequal respident=\"response1\">%f</varequal>", answers[i1,1]))
    WLprint(fileConn, "                  <and>")
    WLprint(fileConn, sprintf("                  <vargte respident=\"response1\">%f</vargte>", answers[i1,2]))
    WLprint(fileConn, sprintf("                  <varlte respident=\"response1\">%f</varlte>", answers[i1,3]))
    WLprint(fileConn, "                  </and>
                </or>
              </conditionvar>
              <setvar action=\"Set\" varname=\"SCORE\">100</setvar>
            </respcondition>")
  }
  WLprint(fileConn, "          </resprocessing>
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
