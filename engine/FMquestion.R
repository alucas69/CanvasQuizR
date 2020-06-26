#' Write a formula question to the quiz file
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

FMquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       svVariables,
                       mVariableboundsprecisions,
                       mValues,
                       iAnswerprecision,
                       counter, texify=FALSE) {

  #######################################
  #######################################
  ## Texify the question
  #######################################
  #######################################
  if (texify){
    intro_text= tex2math(intro_text)
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
  if (!is.matrix(answers)) answers <- cbind(answers, answers, answers)
  iNumberOfAnswers <- nrow(mValues)
  vAnswerIdentifiers <- paste(answer_counter + (1 : nrow(mValues)))
  WLprint(fileConn, sprintf("          <item ident=\"%s\" title=\"%s\">", qkey, qname))
  WLprint(fileConn, "            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>calculated_question</fieldentry>
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
          </outcomes>
          <respcondition title=\"correct\">
            <conditionvar>
              <other/>
            </conditionvar>
            <setvar varname=\"SCORE\" action=\"Set\">100</setvar>
          </respcondition>
          <respcondition title=\"incorrect\">
            <conditionvar>
              <not>
                <other/>
              </not>
            </conditionvar>
            <setvar varname=\"SCORE\" action=\"Set\">0</setvar>
          </respcondition>
        </resprocessing>
        <itemproc_extension>
          <calculated>
            <answer_tolerance>0</answer_tolerance>
            <formulas decimal_places=\"0\">
              <formula>3.14159265456</formula>
            </formulas>
            <vars>")
  for (i1 in 1:length(svVariables)) {
    WLprint(fileConn, sprintf("              <var name=\"%s\" scale=\"%d\">", svVariables[i1], round(mVariableboundsprecisions[3,i1])))
    WLprint(fileConn, sprintf("                <min>%d</min>", round(mVariableboundsprecisions[1,i1])))
    WLprint(fileConn, sprintf("                <max>%d</max>", round(mVariableboundsprecisions[2,i1])))
    WLprint(fileConn, sprintf("              </var>"))
  }
  WLprint(fileConn, "            <var_sets>")
  for (i1 in 1:nrow(mValues)) {
    WLprint(fileConn, sprintf("              <var_set ident=\"%s\">", vAnswerIdentifiers[i1]))
    for (i2 in 1:length(svVariables)) {
      WLprint(fileConn, sprintf(sprintf("              <var name=\"%%s\">%%1.%df</var>", round(mVariableboundsprecisions[3,i2])), svVariables[i2], mValues[i1,i2]))
    }
    WLprint(fileConn, sprintf(sprinft("                <answer>%%1.%df</answer>", iAnswerprecision), mValues[i2, ncol(mValues)]))
    WLprint(fileConn, "              </var_set>")
  }
  WLprint(ffileConn, "            </var_sets>
          </calculated>
        </itemproc_extension>
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
