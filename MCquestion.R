#' Write a multiple answer question to the quiz file
#'
#' This function writes an multiple answer question to the quiz file.
#' @param QuizFile is a list structure with elements QuizFile$fileConn the link to the open file where the question can be saved, and QuizFile$key a string holding the Canvas identifier for the quiz.
#' @param qname is a string holding the Canvas name for the question
#' @param intro_text is a vector of strings, holding the paragraphs of the quiz text.
#' @param answer_text is a vector of strings, holding the answers.
#' @param answer_correct is a vector of booleans, holding which answer(s) are true. Put the correct ones at the top.
#' @param counter is a list with elements $AnswerCounter holding the 5 digit identifier of the answers across all questions, and $QuestionCounter the question counter across all questions.
#' @keywords multiple choice question for canvas quiz
#' @export counter holds the updated counters after the current question
#' @examples
#' MCquestion(QuizFile, "ca,aacooq", "q1", c("this is", "test"), c("yes", "no"), c(TRUE, FALSE), 25)

MCquestion <- function(QuizFile,
                       qname,
                       intro_text,
                       answer_text,
                       answer_correct,
                       counter, texify=FALSE, htmlesc=FALSE) {

  #######################################
  #######################################
  ## Texify the question and html escape
  #######################################
  #######################################
  if (htmlesc) {
    intro_text <- EscapeHtml(intro_text)
    answer_text <- EscapeHtml(answer_text)
  }
  if (texify){
    intro_text= tex2math(intro_text)
    answer_text= tex2math(answer_text)
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
  WLprint(fileConn, "
            <itemmetadata>
              <qtimetadata>
                <qtimetadatafield>
                  <fieldlabel>question_type</fieldlabel>
                  <fieldentry>multiple_choice_question</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>points_possible</fieldlabel>
                  <fieldentry>1.0</fieldentry>
                </qtimetadatafield>
                <qtimetadatafield>
                  <fieldlabel>original_answer_ids</fieldlabel>
  ")
  WLprint(fileConn, sprintf("                  <fieldentry>%s</fieldentry>", paste(vAnswerIdentifiers, collapse = ",") ))
  WLprint(fileConn, "
                  </qtimetadatafield>
                  <qtimetadatafield>
                    <fieldlabel>assessment_question_identifierref</fieldlabel>
  ")
  WLprint(fileConn, sprintf("                    <fieldentry>%s</fieldentry>", paste(qkey, "a", sep = "") ))
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
    WLprint(fileConn, sprintf("                    <p>%s</p>", intro_text[i1]))
  }
  WLprint(fileConn, "
                    </div></mattext>
                  </material>
  ")
  ######################################
  ## here come the answer type and answers
  ######################################
  WLprint(fileConn, "
                  <response_lid ident=\"response1\" rcardinality=\"Single\">
                    <render_choice>
  ")
  for (i1 in 1:iNumberOfAnswers) {
    WLprint(fileConn, sprintf("                    <response_label ident=\"%s\">", vAnswerIdentifiers[i1]))
    WLprint(fileConn, "                      <material>")
    WLprint(fileConn, sprintf("<mattext texttype=\"text/html\">%s</mattext>", answer_text[i1] ))
#    WLprint(fileConn, sprintf("                        <mattext texttype=\"text/plain\">%s</mattext>", answer_text[i1] ))
    WLprint(fileConn, "                      </material>")
    WLprint(fileConn, "                    </response_label>")
  }
  WLprint(fileConn, "
                    </render_choice>
                  </response_lid>
                </presentation>
                <resprocessing>
                <outcomes>
                  <decvar maxvalue=\"100\" minvalue=\"0\" varname=\"SCORE\" vartype=\"Decimal\"/>
                </outcomes>
                <respcondition continue=\"No\">
                  <conditionvar>
  ")
  ######################################
  ## here come the correct answer indicators
  ######################################
  answer_tmp <-vAnswerIdentifiers[ which(answer_correct == TRUE) ]
  if (length(answer_tmp) > 1) {
    cat(red(paste(rep(c(rep("*",50), "\n"), 3), collapse = "")))
    cat(red("** ERROR: MORE THAN 1 CORRECT ANSWER IN THIS MC"))
    cat(red(paste(rep(c(rep("*",50), "\n"), 3), collapse = "")))
    cbind(answer_correct, answer_text)
    stop("MC question multiple answers")
  } else {
    WLprint(fileConn, sprintf("                        <varequal respident=\"response1\">%s</varequal>", answer_tmp[1]))
  }
  WLprint(fileConn, "
                  </conditionvar>
                  <setvar action=\"Set\" varname=\"SCORE\">100</setvar>
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
  return( list(AnswerCounter = answer_counter + iNumberOfAnswers, QuestionCounter = counter$QuestionCounter + 1) )
}
