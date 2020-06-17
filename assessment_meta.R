#' Write the ASSESSMENT_META.XML file for CANVAS
#'
#' This function creates the assessment general introduction and set-up file needed by Canvas.
#' @param subdir is the map where the files are placed, the contents of which need to be zipped and uploaded into CANVAS. The default is the current directory.
#' @param key is a string holding the key ( = name) of the main quiz file.
#' @param title is a string holding the title of the test.
#' @param intro_text is a vector of strings, holding the introductory text before the quiz starts. Each element is a paragraph.
#' @param randomizeMC is a Boolean. If true, Canvas randomizes the order of MC and MA questions.
#' @keywords canvas quiz
#' @export NONE no outputs
#' @examples
#' assessment_meta(".", "riaic_id", "quiz1", "Good Luck!")

assessment_meta <- function(subdir = ".", key = "", title = "R generated quiz", 
                            intro_text = "", randomizeMC = FALSE) {

  #############################################
  #############################################
  ## generate auxiliary keys
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
  if (!dir.exists(sprintf("%s/non_cc_assessments", subdir))) dir.create(sprintf("%s/non_cc_assessments", subdir))

  fname = sprintf("%s/%s/assessment_meta.xml", subdir, key)
  fileConn <- file(fname, "w")
  WLprint(fileConn, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  WLprint(fileConn, sprintf("<quiz identifier=\"%s\" xmlns=\"http://canvas.instructure.com/xsd/cccv1p0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://canvas.instructure.com/xsd/cccv1p0 https://canvas.instructure.com/xsd/cccv1p0.xsd\">", key))
  WLprint(fileConn, sprintf("  <title>%s</title>", title))
  WLprint(fileConn, "  <description>")
  for (i1 in 1:length(intro_text)) {
    WLprint(fileConn, sprintf("     &lt;p&gt;%s&lt;/p&gt;", intro_text[i1]))
  }
  #############################################
  #############################################
  ## some quiz settings; double check in canvas
  #############################################
  #############################################
  WLprint(fileConn, "    </description>
    <lock_at>2020-02-26T12:00:00</lock_at>
    <unlock_at>2020-02-26T10:00:00</unlock_at>")
  WLprint(fileConn, sprintf("    <shuffle_answers>%s</shuffle_answers>", c("true","false")[1 + (randomizeMC == FALSE)]))
  WLprint(fileConn, "    <scoring_policy>keep_highest</scoring_policy>
    <hide_results></hide_results>
    <quiz_type>assignment</quiz_type>
    <points_possible>4.0</points_possible>
    <require_lockdown_browser>false</require_lockdown_browser>
    <require_lockdown_browser_for_results>false</require_lockdown_browser_for_results>
    <require_lockdown_browser_monitor>false</require_lockdown_browser_monitor>
    <lockdown_browser_monitor_data/>
    <access_code></access_code>
    <show_correct_answers></show_correct_answers>
    <anonymous_submissions>false</anonymous_submissions>
    <could_be_locked>false</could_be_locked>
    <time_limit>120</time_limit>
    <allowed_attempts>1</allowed_attempts>
    <one_question_at_a_time>true</one_question_at_a_time>
    <cant_go_back>true</cant_go_back>
    <available>false</available>
    <one_time_results>false</one_time_results>
    <show_correct_answers_last_attempt>false</show_correct_answers_last_attempt>
    <only_visible_to_overrides>false</only_visible_to_overrides>
    <module_locked>false</module_locked>  ")
  WLprint(fileConn, sprintf("    <assignment identifier=\"%s\">", key2))
  WLprint(fileConn, sprintf("    <title>%s</title>",title))
  WLprint(fileConn, "    <due_at/>")
  # WLprint(fileConn, "
  #   <lock_at>2020-02-26T12:00:00</lock_at>
  #   <unlock_at>2020-02-26T10:00:00</unlock_at>
  # ")
  WLprint(fileConn, "
    <module_locked>false</module_locked>
    <workflow_state>published</workflow_state>
    <assignment_overrides>
    </assignment_overrides>
  ")
  WLprint(fileConn, sprintf("    <quiz_identifierref>%s</quiz_identifierref>", key))
  WLprint(fileConn, "
    <allowed_extensions></allowed_extensions>
    <has_group_category>false</has_group_category>
    <points_possible>4.0</points_possible>
    <grading_type>points</grading_type>
    <all_day>false</all_day>
    <submission_types>online_quiz</submission_types>
    <position>25</position>
    <turnitin_enabled>false</turnitin_enabled>
    <vericite_enabled>false</vericite_enabled>
    <peer_review_count>0</peer_review_count>
    <peer_reviews>false</peer_reviews>
    <automatic_peer_reviews>false</automatic_peer_reviews>
    <anonymous_peer_reviews>false</anonymous_peer_reviews>
    <grade_group_students_individually>false</grade_group_students_individually>
    <freeze_on_copy>false</freeze_on_copy>
    <omit_from_final_grade>false</omit_from_final_grade>
    <intra_group_peer_reviews>false</intra_group_peer_reviews>
    <only_visible_to_overrides>false</only_visible_to_overrides>
    <post_to_sis>false</post_to_sis>
    <moderated_grading>false</moderated_grading>
    <grader_count>0</grader_count>
    <grader_comments_visible_to_graders>true</grader_comments_visible_to_graders>
    <anonymous_grading>false</anonymous_grading>
    <graders_anonymous_to_graders>false</graders_anonymous_to_graders>
    <grader_names_visible_to_final_grader>true</grader_names_visible_to_final_grader>
    <anonymous_instructor_annotations>false</anonymous_instructor_annotations>
    <post_policy>
      <post_manually>true</post_manually>
    </post_policy>
  </assignment>
  ")
  WLprint(fileConn, sprintf("  <assignment_group_identifierref>%s</assignment_group_identifierref>", key3))
  WLprint(fileConn, "
  <assignment_overrides>
  </assignment_overrides>
</quiz>
  ")
  close(fileConn)

}
