#' Write the ASSESSMENT_META.XML file for CANVAS
#'
#' This function creates the assessment general introduction and set-up file needed by Canvas.
#' @param quiz_key is a string holding the key ( = name) of the main quiz file.
#' @param subdir is the map where the files are placed, the contents of which need to be zipped and uploaded into CANVAS. The default is the current directory.
#' @param title is a string holding the title of the test.
#' @param intro_text is a vector of strings, holding the introductory text before the quiz starts. Each element is a paragraph.
#' @param randomizeMC is a Boolean. If true, Canvas randomizes the order of MC and MA questions.
#' @keywords canvas quiz
#' @export NONE no outputs
#' @examples
#' assessment_meta(".", "riaic_id", "quiz1", "Good Luck!")

assessment_meta <- function(quiz_key, subdir = ".", title = "R generated quiz", 
                            intro_text = "", randomizeMC = FALSE) {

  # check intro text on correct html
  simple_html_checker(intro_text)
  
  # generate auxiliary keys
  keylength = 33
  key2 = stri_rand_strings(1, keylength)
  key3 = stri_rand_strings(1, keylength)

  # initialize the output
  output1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  
  # level 2: in quiz we find many loose settings
  {
    output2= c(
      write_in_wrapper(sprintf("%s", title), "title"),
      write_in_wrapper(html_escape(write_in_wrapper(intro_text, "p")), "description", block=TRUE),
      write_in_wrapper(tolower(randomizeMC == TRUE), "shuffle_answers"),
      write_in_wrapper("keep_highest", "scoring_policy"),
      write_in_wrapper("always", "hide_results"),
      write_in_wrapper("assignment", "quiz_type"),
      write_in_wrapper("1.0", "points_possible"),
      write_in_wrapper("false", "require_lockdown_browser"),
      write_in_wrapper("false", "require_lockdown_browser_for_results"),
      write_in_wrapper("false", "require_lockdown_browser_monitor"),
      "<lockdown_browser_monitor_data/>",
      write_in_wrapper("false", "show_correct_answers"),
      write_in_wrapper("false", "anonymous_submissions"),
      write_in_wrapper("false", "could_be_locked"),
      write_in_wrapper("false", "disable_timer_autosubmission"),
      write_in_wrapper("1", "allowed_attempts"),
      write_in_wrapper("false", "one_question_at_a_time"),
      write_in_wrapper("false", "cant_go_back"),
      write_in_wrapper("false", "available"),
      write_in_wrapper("false", "one_time_results"),
      write_in_wrapper("false", "show_correct_answers_last_attempt"),
      write_in_wrapper("false", "only_visible_to_overrides"),
      write_in_wrapper("false", "module_locked")
    )
    
    # level 3: assignment settings
    {
      output3= c(
        write_in_wrapper(title, "title"),
        "<due_at/>",
        "<lock_at/>",
        "<unlock_at/>",
        write_in_wrapper("false", "module_locked"),
        write_in_wrapper("unpublished", "workflow_state"),
        write_in_wrapper("", "assignment_overrides"),
        write_in_wrapper(quiz_key, "quiz_identifierref"),
        write_in_wrapper("", "allowed_extensions"),
        write_in_wrapper("false", "has_group_category"),
        write_in_wrapper("1.0", "points_possible"),
        write_in_wrapper("points", "grading_type"),
        write_in_wrapper("false", "all_day"),
        write_in_wrapper("online_quiz", "submission_types"),
        write_in_wrapper("1", "position"),
        write_in_wrapper("false", "turnitin_enabled"),
        write_in_wrapper("false", "vericite_enabled"),
        write_in_wrapper("0", "peer_review_count"),
        write_in_wrapper("false", "peer_reviews"),
        write_in_wrapper("false", "automatic_peer_reviews"),
        write_in_wrapper("false", "anonymous_peer_reviews"),
        write_in_wrapper("false", "grade_group_students_individually"),
        write_in_wrapper("false", "freeze_on_copy"),
        write_in_wrapper("false", "omit_from_final_grade"),
        write_in_wrapper("false", "intra_group_peer_reviews"),
        write_in_wrapper("false", "only_visible_to_overrides"),
        write_in_wrapper("false", "post_to_sis"),
        write_in_wrapper("false", "moderated_grading"),
        write_in_wrapper("0", "grader_count"),
        write_in_wrapper("true", "grader_comments_visible_to_graders"),
        write_in_wrapper("false", "anonymous_grading"),
        write_in_wrapper("false", "graders_anonymous_to_graders"),
        write_in_wrapper("true", "grader_names_visible_to_final_grader"),
        write_in_wrapper("false", "anonymous_instructor_annotations"),
        write_in_wrapper(write_in_wrapper("false", "post_manually"), "post_policy", block=TRUE)
      )
    }
    
    output2= c(output2, write_in_wrapper(output3, "assignment", s_wrappertag=sprintf("identifier=\"%s\"", key2) , block=TRUE))
    output2= c(output2, write_in_wrapper(sprintf("%s", key3), "assignment_group_identifierref"))
    output2= c(output2, write_in_wrapper("", "assignment_overrides"))
  }
  
  output1= c(output1, 
             write_in_wrapper(output2, "quiz", s_wrappertag=sprintf("identifier=\"%s\" xmlns=\"http://canvas.instructure.com/xsd/cccv1p0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://canvas.instructure.com/xsd/cccv1p0 https://canvas.instructure.com/xsd/cccv1p0.xsd\"", quiz_key),
                              block=TRUE))

  # write the file
  if (!dir.exists(sprintf("%s/%s", subdir, quiz_key))) dir.create(sprintf("%s/%s", subdir, quiz_key))
  if (!dir.exists(sprintf("%s/non_cc_assessments", subdir))) dir.create(sprintf("%s/non_cc_assessments", subdir))
  f= file(sprintf("%s/%s/assessment_meta.xml", subdir, quiz_key), "w")
  writeLines(output1, f)
  close(f)

}
