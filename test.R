rm(list=ls())
engine_sources= c(
  #
  # auxiliary functions
  #
  "FloorDfT.R", "aidfunctions.R", "generate_key.R", "quizzip.R",
  "sprintf_indent.R", "tex2math.R",
  #
  # general quiz routines
  #
  "check_mb_answers.R", "check_mc_answers.R", "check_num_answers.R",
  "write_quiz_maketestquiz.R", "write_quiz_to_dataframe.R", "write_quiz_html_test.R", 
  #
  # canvas wrapper files
  #
  "assessment_meta.R", "imsmanifest.R", "write_quiz_canvas.R",
  "write_quiz_canvas_alph.R", "write_quiz_canvas_blockwrapper.R",
  "write_quiz_canvas_ma.R", "write_quiz_canvas_mc.R", "write_quiz_canvas_num.R",
  "write_quiz_canvas_question_preamble.R", "write_quiz_canvas_upl.R",
  #
  # html related
  #
  "html_escape.R", "simple_html_checker.R", "simple_html_matching_tag_checker.R",
  "write_as_html_table.R", "write_as_html_ul.R", "write_as_html_ul_color.R",
  "write_in_wrapper.R", "write_quiz_html.R", "write_quiz_html_alph.R",
  "write_quiz_html_file.R", "write_quiz_html_ma.R", "write_quiz_html_mb.R",
  "write_quiz_html_mb_highlight_variables.R", "write_quiz_html_mc.R",
  "write_quiz_html_num.R", "write_quiz_html_upl.R", "write_three_part_table.R"
)
for (subsource in engine_sources) eval(parse(text=sprintf("source(\"%s\")", subsource)))

write_quiz_html_test()
