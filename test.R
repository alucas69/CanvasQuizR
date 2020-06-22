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


# set up the quiz
qdir= "./2020resitv_1/questions"
numbervariations= 1
questions= matrix(c(
  numbervariations, "Q20200629_1a", 
  numbervariations, "Q20200629_1b", 
  numbervariations, "Q20200629_1c",
  numbervariations, "Q20200629_1d",
  numbervariations, "Q20200629_1e",
  numbervariations, "Q20200629_1g",
  numbervariations, "Q20200629_1h",
  numbervariations, "Q20200629_1i",
  numbervariations, "Q20200629_1j",
  numbervariations, "Q20200629_1l",
  numbervariations, "Q20200629_1m"
), nrow=2)

# load the sources
for (blockcounter in 1:ncol(questions)) {
  print(sprintf("source(\"%s/%s.R\")", qdir, questions[2, blockcounter]))
  eval(parse(text = sprintf("source(\"%s/%s.R\")", qdir, questions[2, blockcounter])))
}

# draw the quiz
exam = list()
for (blockcounter in 1:ncol(questions)) {
  block = list()
  for (questioncounter in 1:questions[1, blockcounter]) {
    print(sprintf("Question %d.%d", blockcounter, questioncounter))
    eval(parse(text = sprintf("question_tmp= %s()", questions[2, blockcounter])))
    question_tmp$q= sprintf("Q%d", blockcounter)
    block= append(block, list(question_tmp))
    
    #check html
    print("Checking html of question")
    print(question_tmp$text)
    simple_html_checker(question_tmp$text)
    print("Checking html of answer")
    if ((question_tmp$type == "ma") | (question_tmp$type == "mb") | (question_tmp$type == "mc")) simple_html_checker(question_tmp$answer)
  }
  exam= append(exam, list(block))
}

# write the quiz
write_quiz_html(exam)
write_quiz_canvas(exam)




