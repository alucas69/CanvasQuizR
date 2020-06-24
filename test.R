# clean up
rm(list=ls())


# libraries
library(stringi)
library(psych)
library(car)


# load engine
engine_sources= c(
  #
  # auxiliary functions
  #
  "FloorDfT.R", "aidfunctions.R", "generate_key.R", "quizzip.R",
  "sprintf_indent.R", "tex2math.R", "answer_select.R",
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



# load quiz sources
qdir= "./2020resitv_1/questions"
numbervariations= 50
exam_sources= c(
  "ID_question.R", "Q20200629_1nw-core.R", 
  "Q20200629_1a.R", "Q20200629_1b.R", "Q20200629_1c.R", "Q20200629_1d.R",
  "Q20200629_1e.R", "Q20200629_1g.R", "Q20200629_1h.R", "Q20200629_1i.R",
  "Q20200629_1j.R", "Q20200629_1l.R", "Q20200629_1m.R", "Q20200629_1n.R",
  "Q20200629_1o.R", "Q20200629_1p.R", "Q20200629_1q.R", "Q20200629_1r.R",
  "Q20200629_1s.R", "Q20200629_1u.R", "Q20200629_1v.R", "Q20200629_1w.R",
  "Q20200629_1x.R", "Q20200629_1y.R", "Q20200629_1z.R", 
  "Q_5step_anova.1.R"
)
for (subsource in exam_sources) eval(parse(text=sprintf("source(\"%s/%s\")", qdir, subsource)))
questions= matrix(c(
  # numbervariations, "Q20200629_1a",
  # numbervariations, "Q20200629_1b",
  # numbervariations, "Q20200629_1c",
  # numbervariations, "Q20200629_1d",
  # numbervariations, "Q20200629_1e",
  # numbervariations, "Q20200629_1g",
  # numbervariations, "Q20200629_1h",
  # numbervariations, "Q20200629_1i",
  # numbervariations, "Q20200629_1j",
  # numbervariations, "Q20200629_1l",
  # numbervariations, "Q20200629_1m",
  # numbervariations, "Q20200629_1n",
  # numbervariations, "Q20200629_1o",
  # numbervariations, "Q20200629_1p",
  # numbervariations, "Q20200629_1q",
  # numbervariations, "Q20200629_1r",
  # numbervariations, "Q20200629_1s",
  # numbervariations, "Q20200629_1t",
  # numbervariations, "Q20200629_1u",
  # numbervariations, "Q20200629_1v",
  # numbervariations, "Q20200629_1w",
  # numbervariations, "Q20200629_1x",
  # numbervariations, "Q20200629_1y",
  # numbervariations, "Q20200629_1z",
  numbervariations, "Q_5step_anova.1_step1a"
), nrow=2)


set.seed(55)


# construct the exam
exam = list()
for (blockcounter in 1:ncol(questions)) {
  block = list()
  for (questioncounter in 1:questions[1, blockcounter]) {
    print(sprintf("Question %d.%d", blockcounter, questioncounter))
    # print(questions[2,blockcounter])
    eval(parse(text = sprintf("question_tmp= %s()", questions[2, blockcounter])))
    question_tmp$q= sprintf("Q%d", blockcounter)
    block= append(block, list(question_tmp))
  }
  exam= append(exam, list(block))
}


# write the exam
write_quiz_html(exam, subdir="C:/Users/me/surfdrive/BSTAT/exams/tmp")
# write_quiz_canvas(exam, subdir="C:/Users/me/surfdrive/BSTAT/exams/tmp")




