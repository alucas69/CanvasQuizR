# clean up
rm(list=ls())
enginedir="../engine"
questiondir="../questions"
rndseed= 55
emergency_message= "In case of emergency, mail a.lucas@vu.nl."

# check for emergency message 
if (!exists("emergency_message")) stop("FULL STOP: an emergency message needs to be set for students to contact you")


# libraries
library(stringi)
library(psych)
library(car)
# library(tools)


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
for (subsource in engine_sources) eval(parse(text=sprintf("source(\"%s/%s\")", enginedir, subsource)))



# load quiz sources
numbervariations= 1
exam_sources= c(
  "ID_question.R", "Q20200629_1nw-core.R", 
  "Q20200629_1a.R", "Q20200629_1b.R", "Q20200629_1c.R", "Q20200629_1d.R",
  "Q20200629_1e.R", "Q20200629_1g.R", "Q20200629_1h.R", "Q20200629_1i.R",
  "Q20200629_1j.R", "Q20200629_1l.R", "Q20200629_1m.R", "Q20200629_1n.R",
  "Q20200629_1o.R", "Q20200629_1p.R", "Q20200629_1q.R", "Q20200629_1r.R",
  "Q20200629_1s.R", "Q20200629_1t.R", "Q20200629_1u.R", "Q20200629_1v.R", 
  "Q20200629_1w.R", "Q20200629_1x.R", "Q20200629_1y.R", "Q20200629_1z.R", 
  "Q_5step_2pi.1.R", "Q_5step_anova.1.R", "Q_5step_regression.1.R"

)
for (subsource in exam_sources) eval(parse(text=sprintf("source(\"%s/%s\")", questiondir, subsource)))

questions= matrix(c(
  # numbervariations, "Q20200629_1a",
  # numbervariations, "Q20200629_1b",
  # numbervariations, "Q20200629_1e",
  # numbervariations, "Q20200629_1h",
  # numbervariations, "Q20200629_1i",
  # numbervariations, "Q20200629_1j",
  # numbervariations, "Q20200629_1o",
  # numbervariations, "Q20200629_1q",
  # numbervariations, "Q20200629_1t",
  # numbervariations, "Q20200629_1u",
  # numbervariations, "Q20200629_1w",
  # numbervariations, "Q_5step_anova.1_step1a",
  # numbervariations, "Q_5step_anova.1_step2a",
  # numbervariations, "Q_5step_anova.1_step3a",
  # numbervariations, "Q_5step_anova.1_step3b",
  # numbervariations, "Q_5step_anova.1_step3c",
  # numbervariations, "Q_5step_anova.1_step5a",
  # numbervariations, "Q_5step_anova.1_step5b",
  # numbervariations, "Q_5step_2pi.1_step1a",
  # numbervariations, "Q_5step_2pi.1_step2a",
  # numbervariations, "Q_5step_2pi.1_step3a",
  # numbervariations, "Q_5step_2pi.1_step3b",
  # numbervariations, "Q_5step_2pi.1_step3c",
  # numbervariations, "Q_5step_2pi.1_step4a",
  # numbervariations, "Q_5step_2pi.1_step4b",
  # numbervariations, "Q_5step_2pi.1_step5a",
  numbervariations, "Q_5step_regression.1.a1",
  # numbervariations, "Q_5step_regression.1.a2",
  # numbervariations, "Q_5step_regression.1.a3",
  # numbervariations, "Q_5step_regression.1.c2",
  # numbervariations, "Q_5step_regression.1.c3",
  # numbervariations, "Q_5step_regression.1.c4",
  # numbervariations, "Q_5step_regression.1.c5",
  # numbervariations, "Q_5step_regression.1.c6",
  # numbervariations, "Q_5step_regression.1.c7",
  # numbervariations, "Q_5step_regression.1.c8",
  NULL
), nrow=2)


# set the exam key
examkey= paste("E_IBA1_BS_20200629_", generate_key(4), "_rnd", rndseed, sep="")


# set seed and retain for storage if set
if (exists("rndseed")) {
  rndseed= abs(as.integer(rndseed))
  set.seed(abs(rndseed))
} else rndseed= NULL


# construct the exam
exam = list(blocks=list(), 
            seed=rndseed, 
            emergency_html=emergency_message, 
            key = examkey, 
            name="R generated quiz")
for (blockcounter in 1:ncol(questions)) {
  block = list(questions=list(), name=sprintf("Question %d", blockcounter))
  for (questioncounter in 1:questions[1, blockcounter]) {
    print(sprintf("Question %d.%d", blockcounter, questioncounter))
    # print(questions[2,blockcounter])
    eval(parse(text = sprintf("question_tmp= %s()", questions[2, blockcounter])))
    question_tmp$q= sprintf("Q%d", blockcounter)
    block$questions= append(block$questions, list(question_tmp))
  }
  exam$blocks= append(exam$blocks, list(block))
}


# write the exam
write_quiz_html(exam)
#write_quiz_canvas(exam, subdir="C:/Users/me/surfdrive/BSTAT/exams/tmp")
