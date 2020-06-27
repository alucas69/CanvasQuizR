# clean up
rm(list=ls())
engine_dir="../engine"
question_dir="../questions"
rnd_seed= 55
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
for (exam_source in engine_sources) eval(parse(text=sprintf("source(\"%s/%s\")", engine_dir, exam_source)))



# load quiz sources
variation_count= 1
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
for (exam_source in exam_sources) eval(parse(text=sprintf("source(\"%s/%s\")", question_dir, exam_source)))

questions= matrix(c(
  variation_count, "Q20200629_1a",
  variation_count, "Q20200629_1b",
  variation_count, "Q20200629_1e",
  variation_count, "Q20200629_1h",
  variation_count, "Q20200629_1i",
  variation_count, "Q20200629_1j",
  variation_count, "Q20200629_1o",
  variation_count, "Q20200629_1q",
  variation_count, "Q20200629_1t",
  variation_count, "Q20200629_1u",
  variation_count, "Q20200629_1w",
  variation_count, "Q_5step_anova.1_step1a",
  variation_count, "Q_5step_anova.1_step2a",
  variation_count, "Q_5step_anova.1_step3a",
  variation_count, "Q_5step_anova.1_step3b",
  variation_count, "Q_5step_anova.1_step3c",
  variation_count, "Q_5step_anova.1_step5a",
  variation_count, "Q_5step_anova.1_step5b",
  variation_count, "Q_5step_2pi.1_step1a",
  variation_count, "Q_5step_2pi.1_step2a",
  variation_count, "Q_5step_2pi.1_step3a",
  variation_count, "Q_5step_2pi.1_step3b",
  variation_count, "Q_5step_2pi.1_step3c",
  variation_count, "Q_5step_2pi.1_step4a",
  variation_count, "Q_5step_2pi.1_step4b",
  variation_count, "Q_5step_2pi.1_step5a",
  variation_count, "Q_5step_regression.1.a1",
  variation_count, "Q_5step_regression.1.a2",
  #! numbervariations, "Q_5step_regression.1.a3",
  variation_count, "Q_5step_regression.1.c2",
  variation_count, "Q_5step_regression.1.c3",
  #! numbervariations, "Q_5step_regression.1.c4",
  variation_count, "Q_5step_regression.1.c5",
  #w numbervariations, "Q_5step_regression.1.c6",
  #w numbervariations, "Q_5step_regression.1.c7",
  variation_count, "Q_5step_regression.1.c8",
  NULL
), nrow=2)


# set the exam key
#examkey= paste("E_IBA1_BS_20200629_", "", "_rnd", rnd_seed, sep="")
exam_key= paste("E_IBA1_BS_20200629_", generate_key(4), "_rnd", rnd_seed, sep="")


# set seed and retain for storage if set
if (exists("rnd_seed")) {
  rnd_seed= abs(as.integer(rnd_seed))
  set.seed(abs(rnd_seed))
} else rnd_seed= NULL


# construct the exam
exam = list(blocks=list(), 
            seed=rnd_seed,
            emergency_html=emergency_message, 
            key = exam_key,
            name="R generated quiz")
for (block_number in 1:ncol(questions)) {
  block = list(questions=list(), name=sprintf("Question %d", block_number))
  for (question_number in 1:questions[1, block_number]) {
    print(sprintf("Question %d.%d", block_number, question_number))
    # print(questions[2,block_number])
    eval(parse(text = sprintf("question_tmp= %s()", questions[2, block_number])))
    question_tmp$q= sprintf("Q%d", block_number)
    block$questions= append(block$questions, list(question_tmp))
  }
  exam$blocks= append(exam$blocks, list(block))
}


# write the exam
write_quiz_html(exam, subdir="../build")
#write_quiz_canvas(exam, subdir="../../tmp")
