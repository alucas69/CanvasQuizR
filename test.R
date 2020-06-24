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
numbervariations= 10
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
  # numbervariations, "Q_5step_anova.1_step1a",
  # numbervariations, "Q_5step_anova.1_step2a",
  # numbervariations, "Q_5step_anova.1_step3a",
  # numbervariations, "Q_5step_anova.1_step3b",
  # numbervariations, "Q_5step_anova.1_step3c",
  numbervariations, "Q_5step_anova.1_step5a"
  # numbervariations, "Q_5step_anova.1_step5b"
), nrow=2)


#set.seed(55)
# myprettycapture= function(s_command, l.aov, collapse=NULL, htmlescape=TRUE, convert_ampersand=FALSE, debug=FALSE, omitlast=0) {
#   # TODO: next line should work, but does not; if it does, eliminate l.aov argument
#   # then also change myprettyaovprint()
#   # output= paste(capture.output(eval.parent(parse(text = s_command), n=1)), collapse=collapse)
#   output=capture.output(eval(parse(text=s_command)))
#   if (debug) {
#     print("AFTER CAPTURE 1")
#     print(output)
#     print(typeof(output))
#     print(str(output))
#   }
#   outputp= stri_replace_all(output, "&apos;", regex="[‘’]")
#   if (debug) {
#     print("AFTER CAPTURE 2")
#     print(outputp)
#     print(typeof(outputp))
#     print(str(outputp))
#   }
#   output= stri_replace_all(output, "&quot;", regex="[\"]")
#   if (debug) {
#     print("AFTER CAPTURE 3")
#     print(output)
#     print(typeof(output))
#   }
#   if (omitlast > 0) output= output[1:(length(output)-omitlast)]
#   if (debug) {
#     print("AFTER CAPTURE 3b")
#     print(output)
#     print(typeof(output))
#   }
#   output= paste(output, collapse=collapse)
#   if (htmlescape) output= html_escape(output, convert_ampersand=convert_ampersand)
#   if (debug) {
#     print("AFTER CAPTURE 4")
#     print(output)
#     print(typeof(output))
#   }
#   if (debug) {print("IN PRETTYCAPTURE 5"); print(output)}
#   return(output)
# }
# 
# myprettyaovprint <- function(l.aov) {
#   # Output: list of 4 tables, 
#   #   $aov, with output of aov print
#   #   $aov_summary, with output of summary(aov) print
#   #   $aov_levene, with output of leveneTest(aov) print
#   #   $aov_tukey, with output of TukeyHSD(aov) print
#   aov=myprettycapture("l.aov", l.aov, collapse="\n")
#   # MYSTERY: this works in console
#   # a=data.frame(cbind(rnorm(50), sample(c(1,2),50, replace = TRUE))); a["X1"]=3*a["X2"]; b=aov(X1 ~ X2, data=a); o=capture.output(eval(parse(text="summary(b)"))); oo=stri_replace_all(o, "&apos;", regex="[‘’']"); oo
#   aov_summary=myprettycapture("summary(l.aov)", l.aov, collapse="\n", debug=TRUE, omitlast=TRUE)
#   print("IN AOVPRINT")
#   print(aov_summary)
#   aov_levene=myprettycapture("leveneTest(l.aov)", l.aov, collapse="\n")
#   aov_tukey=myprettycapture("TukeyHSD(l.aov)", l.aov, collapse="\n")
#   output=list(aov=aov, aov_summary=aov_summary, aov_levene=aov_levene, aov_tukey=aov_tukey)
#   return(output)
# }




# myprettycapture= function(s_command, l.aov, collapse=NULL, htmlescape=TRUE, convert_ampersand=FALSE) {
#   # TODO: next line should work, but does not; if it does, eliminate l.aov argument
#   # then also change myprettyaovprint()
#   # output= paste(capture.output(eval.parent(parse(text = s_command), n=1)), collapse=collapse)
#   output= capture.output(eval(parse(text = s_command)))
#   output= stri_replace_all(output, "&apos;", regex="[‘’']")
#   output= stri_replace_all(output, "&quot;", regex="[\"]")
#   output= paste(output, collapse=collapse)
#   if (htmlescape) output= html_escape(output, convert_ampersand=convert_ampersand)
#   return(output)
# }
# 
# myprettyaovprint <- function(l.aov) {
#   # Output: list of 4 tables, 
#   #   $aov, with output of aov print
#   #   $aov_summary, with output of summary(aov) print
#   #   $aov_levene, with output of leveneTest(aov) print
#   #   $aov_tukey, with output of TukeyHSD(aov) print
#   aov=myprettycapture("l.aov", l.aov, collapse="\n")
#   aov_summary=myprettycapture("summary(l.aov)", l.aov, collapse="\n")
#   aov_levene=myprettycapture("leveneTest(l.aov)", l.aov, collapse="\n")
#   aov_tukey=myprettycapture("TukeyHSD(l.aov)", l.aov, collapse="\n")
#   output=list(aov=aov, aov_summary=aov_summary, aov_levene=aov_levene, aov_tukey=aov_tukey)
#   return(output)
# }


a=data.frame(cbind(rnorm(50), sample(c(1,2),50, replace = TRUE)))
a["X1"]=3*a["X2"]; 
b=aov(X1 ~ X2, data=a)
o=capture.output(eval(parse(text="summary(b)")))
print(o)
oo=stri_replace_all(o, "&apos;", regex="[‘’']"); 
print(oo)
stop(0)




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
write_quiz_canvas(exam, subdir="C:/Users/me/surfdrive/BSTAT/exams/tmp")




