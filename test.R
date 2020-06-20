rm(list=ls())
engine_sources= c(
  "assessment_meta", "imsmanifest",
  "write_quiz_canvas_blockwrapper",
  "generate_key", "quizzip"
)


for (subsource in engine_sources) eval(parse(text=sprintf("source(\"%s.R\")", subsource)))

eval(parse(text="a= 5"))
                                 
                                 
source("generate_key.R")
