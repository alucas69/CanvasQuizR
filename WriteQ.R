########################################
# Counters= WriteQ(Counters, Qi)
#
# Purpose:
#   Write a single question
#
# Return value:
#   Counters, updated list
WriteQ <- function(Qi, Counters){
  if (Qi$type == "mc"){
    Counters <- MCquestion(QuizFile, Qi$q, Qi$text,
                           Qi$answer, Qi$correct, Counters, texify=TRUE)
  } else if (Qi$type == "ma"){
    Counters <- MAquestion(QuizFile, Qi$q, Qi$text,
                           Qi$answer, Qi$correct, Counters, texify=TRUE)
  } else if (Qi$type == "num"){
    Counters <- NUMquestion(QuizFile, Qi$q, Qi$text,
                            Qi$correct, Counters, texify=TRUE)
  } else if (Qi$type == "upl"){
    Counters <- UPLOADquestion(QuizFile, Qi$q, Qi$text,
                            Counters, texify=TRUE, pointspossible=0)
  } else {
    print (sprintf("Dropping question %s of type %s", Qi$q, Qi$type))
  }

  # drop the question code from memory
  # eval(parse(text = sprintf("rm(%s_%s)", BaseFileName, WhichQuestions[i1])))
  return (Counters)
}
