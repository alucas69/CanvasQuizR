#############################################################################
# answer_select(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
# Purpose:
#   Select iAlt of the answers, ensuring that at least mincorrect of the correct answers
#   are included, and at most maxcorrect answers.
#   Add a 'none of the above', unless ruled out.
answer_select <- function(vAnswers, vCorrect, iAlt=NULL, addnone=TRUE, maxcorrect=NULL, mincorrect=NULL, alphorder=FALSE){

  # initialize
  vCorrect= as.logical(vCorrect)
  iTotal= length(vAnswers)
  iTotalTrue= sum(as.integer(vCorrect))
  iTotalFalse= sum(1 - as.integer(vCorrect))
  
  # set null arguments if needed
  if (is.null(iAlt)) iAlt=iTotal
  if (is.null(mincorrect)) mincorrect= as.integer(addnone==FALSE)
  if (is.null(maxcorrect)) maxcorrect= iAlt-(addnone==FALSE)

  # minimum one correct, and one false answer; warn if change implemented
  if (mincorrect < as.integer(addnone==FALSE)) {
    warning("WARNING: increased mincorrect to have at least one correct answer", immediate. = TRUE)
    mincorrect= as.integer(addnone==FALSE)
  }
  if (maxcorrect > iAlt-(addnone==FALSE)) {
    warning("WARNING: decreased maxcorrect to have at least one false answer", immediate. = TRUE)
    maxcorrect= iAlt-(addnone==FALSE)
  }

  # check formats and possibility
  if (length(vAnswers) != length(vCorrect)) stop("ERROR: vAnswers and vCorrect of unequal size\n", paste(vAnswers, collapse = ","), "\n", paste(vCorrect, collapse = ","))
  if (iAlt <= 1) stop("ERROR: only one alternative possible")
  if (mincorrect > iTotalTrue) stop(sprintf("ERROR: mincorrect (%d) > #correct answers\n", mincorrect), paste(vAnswers, collapse = ","), "\n", paste(vCorrect, collapse = ","))
  if (iAlt-maxcorrect > iTotalFalse) stop(sprintf("ERROR: iAlt-maxcorrect (%d) > #incorrect answers\n", iAlt-maxcorrect), paste(vAnswers, collapse = ","), "\n", paste(vCorrect, collapse = ","))
  mincorrect= max(as.integer(addnone==FALSE), mincorrect, iAlt-iTotalFalse)
  maxcorrect= min(iAlt-as.integer(addnone==FALSE), maxcorrect, iTotalTrue)
  if (mincorrect > maxcorrect) stop(sprintf("ERROR: mincorrect (%d) > maxcorrect (%d)", mincorrect, maxcorrect))

  # draw indices
  iCorrect= vectorsample( (mincorrect:maxcorrect), 1 )
  index= vectorsample(which(vCorrect), iCorrect)
  index= c(index, vectorsample(which(!vCorrect), iAlt-iCorrect))
  index= sample(index) # randomize the answer order
  vAnswers=vAnswers[index]
  vCorrect=vCorrect[index]

  # order for easier readability
  if (alphorder) {
    nlen= length(vAnswers)
    index= sort.list(vAnswers)
    vAnswers=vAnswers[index]
    vCorrect=vCorrect[index]
  }

  # add none of the above
  if (addnone){
    vAnswers= c(vAnswers, "None of the above")
    vCorrect= c(vCorrect, (sum(as.integer(vCorrect)) == 0))
  }
  
  return (list(answer=vAnswers, correct=vCorrect))
}


# testcase
# answer_select(letters[1:5], c(T,T,F,F,F), iAlt=4, mincorrect=1, maxcorrect=3, addnone = FALSE)
