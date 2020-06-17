#############################################################################
# lac= Qselect(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
# Purpose:
#   Select iAlt of the answers, ensuring that at least mincorrect of the correct answers
#   are included, and at most maxcorrect answers.
#   If requested, add a 'none of the above'
Qselect <- function(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=NULL, mincorrect=NULL){
  iA= length(vAnswers)
  
  if (is.null(maxcorrect))
    maxcorrect= iA
  if (is.null(mincorrect))
    mincorrect= 0
  
  if (sum(as.integer(vCorrect)) < mincorrect){
    print(sprintf("Number of answers requested: %d", iAlt))
    print(rbind(vAnswers, vCorrect))
    stop(print ("Warning: Not sufficient correct answers"))
  }
  if (iAlt < mincorrect){
    print(sprintf("Alternatives: %d; Min Correct: %d", iAlt, mincorrect))
    stop("Warning: insufficient alternatives drawn to achieve correct answers")
    return (NULL)
  }
  if (mincorrect < 0)
    mincorrect= 0
  if (maxcorrect < mincorrect)
    maxcorrect= mincorrect
  if (mincorrect > maxcorrect)
    mincorrect= maxcorrect
  
  # sample minimum number correct answer indices
  iC= 0
  vI= NULL
  if (mincorrect > 0) {
    vI= sample(which( as.logical(vCorrect) ), mincorrect)
    iC= mincorrect
  }
  # sample rest
  while (length(vI) < iAlt) {
    if (iC < maxcorrect) {
      # any index not already drawn
      vI= c(sample(which( !((1:iA) %*% vI) ), 1), vI)
      iC= iC + as.numeric(vCorrect[ vI[1] ])
    } else
      # any index not already drawn and corresponding to an incorrect answer
      vI= c(sample(which( (!((1:iA) %*% vI)) & (!as.logical(vCorrect)) ), 1), vI)
  }
  # scramble order
  vI = sample(vI, iAlt)
  vAnswers= vAnswers[vI]
  vCorrect= vCorrect[vI]
  if (addnone){
    vAnswers= c(vAnswers, "None of the above")
    vCorrect= c(vCorrect, iC == 0)
  }
  
  return (list(answer=vAnswers, correct=vCorrect))
}
