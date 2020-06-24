#############################################################################
# answer_select(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
# Purpose:
#   Select iAlt of the answers, ensuring that at least mincorrect of the correct answers
#   are included, and at most maxcorrect answers.
#   Add a 'none of the above', unless ruled out.
answer_select <- function(vAnswers, vCorrect, iAlt=NULL, addnone=TRUE, maxcorrect=NULL, mincorrect=NULL, alphorder=FALSE){
  
  iA= length(vAnswers)
  if (is.null(iAlt)) iAlt=iA
  vCorrect= as.logical(vCorrect)
  
  if (is.null(maxcorrect))
    maxcorrect= iAlt-(addnone==FALSE)
  if (is.null(mincorrect))
    mincorrect= 0+(addnone==FALSE)
  if (mincorrect < 0+(addnone==FALSE)) {
    print("WARNING: added minimim of 1 correct answers (incl. none of the above)")
    mincorrect= 1
  }
  if (maxcorrect > iAlt-(addnone==FALSE)) {
    print("WARNING: decreased maximum number of correct answers (incl. none of the above)")
    maxcorrect= iAlt-(addnone==FALSE)
  }
  if (maxcorrect < mincorrect) {
    print("WARNING: mincorrect > maxcorrect")
    maxcorrect= mincorrect
  }
  if (mincorrect > maxcorrect) {
    print("WARNING: mincorrect < maxcorrect")
    mincorrect= maxcorrect
  }
  if ((iAlt < mincorrect)) {
    print(cbind(vAnswers, vCorrect))
    stop(sprintf("ERROR: too few alternatives requested, iAlt=%d, mincorrect=%d", iAlt, mincorrect))
  }
  if (sum(as.integer(vCorrect)) < mincorrect){
    print(cbind(vAnswers, vCorrect))
    stop(sprintf("Warning: insufficient correct (%d) answers possible", mincorrect))
  }
  if (sum(1-as.integer(vCorrect)) < iAlt-maxcorrect){
    print(cbind(vAnswers, vCorrect))
    stop(sprintf("Warning: insufficient incorrect (%d) answers possible", iA-maxcorrect))
  }

  # first draw minimum correct answers  
  vCorrect= as.logical(vCorrect)
  vTrack= vCorrect
  index= NULL
  iC= 0
  if (mincorrect > 0) {
    index= which(vTrack)
    if (length(index) > 1) index= sample(index, 1)
    vTrack[index]= NA
    iC= mincorrect
  }
  # then draw until maxcorrect is reached or iAlt is reached
  while ((iC < maxcorrect) & (length(index) < iAlt)) {
    tmp= which(!is.na(vTrack))
    if (length(tmp) > 1) tmp= sample(tmp, 1)
    index= c(tmp, index)
    vTrack[index[1]]= NA
    iC= iC+as.integer(vCorrect[index[1]])
  }
  # if not iAlt reached, complete with incorrect answers
  if (length(index) < iAlt) 
    tmp= which(!vTrack)
    if (length(tmp) > 1) tmp= sample(tmp, iAlt-length(index))
    index= c(tmp, index)
  index=sample(index)
  vAnswers=vAnswers[index]
  vCorrect=vCorrect[index]
  
  # order for easier readability
  if (alphorder) {
    nlen= length(vAnswers)
    order= sort.list(vAnswers)
    vAnswers= vAnswers[order]
    vCorrect= vCorrect[order]
  }

  # add none of the above
  if (addnone){
    vAnswers= c(vAnswers, "None of the above")
    vCorrect= c(vCorrect, (iC == 0))
  }
  
  return (list(answer=vAnswers, correct=vCorrect))
}
