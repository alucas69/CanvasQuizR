Q20200629_1b <- function() {
  
  # initialize
  qtype= "num"

  # generate random mu, sigma2, and quantile
  iDigits <- 2
  dAlpha <- sample(c(1:4, 6:9), 1) / 10
  dMu <- sample(c(-1,1), 1) * sample(4:24, 1) * 10
  dSigma <- sample(1:floor(abs(dMu) / 3), 1)
  
  # formulate the question
  QuestionText <- c(
    tex2math(sprintf(
      "Let $X$ be a normal random variable $N(\\mu = %d, \\sigma^2 = %d)$. Compute the %d-th percentile of the distribution of $X$ in %d decimal positions.",
      dMu, dSigma^2, 100 * dAlpha, iDigits)),
    DigitDecimalWarning(iDigits)
  )
  
  # compute the answer and error margin
  dZ <- qnorm(dAlpha)
  dZerror <- 0.021
  Answers <- matrix(dMu + dSigma * ( dZ + dZerror * c(0, -1, 1)), ncol=3)

  # return the question
  return(list(type=qtype, text=QuestionText, answer=Answers))    
}

