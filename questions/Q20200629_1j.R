Q20200629_1j <- function() {
  
  # generate random mu, sigma2, and quantile
  iDigits= 3
  qtype= "num"
  iN= sample(5:35, 1)
  dMu= sample(100:260, 1)
  dSigma= sample(6:50, 1) / 2
  dQuantile= sample( c(1, 5, 10, 20), 1)
  
  # formulate the question
  QuestionText <- c(
    tex2math(sprintf("In a sample of size %d from a normally $N(\\mu, \\sigma^2)$ distributed 
      population, we find a sample mean of %d.0 and a sample standard deviation of 
      %3.1f. Give the lower bound of the %d%% confidence interval for $\\sigma^2$ (so *not* $\\mu$) in %d decimals.",
                     iN, dMu, dSigma, 100 - dQuantile, iDigits)),
    DigitDecimalWarning(iDigits)
  )
  
  # compute the answer and error margin
  dZ <- round(qchisq(1 - dQuantile/200, iN - 1), digits = 2) + 0.005 * c(0,-1,1)
  dZerror <- 0.015
  Answers <- matrix((iN - 1) * dSigma * dSigma / (dZ + dZerror * c(0,1,-1)), ncol=3)
  
  return(list(type=qtype, text=QuestionText, answer=Answers))    
}