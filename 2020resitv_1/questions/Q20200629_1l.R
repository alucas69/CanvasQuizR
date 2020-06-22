Q20200629_1l <- function() {
  
  # generate random mu, sigma2, and quantile
  iDigits= 3
  qtype= "num"
  iN= sample(10:30, 1)
  dR= sample( c(-90:-15, 15:90)/100, 1)
  iRdigits= 2
  
  # formulate the question
  QuestionText <- c(
    tex2math(sprintf("Given is a sample of size %d of observations of the two variables $X$ 
      and $Y$, we computed a sample correlation of $r = %4.2f$. We test the 
      hypothesis $H_0: \\rho=0$ against a 2-sided alternative using our standard 
      test statistic for this purpose. Compute the value of this test statistic 
      in %d decimals.",
                     iN, dR, iDigits)),
    DigitDecimalWarning(iDigits)
  )
  
  # compute the answer and error margin
  Answers <- dR / sqrt((1 - dR*dR) / (iN-2))
  Answers <- matrix(Answers + 0.002 * c(0, -1, 1), ncol=3)
  
  return(list(type=qtype, text=QuestionText, answer=Answers))    
}