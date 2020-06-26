Q20200629_1c <- function() {
  
  # generate random mu, sigma2, and quantile
  iDigits <- 4
  qtype= "num"
  dAlpha <- sample(c(1:4, 6:9), 1) / 10
  dMu <- sample(c(-1,1), 1) * sample(4:24, 1) * 10
  dSigma <- sample(1:floor(abs(dMu) / 3), 1)
  dQuantile <- sample(c(-1,1),1) * runif(1, min = 0.5, max = 2)
  dQuantile <- round( dMu + dQuantile * dSigma / sqrt(2), digits = 0)
  iDirection <- sample(c(-1,1), 1)
  
  # formulate the question
  QuestionText <- c(
    tex2math(sprintf(
      "Let $X_1$ and $X_2$ be two independent normal random variables, both $N(\\mu = %d, \\sigma^2 = %d)$
        distributed. Let $\\overline X = 0.5(X_1 + X_2)$. Compute $P(\\overline X %s %d)$
        in %d decimal positions.",
      dMu, dSigma^2, c("\\le", "\\ge")[(iDirection == 1) + 1], dQuantile, iDigits
    )),
    DigitDecimalWarning(iDigits)
  )
  
  # compute the answer and error margin
  dZ <- (dQuantile - dMu) / (dSigma / sqrt(2))
  dZerror <- 0.018
  Answers <- matrix(pnorm( dZ + dZerror * c(0, -1, 1), lower.tail = (iDirection == -1) ),
                    ncol=3)
  for (i1 in 1:nrow(Answers)) Answers[i1, 2:3] <- sort(Answers[i1, 2:3])
  
  return(list(type=qtype, text=QuestionText, answer=Answers))    
}

