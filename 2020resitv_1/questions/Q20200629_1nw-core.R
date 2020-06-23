Q20200629_1nw_core <- function() {
  
  # make the dgp
  dMean1 <- sample(30:100, 1)
  dSignMeanDifference <- sample(c(-1,1), 1)
  dMean2 <- dMean1 + dSignMeanDifference * sample(3:10, 1)
  iN <- sample(8:15, 1)
  dS1 <- abs(dMean1 - dMean2) / 2.2 * sqrt(iN)
  dS2 <- dS1
  Sample1 <- rnorm(iN, mean = dMean1, sd = dS1)
  Sample2 <- rnorm(iN, mean = dMean2, sd = dS2)
  Sample <- data.frame(Sample1, Sample2)

  # return
  return( Sample )
}

Q20200605_1nw_core <- function() return(Q20200629_1nw_core())
  