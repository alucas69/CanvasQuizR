MathParenthesesTag <- "stretchy=\"false\""


MathConvert <- function(VectorOfStrings) {
  MathStart <- "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow>"
  MathStop <- "</mrow></math>"
  return(
    paste(MathStart, paste(VectorOfStrings, collapse = ""), MathStop, sep = "")
  )
}
 
MathSymbol <- function(Letter, numberdigits = 0, WrapInMathSymbol = TRUE) {
  if (is.numeric(Letter)) {
    return(
      paste("<mn>", sprintf( sprintf("%%1.%df", numberdigits), Letter), "</mn>", sep = "")
    )
  } else {
    return(
      paste("<mi mathvariant=\"italic\">", Letter, "</mi>", sep = "")
    )
  }
}

MathGreek <- function(LetterName) {
  return(
      paste("<mi mathvariant=\"italic\">&amp;", LetterName, ";</mi>", sep = "")
  )
}

MathOperator <- function(Symbol, AdditionalTag = "") {
  return(
    paste(sprintf("<mo %s>", AdditionalTag), Symbol, "</mo>", sep = "")
  )
}

MathSub <- function(MainSymbol, Index, MainSymbolDigits = 0, IndexDigits = 0,
                    WrapInMathSymbol = TRUE) {
  if (WrapInMathSymbol == TRUE) {
    MainSymbol <- MathSymbol(MainSymbol, numberdigits = MainSymbolDigits)
    Index <- MathSymbol(Index, numberdigits = IndexDigits)
  } 
  
  return(paste("<msub><mrow>", MainSymbol, "</mrow><mrow>", Index, "</mrow></msub>",
               sep = ""))
}




MathSup <- function(MainSymbol, Index, MainSymbolDigits = 0, IndexDigits = 0,
                    WrapInMathSymbol = TRUE) {
  
  if (WrapInMathSymbol == TRUE) {
    MainSymbol <- MathSymbol(MainSymbol, numberdigits = MainSymbolDigits)
    Index <- MathSymbol(Index, numberdigits = IndexDigits)
  } 
  
  return(paste("<msup><mrow>", MainSymbol, "</mrow><mrow>", Index, "</mrow></msup>",
               sep = ""))
}




MathSubSup <- function(MainSymbol, IndexLow, IndexHigh, 
                       MainSymbolDigits = 0, IndexLowDigits = 0, IndexHighDigits = 0,
                       WrapInMathSymbol = TRUE) {
  
  if (WrapInMathSymbol == TRUE) {
    MainSymbol <- MathSymbol(MainSymbol, numberdigits = MainSymbolDigits)
    IndexLow <- MathSymbol(IndexLow, numberdigits = IndexLowDigits)
    IndexHigh <- MathSymbol(IndexHigh, numberdigits = IndexHighDigits)
  } 
  
  return(paste("<msubsup><mrow>", MainSymbol, "</mrow><mrow>", IndexLow, "</mrow><mrow>",
               IndexHigh, "</mrow></msubsup>", sep = ""))
}



MathSupSub <- function(MainSymbol, IndexHigh, IndexLow, 
                       MainSymbolDigits = 0, IndexLowDigits = 0, IndexHighDigits = 0,
                       WrapInMathSymbol = TRUE) {
  return(MathSubSup(MainSymbol, IndexLow, IndexHigh, MainSymbolDigits,
                    IndexLowDigits, IndexHighDigits, WrapInMathSymbol))
}



MathFrac <- function(Numerator, Denominator) {
  return(paste("<mfrac><mrow>", Numerator, "</mrow><mrow>", Denominator, "</mrow></mfrac>"))
}


MathSqrt <- function(Argument) {
  return(paste("<msqrt><mrow>", Argument, "</mrow></msqrt>"))
}


Mathxbar <- "<mover accent=\"true\"><mi>X</mi><mo>&amp;#x203E;</mo></mover>"
Mathxbar <- function(Letter = "X", Index = NULL) {
  xbar <- sprintf("<mover accent=\"true\"><mi>%s</mi><mo>&amp;#x203E;</mo></mover>", Letter)
  if (!is.null(Index)) {
    xbar <- MathSub(xbar, Index)
  }
  return(xbar)
}
MathSsquared <- function(Letter = "s", Index = NULL) {
  if (is.null(Index)) s2 <- MathSup(Letter, 2)
  else s2 <- MathSubSup(Letter, Index, 2)
  return(s2)
}


MathUnequal <- "<mo>&amp;#x2260;</mo>"
MathNeq <- MathUnequal
MathTimes <- "<mo>&amp;times;</mo>"
MathVariance <- MathSup("&amp;sigma;", 2)
MathChisq <- MathSup("&amp;chi;", 2)
MathH0 <- MathSub("H",0)
MathH1 <- MathSub("H",1)

