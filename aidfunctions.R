myfloor <- function(x, digits = 0) {
  iSide <- 1
  if (x < 0) {
    iSide <- -1
    x <- -x
  }
  return( iSide * floor( x * 10^digits) * 10^(-digits) )
}



mycapitalize <- function(s, allwords = FALSE) {
  if (allwords == TRUE) {
    s <- strsplit(s, " ")[[1]]
    return(paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "", collapse = " "))
  } else {
    return(paste(toupper(substring(s,1,1)), substring(s,2), sep="", collapse = " "))
  }
}


myprettytableprint <- function(s, digits = 3, title = NULL, floatcolumn = NULL) {
  # print title
  if (!is.null(title)) outtable <- sprintf("%s\n", title) else outtable <- ""
  if (is.null(floatcolumn)) floatcolumn <- rep(1, ncol(s))
  else floatcolumn <- 1 - (floatcolumn == 0)
  # measure widths
  colwidths <- rep(0, ncol(s)+1)
  colwidths[1] <- max(stri_length(rownames(s)))
  for (i2 in 1:ncol(s)) {
    colwidths[i2+1] <- max(stri_length(colnames(s)[i2]), 
                           stri_length(sprintf(sprintf("%%1.%df", floatcolumn[i2] * digits), s[,i2])))
  }
  spacing <- "  "
  # pretty print
  for (i1 in 0:nrow(s)) {
    for (i2 in 0:ncol(s)) {
      if ((i1 == 0) && (i2 == 0)) {
        outline <- sprintf(sprintf("%%%ds%%s", colwidths[1]), " ", spacing)
      } else if (i2 == 0) {
        outline <- sprintf(sprintf("%%%ds%%s", colwidths[1]), rownames(s)[i1], spacing)
      } else if (i1 == 0) {
        outline <- sprintf(sprintf("%%s%%%ds%%s", colwidths[1+i2]), outline, colnames(s)[i2], spacing)
      } else {
        outline <- sprintf(sprintf("%%s%%%d.%df%%s", colwidths[1+i2], floatcolumn[i2] * digits), outline, s[i1,i2], spacing)
      }
    }
    outtable <- sprintf("%s%s\n",outtable, outline)
  }
  return(EscapeHtml(outtable, ampersand = FALSE))
}



myprettyleveneprint <- function(tb, digits = 3) {
  len1 <- max(8, stri_length(sprintf(sprintf("%%1.%df", digits), tb$`F value`[1])))
  len2 <- max(6, digits+2+1)
  outtable <- sprintf(sprintf("%%s\n%%%ds %%%ds\n%%%d.%df %%%d.%df", len1, len2, 
                              len1, digits, len2, digits),
                      "Levene's Test for Homogeneity of Variance",
                      "F value", "P(>F)", tb$`F value`[1], tb$`Pr(>F)`[1])
  return(outtable)
}



myprettyttestprint <- function(tb, digits = 3, withalternative = TRUE, withnullvalue = TRUE) {
  # print title
  outtable <- sprintf(sprintf("Two sample t-test\nt = %%1.%df, df = %d", digits, 
                              tb$parameter), tb$statistic)
  if (withnullvalue == TRUE) {
    outtable <- sprintf("%s\n(Null hypothesis for this test: true difference in means is %1.0f)",
                        outtable, tb$null.value)
  }
  if (withalternative == TRUE) {
    alttext <- "not equal to"
    if (tb$alternative == "less") alttext <- "less than"
    else if (tb$alternative == "greater") alttext <- "greater than"
    outtable <- sprintf(sprintf("%%s, p-value = %%1.%df\nalternative hypothesis:
                                true difference in means is %s %%1.0f", digits, alttext),
                        outtable, tb$p.value, tb$null.value)
  }
  return(outtable)
}


#' Write a pretty-looking p-value
#'
#' This function writes a pretty looking p-value, based on F statistics
#' @param f is a structure with elements for the f-statistic
#' @keywords regression summary f-statistic
#' @export sPv holds the output string, p-value
#' @examples
#' myprettyr2fprint(summary(lm0.r)$fstatistic)
myprettypvf <- function(f){
  dPv= pf(f[1], f[2], f[3], lower.tail=FALSE)
  sPv= sprintf("%.4g", dPv)
  if (dPv < 2.22e-16)
    sPv= "&lt; 2.22e-16"
  return (sPv)
}

#' Write a pretty-looking regression summary
#'
#' This function writes a pretty looking regression summary to a string
#' @param s is a list structure with elements from the summary
#' @keywords regression summary
#' @export sOut holds the output string, 3-lines
#' @examples
#' myprettyr2fprint(summary(lm0.r))
myprettyr2fprint <- function(s){
  sPv= myprettypvf(s$fstatistic)
  sOut= sprintf('Residual standard error: %.1f on %i degrees of freedom\nMultiple R-squared: %.4g,	Adjusted R-squared: %.4g\nF-statistic: %.3f on %i and %i DF,  p-value: %s',
                s$sigma, s$df[2], s$r.squared, s$adj.r.squared, s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], sPv)
  
  return (sOut)
}




DigitDecimalWarning <- function(iDigits = NULL) {
  if (is.null(iDigits)) {
    return(sprintf("(Use a decimal point, *not* a comma and only round your *final* answer and not any intermediate results.)"))    
  } else {
    return(sprintf("(Use %d decimals behind the decimal point in your answer. Also, use a decimal point, *not* a comma and only round your *final* answer and not any intermediate results.)", iDigits))    
  }
}


BlueCourier <- function(sString) {
  return(sprintf("<b style=\"font-family:'Courier New'; color:blue\">%s</b>", sString))
  
}



HtmlUlList <- function(vStrings) {
  Out <- "<ul>"
  for (i1 in 1:length(vStrings)) {
    Out <- paste(Out, sprintf("<li>%s</li>", vStrings[i1]), sep = "")
  }
  Out <- paste(Out, "</ul>", sep = "")
  return(Out)
}
