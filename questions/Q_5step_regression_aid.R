# Imports
library("stringi")
library("car")

# source ("../../aidfunctions.R")
# source ("../../html_escape.R")
# source ("../../tex2math.R")
# source ("../../write_in_wrapper.R")
# source ("../../RoundAnswer.R")
# source ("../../answer_select.R")
# source ("../../FloorDfT.R")


################################################################
### Q_doubleminus(asS)
#
# Purpose:
#    Replace double minus by a plus
Q_doubleminus <- function(asS){
  for (i in 1:length(asS)){
    asS[i]= stri_replace_all(asS[i], regex="-\\s*-", " + ")   # -- -> +
    asS[i]= stri_replace_all(asS[i], regex="\\+\\s*-", " - ") # +- -> -
    asS[i]= stri_replace_all(asS[i], fixed="  ", " ")         # "  " -> " "
  }
  return (asS)
}

################################################################
### Q_detect(asS, asFind)
#
# Purpose:
#    Check with elements of asS occur in asFind
#
# Return value:
#    vF     iS vector, false if elements of asS is not found in asFind, true otherwise
Q_detect <- function(asS, asFind){
  iS= length(asS)
  vF= rep(FALSE, iS)
  for (s in 1:iS){
    vF[s]= asS[s] %in% asFind
  }
  return (vF)
}

################################################################
### Q_theormodel(vI)
#
# Purpose:
#    Prepare a theoretical model, using x's indexed by vI
Q_theormodel <- function(vI, hat= FALSE, par='\\beta'){
  # vI= c(1, 2, 5)
  asMod= '$'
  if (hat)
    asMod= c(asMod, '\\hat ')

  asMod= c(asMod, sprintf('y= %s_0', par))
  for (i in vI)
    asMod= c(asMod, sprintf(' + %s_%i x%i', par, i, i))
  asMod= c(asMod, ' + \\epsilon$')

  return (paste(asMod, collapse=''))
}

################################################################
### Q_empmodel(vI, vP)
#
# Purpose:
#    Prepare a empirical model, using x's indexed by vI, and parameters
Q_empmodel <- function(vI, vP, hat= TRUE, res= FALSE){
  # vI= c(1, 2, 5)
  asMod= '$'
  if (hat)
    asMod= c(asMod, '\\hat ')
  asMod= c(asMod, sprintf('y= %.3f', vP[1]))

  iK= length(vI)
  for (j in 1:iK)
    asMod= c(asMod, sprintf(' + %.3f x%i', vP[j+1], vI[j]))
  if (res)    # Incorrect... add residual
    asMod= c(asMod, ' + e')

  asMod= c(asMod, '$')
  return (paste(asMod, collapse=''))
}

################################################################
# Q_descvar(asVar, asVarSh, asUnits, var='x')
#
# Purpose:
#    Prepare a list of descriptions for all variables
#
# Inputs:
#   asVar   iYX list of variable names
#   units   (optional, default= NULL) if given, units of measurement
#   var     (optional, default= 'x'), type of variable
#   coef    (optional, default= '') string, extra text to describe coefficient
#
# Return value:
#   asDesc  iYX list of full descriptions, for y, x0
Q_descvar <- function(asVar, units= NULL, var='x', coef=''){

  if (is.null(units))
    asDesc= sprintf('$y$: %s', asVar[1])
  else
    asDesc= sprintf('$y$: %s (%s)', asVar[1], units[1])

  iK= length(asVar)
  for (i in 2:iK){
    if ((i == 2) | is.null(units))
      asDesc= c(asDesc, sprintf('$%s_%i$: %s %s', var, i-2, coef, asVar[i]))
    else
      asDesc= c(asDesc, sprintf('$%s_%i$: %s %s (%s)', var, i-2, coef, asVar[i], units[i]))
  }

  return (asDesc)
}

############################################################################
# lac= Qselect(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=1, mincorrect=0)
# Purpose:
#   Select iAlt of the answers, ensuring that at least mincorrect of the correct answers
#   are included, and at most maxcorrect answers.
#   If requested, add a 'none of the above'
Qselect <- function(vAnswers, vCorrect, iAlt, addnone=FALSE, maxcorrect=NULL, mincorrect=NULL){
  warning("Use answer_select")
  return (answer_select(vAnsers, vCorrect, iAlt, addnone, maxcorrect, mincorrect))}

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

myprettylmprint <- function(l.lm, collapse='\n', skipcall=TRUE, htmlescape=FALSE, convert_ampersand=FALSE){
  output=capture.output(eval(parse(text="summary(l.lm)")))

  if (skipcall){    # Get rid of call
    i= grep("Residuals:", output)
    output= output[-(1:i-1)]
  }

  output= stri_replace_all(output, "&apos;", regex="[\x91\x92']")
  output= stri_replace_all(output, "&quot;", regex="[\"]")
  output= paste(output, collapse=collapse)
  if (htmlescape) output= html_escape(output, convert_ampersand=convert_ampersand)

  return (output)
}
