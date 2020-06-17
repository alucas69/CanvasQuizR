########################################
## tex2math
##
## Purpose:
##   Move a tex string to math mode
##
## Author:
##   Charles Bos
##
## Version:
##   a    first start
##   b    getting closer... But have to debug still... Does tex2mathml work? Some error in the recursion...
##   c    with setup for nicer numerics, but turned off...
##
## Date:
##   2020/5/4
########################################

library(stringi)

#######################################################
# is.numeral(sIn)
#
# Purpose:
#   Checks if sIn could be numeric
#
# Inputs:
#   sIn     string
#
# Return value:
#   bN      boolean, TRUE if this is a numeral, FALSE else
is.numeral <- function(sIn){
  return (grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", sIn))
}

#######################################################
# is.operator(sIn)
#
# Purpose:
#   Checks if sIn could be an operator
#
# Inputs:
#   sIn     string
#
# Return value:
#   bN      boolean, TRUE if this is an operator, FALSE else
is.operator <- function(sIn){
  return (grepl("[+-/*()]", sIn))
}

########################################
# (l$start, l$iS, l$end, l$E)= tex_gettext(sIn)
#
# Purpose:
#   Extract text from the input string, until true tex begins
#   It manages to skip k$ and df$ indicators, which might mean something else...
#
# Inputs:
#   sIn     string, input
#
# Return value:
#   l$start  string, textual part from start
#   l$iS     integer, length of textual part
#   l$end    string, remainder
#   l$iE     integer, length of remainder
tex_gettext <- function(sIn){
  # sIn= 'John tested the null hypothesis $H_0: \\beta_1 \\le 0.3$ in the restricted model, and finds $t= (b_1 - 0.3)/s_{b_1}$=0.107091.'
  # sIn= 'John test that $z_{crit}= 5.15.'
  # sIn= '(Use 4 decimals after the decimal point in your answer. Also, use a decimal point, *not* a comma and only round your *final* answer and not any intermediate results.)'
  # sIn= 'No strings here Only a df$area...'
  # print (sIn)
  iL= nchar(sIn)
  amI= stri_locate_all(sIn, fixed=c("$", "k$", "df$"))
  vI= amI[[1]][,1]
  vKD= c(amI[[2]][,1]+1, amI[[3]][,1]+2)
  vKD= vKD[!is.na(vKD)]
  for (i in vKD)
    vI= vI[vI != i]

  if (length(vI) == 0)
    vI= NaN

  if (all(is.na(vI))){
    sStart= sIn
    sEnd= ""
  } else {
    if ((length(vI) %% 2) == 1){
      print ("Warning: Could it be that a closing '$' is missing?")
      print (sprintf("Input: '%s'", sIn))
    }

    i= min(vI)
    if (i == 1){
      sStart= ""
      sEnd= sIn
    } else {
      sStart= stri_sub(sIn, 1, i-1)
      sEnd= stri_sub(sIn, i, iL)
    }
  }
  # print (sprintf("Extracting text: '%s'+'%s'='%s'", sStart, sEnd, sIn))

  return (list(start=sStart, iS= nchar(sStart), end=sEnd, iE=nchar(sEnd)))
}

########################################
# l$start, l$iS, l$end, l$E)= tex_gettex(sIn)
#
# Purpose:
#   Extract tex from the START of input string, until tex ends
#
# Inputs:
#   sIn     string, input
#
# Return value:
#   l$start  string, tex part from start, between $'s
#   l$iS     integer, length of tex part
#   l$end    string, remainder
#   l$iE     integer, length of remainder
tex_gettex <- function(sIn){
  # sIn= "Hello there k$ with k$ a $\\beta$ included"
  # sIn= "Hello $\\beta$ there"
  # sIn= "Hello k there"
  # sIn= "$\\beta$ there $more$"

  iL= nchar(sIn)
  vI= stri_locate_all(sIn, fixed="$")[[1]][,1]
  sStart= ""
  sEnd= sIn
  if ((length(vI) >= 2) & (vI[1] == 1)){
    sStart= stri_sub(sIn, vI[1]+1, vI[2]-1)
    sEnd= ""
    if (vI[2] < iL)
      sEnd= stri_sub(sIn, vI[2]+1, iL)
  }

  # print (sprintf("Extracting tex: '%s'+'%s'='%s'", sStart, sEnd, sIn))

  return (list(start=sStart, iS= nchar(sStart), end=sEnd, iE=nchar(sEnd)))
}

########################################
# sMath= tex2split(sIn)
#
# Purpose:
#   Transform pure tex string to separate bits
#
# Inputs:
#   sIn      string, with pure latex, no $'s etc around, or
#            list, with elements already split-up
#
# Return value:
#   asIn     list, with elements split-up
#
# Note:
#   Under development
########################################
tex2split <- function(sIn){
  asIn= sIn
  if (length(asIn) == 1){
    as1= stri_split_boundaries(sIn, type="word")[[1]]
    asIn= c()    # Also split on _
    for (s in as1){
      as0= stri_split(s, fixed="_")[[1]]
      asIn= c(asIn, as0[1])

      if (length(as0) > 1){
        for (s0 in as0[2:length(as0)])
          asIn= c(asIn, "_", s0)
      }
    }

    # Get rid of empty elements or spaces
    vI= !((asIn == "") | stri_startswith(asIn, fixed=" "))
    asIn= asIn[vI]
    # print (sprintf("Splitting up %s into ", sIn))
    # print (asIn)
  }

  return (asIn)
}

########################################
# (l$sA, l$i2)= getblock(asIn, i1=1, i2=NULL)
#
# Purpose:
#   Extract a single math block from the list
#
# To consider:
#   At this stage, enclose a block by <mrow>...</mrow>, instead of doing so in tex2mathml?
#
# Inputs:
#   asIn      list, with elements already split-up
#   i1        integer, starting element to use
#   iL        integer, last element to consider (default=length(asIn))
#
# Return value:
#   l$sA      string, MathML representation of the first block
#   l$i2      integer, last element of asIn used
########################################
getblock <- function(asIn, i1= 1, iL= NULL){
  if (is.null(iL))
    iL= length(asIn)
  if (asIn[i1] == "{"){
    iLev= 1
    i2= i1
    while ((iLev > 0) & (i2 < iL)){
      i2= i2 + 1
      if (asIn[i2] == "{")
        iLev= iLev + 1
      if (asIn[i2] == "}")
        iLev= iLev - 1
      # print (sprintf("i1=%i, i2=%i, lev=%i, b=%s", i1, i2, iLev, asIn[i2]))
    }

    # print (sprintf("Putting elements %i-%i as bracketed..., with elements %s, %s, in-between=", i1, i2, asIn[i1], asIn[i2]))
    # print (asIn[i1:i2])

    if (asIn[i2] != "}"){
      print ("Warning: No closing brace found")
      i2= iL+1
    }
    sA= paste(tex2mathml(asIn[(i1+1):(i2-1)]), collapse='')
  } else if (asIn[i1] == "\\"){
      # Not fully correct: Here a block is considered either a letter of a greek letter, nothing weird...
      i2= i1+1
      sA= paste(tex2mathml(asIn[i1:i2]), collapse='')
  } else {
    i2= i1
    sA= paste(tex2mathml(asIn[i1:i2]), collapse='')
  }

  return (list(sA=sA, i2=i2))
}

########################################
# sMath= tex2mathml(sIn)
#
# Purpose:
#   Transform pure tex string to MathML
#
# Inputs:
#   sIn      string, with pure latex, no $'s etc around, or
#             list, with elements already split-up
#
# Return value:
#   asMathML  list of strings, output text with pure MathML (but without the initialisation)
#
# Note:
#   Under development. Seems to work?
########################################
tex2mathml <- function(sIn){
  asIn= tex2split(sIn)

  iL= length(asIn)
  asOut= list()
  sA= ""
  i= 1
  while (i <= iL){
    # print (sprintf("Working on element i=%i, L=%i, '%s'", i, iL, asIn[i]))

    vIss= asIn[i] == c("^", "_")    # Check if we deal with sup/sub
    if (is.numeral(asIn[i])){
      sA= sprintf("<mn>%s</mn>", asIn[i])
    } else if (is.operator(asIn[i])){
        sA= sprintf("<mo>%s</mo>", asIn[i])
        # Something wrong in next code, I'll turn it off for now...
        #   Troublesome call
        #     sIn= "${M,Z}$"
        #     tex2math(sIn)
        #   which resulted in a doubling of the 'M'?!?
      # if (asIn[i] == "-") {
      #   # print (sprintf("Found a minus at location %i/%i, iL=%i", i, length(asIn), iL))
      #   if (i == length(asIn)) { # last element, make operator
      #     # print ("Choose operator")
      #     sA= sprintf("<mo>%s</mo>", asIn[i])
      #   } else if (is.numeral(asIn[i+1])) { # a numeral follows, so maybe a - sign
      #     print (sprintf("Next s='%s' is numeral... operator", asIn[i+1]))
      #     if (i == 1) {
      #       # first element, so make minus sign and increase counter
      #       # print ("First element on line... simple")
      #       sA= sprintf("<mn>-%s</mn>", asIn[i+1])
      #       i= i+1
      #     } else if (grepl("[+-/*(]", asIn[i-1])) {
      #       # print ("Not first element on line... operator before")
      #       # operator (not ')') preceeds, so is minus sign
      #       sA= sprintf("<mn>-%s</mn>", asIn[i+1])
      #       i= i+1
      #     } else {
      #       # print ("Nothing, simple operator")
      #       sA= sprintf("<mo>%s</mo>", asIn[i])
      #     }
      #   } else {
      #     # print ("Not a numeral, just print the -")
      #     sA= sprintf("<mo>%s</mo>", asIn[i])
      #   }
      # }
    } else if (asIn[i] == "{") {
      lA= getblock(asIn, i)
      sA= sprintf("<mrow>%s</mrow>", lA$sA)
      # print (sprintf("Printing block '<mrow>%s</mrow>'", lA$sA))
      i= lA$i2
    } else if (asIn[i] == "}"){
      print ("Warning: Loose closing parentheses found")
      sA= ""
    } else if (stri_startswith(asIn[i], fixed=" ")){
      # Do nothing with a space
      print ("Warning: space found")
      sA= ""
    } else if (asIn[i]==""){
      # Do nothing with nothing
      print ("Warning: empty element found")
      sA= ""
    } else if (asIn[i]=="<"){
      sA= "<mo>&amp;lt;</mo>"
    } else if (asIn[i]==">"){
      sA= "<mo>&amp;gt;</mo>"
    } else if ((asIn[i] == "\\") & ((asIn[i+1] == "ge") | (asIn[i+1] == "geq"))) {
      sA= "<mo>&amp;ge;</mo>"
      i= i+1
    } else if ((asIn[i] == "\\") & ((asIn[i+1] == "le") | (asIn[i+1] == "leq"))) {
      sA= "<mo>&amp;le;</mo>"
      i= i+1
    } else if ((asIn[i] == "\\") & (asIn[i+1] == "infty")) {
      sA= "<mo>&amp;infin;</mo>"
      i= i+1
    } else if ((asIn[i] == "\\") & (asIn[i+1] == "not") & (asIn[i+2] == "=")) {
      sA= "<mo>&amp;ne;</mo>"
      i= i+2
    } else if ((asIn[i] == "\\") & ((asIn[i+1] == "hat") | (asIn[i+1] == "overline") | (asIn[i+1] == "sqrt"))) {
      lA= getblock(asIn, i+2)

      iOHS= which(asIn[i+1] == c("hat", "overline", "sqrt"))
      if (iOHS <= 2)
        sA= sprintf("<mover accent=\"true\"><mrow>%s</mrow><mo stretchy=\"true\">%s</mo></mover>", lA$sA, c("^", "&amp;oline;")[iOHS])
      else
        sA= sprintf("<msqrt><mrow>%s</mrow></msqrt>", lA$sA)
      i= lA$i2
    } else if ((asIn[i] == "\\") & (asIn[i+1] == "frac")) {
      lA= getblock(asIn, i+2)
      lB= getblock(asIn, (lA$i2)+1)
      sA= sprintf("<mfrac><mrow>%s</mrow><mrow>%s</mrow></mfrac>", lA$sA, lB$sA)
      i= lB$i2
    } else if (asIn[i] == "\\") {
      sA= sprintf("<mi>&amp;%s;</mi>", asIn[i+1])
      i= i+1
    } else if (any(vIss)) {
      iS= length(asOut)
      sA= asOut[[iS]]
      lB= getblock(asIn, i+1)

      sSS= c("sup", "sub")[vIss]
      asOut[iS]= sprintf("<m%s><mrow>%s</mrow><mrow>%s</mrow></m%s>", sSS, sA, lB$sA, sSS)
      i= lB$i2

      # Check for s_b^2
      if (i < iL-1){
        vIss= asIn[i+1] == c("^", "_")    # Check if we deal with sup/sub
        if (any(vIss)){
          sB= lB$sA
          lC= getblock(asIn, i+2)
          sC= lC$sA
          sSSc= c("sup", "sub")[vIss]
          if ((sSS == "sup") & (sSSc == "sub")){
            asOut[iS]= sprintf("<msubsup><mrow>%s</mrow><mrow>%s</mrow><mrow>%s</mrow></msubsup>", sA, sC, sB)
            i= lC$i2
          }
          else if ((sSS == "sub") & (sSSc == "sup")){
            asOut[iS]= sprintf("<msubsup><mrow>%s</mrow><mrow>%s</mrow><mrow>%s</mrow></msubsup>", sA, sB, sC)
            i= lC$i2
          }
          else {
            print ("Warning: Incorrect double sup/subscript?")
          }
        }
      }
      sA= ""
    } else {
      sA= sprintf("<mi>%s</mi>", asIn[i])
    }
    if (length(sA) >1){
      print ("What is happening here?")
      print (sA)
    }
    if (!(sA == "")){
      asOut= c(asOut, sA)
      # print (sprintf("i=%i, in=%s, A=%s", i, asIn[i], sA))
    }
    i= i + 1
  }

  # sOut= paste(asOut, collapse="")
  # print (sprintf("sOut=%s", sOut))
  return (asOut)
}

########################################
# sMath= tex2math(sTex, init, math)
#
# Purpose:
#   Transform tex string to math
#
# Inputs:
#   sin     string, with text/latex, or vector, with iS strings of text/latex
#   init    (optional, default=FALSE) boolean, indicating if MathML should be initialised
#   math    (optional, default=FALSE) boolean, indicating if the text is LaTeX math already (hence even without $'s)
#
# Return value:
#   sMath   string, output text with text and MathML
#
# Note:
#   Under development. Capabilities seem to include
#     * Handle mixture of text and LaTex, with LaTeX Math indicated by $\\beta$
#     * Math can handle \overline, \hat, \sqrt, subscripts, superscripts, and blocks {}
#     * \frac
#     * Any other command is considered a greek letter, e.g. \\beta...
#
#   Notably missing is stuff like
#     * don't know?... More advanced math, of course, like integrals, sums etc.
########################################
tex2math <- function(sIn, init=FALSE, math=FALSE){
  iS= length(sIn)
  if (iS > 1)
    { # If this is a vector, transform each vector
      asOut= list()
      for (sInI in sIn){
        asOut= c(asOut, tex2math(sInI, init, math))
      }
      return (asOut)
    }

  # print (sprintf("Entering tex2math, init=%i, math=%i, in=%s", init, math, sIn))
  sMathStart= "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><mrow>"
  sMathStop= "</mrow></math>"

  sOut= ""
  if (!math){
    l= tex_gettext(sIn)   # Split text + tex
    if (l$iS)
      sOut= paste(sOut, l$start, sep="")    # Append text
    if (l$iE)             # If tex, append it
      sOut= paste(sOut, tex2math(l$end, init=TRUE, math=TRUE), sep="")
  }
  if (math & init){
    l= tex_gettex(sIn)   # Split tex + text
    if (l$iS){
      sOut= paste(sOut, sMathStart, sep="")    # Start MathML
      sOut= paste(sOut, tex2math(l$start, init=FALSE, math=TRUE), sep="")    # Append tex
      sOut= paste(sOut, sMathStop, sep="")      # End MathML
    }
    if (l$iE)             # If text, append it
      sOut= paste(sOut, tex2math(l$end, init=FALSE, math=FALSE), sep="")
  }
  if (math & !init){
    # print (sprintf("Sending %s to mathml...", sIn))
    asMathML= tex2mathml(sIn)
    sMathML= paste(asMathML, collapse='')
    sOut= paste(sOut, sMathML, sep="")
  }

  return (sOut)
}

main <- function(){
  sIn= "Hello $H_0: \\chi= \\beta + {x^4}_{3a+6}$"

  sIn= "Hello $H_0$ and $\\beta$"
  sIn= "$\\hat y= H_0: \\overline {x+4} - \\sqrt{a+b}$"
  sIn= "$\\frac {a+b}{c+d}$"
  sIn= "Price (in k$)"
  sIn= 'Test statistic= $p_{M,Z} - p_{M,NB}$, reject for large values'
  # sIn= '$p - q$, reject for large values'
  sIn= '${M,Z}$'
  sIn= "$(\\infty, 5]$"
  sIn= "$s_{b_1}^2$ Test statistic= $p_{M,Z} - p_{M,NB}$"
  sIn= "$H_0: \\pi_{F,T} \\ge \\pi_{M,W}$"
  sIn= 'We test $H_0: \\pi_{A,EUR} \\ge \\pi_{A,VU}$, using test statistic= $p_{A,EUR} - p_{A,VU}$, and reject for small values'
  print (tex2math(sIn))

  # print (tex2mathml(sIn))
}

############################################################################
# main()
