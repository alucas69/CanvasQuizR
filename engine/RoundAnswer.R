#' Round the numerical answer up to a number of digits
#'
#' This function does the rounding, and creates a range which is considered correct
#' @param dA: double, exact numerical answer
#' @param iDigits: integer, number of decimals requested
#' @keywords numerical answer rounding
#' @export row vector, with the exact answer, and lower and upper bounds
#' @examples
#' RoundAnswer(10.75409, 3)
RoundAnswer <- function(dA, iDigits){
  dAR= round(dA, iDigits)

  # Rather precise: Exact iDigit answer, plus/minus minimum distant. Strict.
  # dL= dAR - 0.5*10^(-iDigits)
  # dH= dAR + 0.5*10^(-iDigits)
  # return (c(dAR, dL, dH))

  # More generous, not symmetric around the exact answer.
  dL= dAR - 10^(-iDigits)
  dH= dAR + 10^(-iDigits)
  return (matrix(c(dA, dL, dH), ncol=3))
}
