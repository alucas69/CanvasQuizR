#' Floor the degrees of freedom of the student-t distribution
#'
#' This function floors the DF, in correspondance with the table in Doane et al.
#' @param df: int, requested DF
#' @keywords degrees of freedom flooring
#' @export dfL: int, downward floored/cropped df
#' @examples
#' FloorDfT(57)
FloorDfT <- function(df){
  dfL= df
  if (df > 50){
    if (df <= 100){
      dfL= floor(df/5)*5
    } else if (df <= 150) {
      dfL= floor(df/10)*10
    } else {
      dfL= 25000
    }
  }
  return (dfL)
}
