#' Format a value in a really nice way, including rounding and trailing zeroes
#'
#' @param x The value to format
#' @param dec.points The number of decimal points
#'
#' @return A string representing the formatted value. The value will be rounded to
#' the number of decimal points specified, and will contain trailing zeroes if
#' necessary so that the string has exactly the correct number of decimal points.
#' Commas will be placed in between thousands.
#' @export
#'
#' @examples
#' f(10000)
#' f(999999.1, 3)
f <- function(x, dec.points = 0){
  format(round(x,dec.points), big.mark = ",", nsmall = dec.points)
}
