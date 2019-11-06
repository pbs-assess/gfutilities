#' Format a value in a really nice way, including rounding and trailing zeroes
#'
#' @param x The value to format
#' @param dec.points The number of decimal points
#'
#' @return A string representing the formatted value.
#'
#' @details The value will be rounded to the number of decimal points specified,
#' and will contain trailing zeroes if necessary so that the string has exactly
#' the correct number of decimal points. Commas will be placed in between
#' thousands.
#'
#' @export
#'
#' @examples
#' f(10000)
#' f(999999.1, 3)
f <- function(x, dec.points = 0){
  format(round(x, dec.points),
         big.mark = ifelse(options("OutDec") == ",", " ", ","),
         nsmall = dec.points)
}

# round_nice <- function(x) {
#   out <- plyr::round_any(x, 100)
#   out[out == 0] <- x[out == 0]
#   out[x == 0] <- ""
#   out
# }
#
# mround <- function(x, base){
#   base * round(x / base)
# }

#' Round down to the nearest even number
#'
#' @param x Number to round
#' @param base Base for rounding. If 2, even numbers.
#'
#' @return Number rounded down
#' @export
#'
#' @examples
#' round_down_even(13.1)
#' round_down_even(14.22)
round_down_even <- function(x, base = 2){
  base * floor(x / base)
}

#' Round up to the nearest even number
#'
#' @param x Number to round
#' @param base Base for rounding. If 2, even numbers.
#'
#' @return Number rounded up
#' @export
#'
#' @examples
#' round_up_even(13.1)
#' round_up_even(14.22)
round_up_even <- function(x, base = 2){
  base * ceiling(x / base)
}
