#' Get an RGB string representing the color and opacity given
#'
#' @param color A vector of R color strings or numbers
#' @param opacity An integer between 0 an 99 representing percentage opaque
#'
#' @return A vector of RGB strings of the specified colors and opacity
#'         Format of returned string is #RRGGBBAA
#'         where RR = red, a 2-hexadecimal-digit string
#'               GG = green, a 2-hexadecimal-digit string
#'               BB = blue, a 2-hexadecimal-digit string
#'               AA = alpha or opacity
#'
#' @export
#' @importFrom grDevices col2rgb
#'
#' @examples
#' library(gfutilities)
#' get_shade("blue", 1)
#' get_shade("salmon")
#' get_shade(c(1, 2, 3), 50)
#' get_shade(c("red", "blue", "yellow"), 20)
get_shade <- function(color, opacity = 0){
  if(!(opacity %% 1 == 0) || opacity < 0 || opacity > 99){
    warning("Opacity argument must be an integer between 0 and 99. Setting to 0.")
    opacity <- 0
  }
  opacity <- as.character(opacity)
  if(nchar(opacity) == 1){
    opacity <- paste0("0", opacity)
  }
  colorDEC <- grDevices::col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow = 3,
                       ncol = ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",
                           colorHEX[1,col],
                           colorHEX[2,col],
                           colorHEX[3,col],
                           opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i]) == 1){
        colorHEX[i] <- paste0("0",
                              colorHEX[i])
      }
    }
    shade <- paste0("#",
                    colorHEX[1],
                    colorHEX[2],
                    colorHEX[3],
                    opacity)
  }
  shade
}

#' Create a vector of n colors that are perceptually equidistant and in an
#' order that is easy to interpret
#'
#' @param n The number of colors to produce
#' @param alpha Transparency, from 0 (fully transparent) to 1 (opaque)
#'
#' @return A vector of n colors
#' @export
#' @importFrom stats dnorm
#' @importFrom grDevices rgb
#'
#' @examples
#' library(gfutilities)
#' rich_colors(12)
rich_colors <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1 / (1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- stats::dnorm(x, 0.25, 0.15) / max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  apply(rgb.m,
        1,
        function(v){
          grDevices::rgb(v[1],v[2],v[3],alpha = alpha)
        })
}
