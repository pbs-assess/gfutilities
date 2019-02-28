#' Create a string out of the strings in vec, by glueing together with commas
#' and placing 'and' before the last one
#'
#' @param vec Character vector
#'
#' @return A string with the items of vec glued together using commas, with
#' 'and' place before the last one
#' @export
#'
#' @examples
#' list.string("One")
#' list.string(c("One", "two"))
#' list.string(c("One", "two", "three", "four"))
list.string <- function(vec){
  if(length(vec) == 1){
    return(vec)
  }
  if(length(vec) == 2){
    return(paste0(vec[1], " and ", vec[2]))
  }

  j <- paste(vec[-length(vec)], collapse = ", ")
  paste(j, "and", vec[length(vec)])
}
