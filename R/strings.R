#' Do strings contain special characters
#'
#' @param str A vector of strings to check
#' @param white Include whitespace as special characters?
#'
#' @return A logical vector representing if the input strings contain special characters or not
#' @export
#'
#' @examples
#' has_specials("HelloWorld")
#' has_specials(c("hello!", "asd", "11`"))
has_specials <- function(str, white = FALSE){
  pattern <- "/|:|\\?|!|~|;|\\.|,|\\+|&|\\||<|>|@|\\$|\\^|`|'|\\(|\\)|\\[|\\]|\\{|\\}|\\|\\\\|\\\\\\\\|\\*"
  ret <- vector(length = length(str))
  sapply(seq_along(str), function(x){
    if(white){
      if(grepl("[[:space:]]+", str[x])){
        ret[x] <- TRUE
      }else if(str[x] == "\\" | grepl(pattern, str[x])){
        ret[x] <- TRUE
      }else{
        ret[x] <- FALSE
      }
    }else{
      if(str[x] == "\\" | grepl(pattern, str[x])){
        ret[x] <- TRUE
      }else{
        ret[x] <- FALSE
      }
    }
  })
}

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

#' Capitalize the first letter in strings
#'
#' @param x A vector of strings
#'
#' @return the same vector back, with the first letter capitalized for each vector element
#' @export
#'
#' @examples
#' firstup("hello")
#' firstup(c("hello", "world"))
firstup <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
