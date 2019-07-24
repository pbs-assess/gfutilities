#' Do strings contain special characters
#'
#' @param str A vector of strings to check
#' @param white Include whitespace as special characters?
#'
#' @return A logical vector representing if the input strings contain special characters or not
#' @export
#'
#' @examples
#' library(gfutilities)
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
#' library(gfutilities)
#' list_string("One")
#' list_string(c("One", "two"))
#' list_string(c("One", "two", "three", "four"))
list_string <- function(vec){
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
#' library(gfutilities)
#' firstup("hello")
#' firstup(c("hello", "world"))
firstup <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Make filenames with the supplied extensions. If the extensions are already in the filenames, return the filenames
#'
#' @param filename name for the files with or without the extension
#' @param ext extensions to use. May or may not include a dot (.)
#'
#' @return The filename(s) with the supplied extension(s)
#' @export
#' @importFrom stringr str_sub
#'
#' @examples
#' library(gfutilities)
#' file_addext("hello_world_data", ".rds")
file_addext <- function(filename, ext){
  if(missing(filename)){
    stop("You must provide at least one filename")
  }
  if(class(filename) != "character"){
    stop("You must provide character type filenames")
  }
  if(missing(ext)){
    stop("You must provide at least one extension")
  }
  if(class(ext) != "character"){
    stop("You must provide character type extensions")
  }
  if(length(filename) != length(ext)){
    stop("The number of filenames is not equal to the number of extensions")
  }

  exts_corr <- sapply(ext, function(x){
    ifelse(length(grep("\\.", x)), x, paste0(".", x))
  })

  filenames_corr <- sapply(seq_along(filename), function(x){
    nc <- nchar(filename[x])
    ifelse(tolower(stringr::str_sub(filename[x],
                                    start = nc - (nchar(exts_corr[x]) - 1),
                                    end = nc)) == exts_corr[x],
           filename[x],
           paste0(filename[x], exts_corr[x]))
  })
  names(filenames_corr) <- NULL
  filenames_corr
}
