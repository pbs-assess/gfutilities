#' Convert a number into an English word
#'
#' @param x The number to change into English
#' @param th Make the word end in 'st', 'nd', 'rd', or 'th'
#' @param cap_first Capitalize the first letter of the resulting word
#'
#' @return English text representing the number given
#' @export
#'
#' @examples
#' number_to_word(1)
#' number_to_word(999999, th = TRUE)
#' number_to_word(101, cap_first = TRUE)
number_to_word <- function(x,
                           th = FALSE,
                           cap_first = FALSE){
  if(length(x) > 1){
    stop("Only one value can be converted",
         call. = FALSE)
  }
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if(nDigits == 1){
      as.vector(ones[digits])
    }else if(nDigits == 2){
      if(x <= 19){
        as.vector(teens[digits[1]])
      }else{
        trim(paste(tens[digits[2]],
                   Recall(as.numeric(digits[1]))))
      }
    }else if(nDigits == 3){
      trim(paste(ones[digits[3]],
                 "hundred",
                 Recall(makeNumber(digits[2:1]))))
    }else{
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if(nSuffix > length(suffixes)){
        stop(paste(x, "is too large!"))
      }
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    ## Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    ## Clear any trailing " and"
    text=gsub(" and$","",text)
    ## Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...){
    as.numeric(paste(..., collapse=""))
  }
  ## Disable scientific notation
  opts <- options(scipen = 100)
  on.exit(options(opts))
  ones <- c("",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine")
  names(ones) <- 0:9
  teens <- c("ten",
             "eleven",
             "twelve",
             "thirteen",
             "fourteen",
             "fifteen",
             "sixteen",
             "seventeen",
             "eighteen",
             "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty",
            "thirty",
            "forty",
            "fifty",
            "sixty",
            "seventy",
            "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand",
                "million",
                "billion",
                "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  j <- helper(x)
  if(th){
    j <- strsplit(j, " ")[[1]]
    first <- j[-length(j)]
    last <- j[length(j)]
    if(last == "one"){
      last <- "first"
    }else if(last == "two"){
      last <- "second"
    }else if(last == "three"){
      last <- "third"
    }else if(last == "five"){
      last <- "fifth"
    }else if(last == "eight"){
      last <- "eighth"
    }else if(last == "nine"){
      last <- "ninth"
    }else if(last == "twelve"){
      last <- "twelfth"
    }else if(last == "twenty"){
      last <- "twentieth"
    }else if(last == "thirty"){
      last <- "thirtieth"
    }else if(last == "forty"){
      last <- "fortieth"
    }else if(last == "fifty"){
      last <- "fiftieth"
    }else if(last == "sixty"){
      last <- "sixtieth"
    }else if(last == "seventy"){
      last <- "seventieth"
    }else if(last == "eighty"){
      last <- "eightieth"
    }else if(last == "ninety"){
      last <- "ninetieth"
    }else{
      last <- paste0(last, "th")
    }
    j <- paste(c(first, last), collapse = " ")
  }
  if(cap_first){
    j <- paste0(toupper(substr(j, 1, 1)),
                substr(j, 2, nchar(j)))
  }
  j
}
