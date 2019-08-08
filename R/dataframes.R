#' 'Rasterify' column names. Change x and y column names into lower case or upper case
#'
#' @param df data frame
#' @param inv If TRUE, change x and y column names to upper case
#'
#' @return modified data frame
rasterify_cols <- function(df, inv = FALSE){
  nms <- colnames(df)
  has_x <- FALSE
  has_y <- FALSE
  if("X" %in% nms){
    has_x <- TRUE
    x_ind <- match("X", nms)
  }
  if("x" %in% nms){
    if(has_x){
      stop("Both capital and small x exist as column names", call. = FALSE)
    }
    has_x <- TRUE
    x_ind <- match("x", nms)
  }

  if("Y" %in% nms){
    has_y <- TRUE
    y_ind <- match("Y", nms)
  }
  if("y" %in% nms){
    if(has_y){
      stop("Both capital and small y exist as column names", call. = FALSE)
    }
    has_y <- TRUE
    y_ind <- match("y", nms)
  }
  if(!has_x){
    stop("Data frame does not contain an x column", call. = FALSE)
  }
  if(!has_y){
    stop("Data frame does not contain an y column", call. = FALSE)
  }
  colnames(df)[x_ind] <- ifelse(inv, "X", "x")
  colnames(df)[y_ind] <- ifelse(inv, "Y", "y")
  df
}

#' Remove any asterisks found in the given data frame x
#'
#' @param x a data frame
#'
#' @return a list of 2, first is data frame without
#'  asterisks, second is matrix of TRUE for for position
#'  so that the asterisks can be reapplied after formatting.
#' @export
remove.asterisks <- function(x){

  list(apply(x,
             c(1,2),
             function(y){
               as.numeric(sub("\\*+", "", y))
             }),
       apply(x, c(1,2), function(y)grep("\\*+", y)))
}

#' Add two asterisks to the data frame in the positions
#'
#' @param x a data frame
#' @param w matrix 'where' to add asterisks to
#'
#' @return a data frame
#' @export
add.asterisks <- function(x, w){

  p <- x[w == 1]
  p[!is.na(p)] <- paste0("**", p[!is.na(p)])
  x[!is.na(p)] <- p[!is.na(p)]
  x
}
