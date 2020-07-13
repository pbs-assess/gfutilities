#' Round all values in an arbitrarily complex [list]. All character values
#' are ignored and returned as they were
#'
#' @details This is a recursive function and therefore can have an arbitrary
#' nesting of lists.
#'
#' @param lst A [list] of arbitrary complexity
#' @param digits The number of decimal points to round all numeric values to
#'
#' @return A [list] in the same format as `lst` but with all values rounded to
#' `digits` decimal points
#' @export
round_list <- function(lst = NULL,
                       digits = 2){
  if(is.null(lst)){
    return(NULL)
  }
  if(!length(lst)){
    return(NULL)
  }
  if(class(lst) != "list"){
    # At this point lst is a single non-list object (data frame, matrix, vector, etc)
    if(class(lst) == "character"){
      return(lst)
    }else if(class(lst) == "data.frame"){
      return(round_data_frame(lst, digits))
    }else if(class(lst) == "matrix"){
      return(as.matrix(round_data_frame(as.data.frame(lst))))
    }else if(class(lst) == "array"){
      n_arr_dims <- length(dim(lst))
      if(n_arr_dims == 3){
        return(round_3d_array(lst, digits))
      }else if(n_arr_dims == 4){
        return(round_4d_array(lst, digits))
      }else if(n_arr_dims == 5){
        return(round_5d_array(lst, digits))
      }else{
        stop("Arrays greater than 5 dimensions are not implemented",
             call. = FALSE)
      }
    }else{
      return(round(lst, digits))
    }
  }

  # At this point lst is guaranteed to be a list of one or greater
  nms <- names(lst)
  out_first <- round_list(lst[[1]], digits)
  out_therest <- round_list(lst[-1], digits)
  if(class(out_therest) == "list"){
    out <- c(list(out_first), out_therest)
  }else{
    out <- list(out_first, out_therest)
  }
  names(out) <- nms
  out[sapply(out, is.null)] <- NULL
  out
}

#' Round all numeric values found in a [data.frame] to a specified number of decimal points
#'
#' @details Columns which are not numeric will be returned unmodified
#' @param df A [data.frame]
#' @param digits The number of decimal points to round all numeric values to
#'
#' @return A [data.frame] identical to the input `df` but with all numerical values
#' rounded
#' @importFrom purrr map_df
#' @importFrom dplyr %>%
#' @export
round_data_frame <- function(df, digits = 2){
  map_df(df,~{
    tryCatch(round(.x, digits),
             error = function(e) .x)

  }) %>% as.data.frame
}

#' Round all numeric values found in a multidimensional [array]
#' to a specified number of decimal points
#'
#' @param arr The array
#' @param digits  The number of decimal points to round all numeric values to
#'
#' @return An array identical to the input `arr` but with all numerical values rounded
#' @export
round_3d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 3){
    stop("Not a 3D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    new_arr[i, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, , ]), digits = digits))
  }
  new_arr
}

#' @rdname round_3d_array
#' @export
round_4d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 4){
    stop("Not a 4D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    for(j in seq_len(dims[2])){
      new_arr[i, j, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, j, , ]), digits = digits))
    }
  }
  new_arr
}

#' @rdname round_3d_array
#' @export
round_5d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 5){
    stop("Not a 5D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    for(j in seq_len(dims[2])){
      for(k in seq_len(dims[3])){
        new_arr[i, j, k, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, j, k, , ]), digits = digits))
      }
    }
  }
  new_arr
}

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
