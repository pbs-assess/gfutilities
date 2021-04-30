#' Round all values in an arbitrarily complex [list]. All character values
#' are ignored and returned as they were
#'
#' @details This is a recursive function and therefore can have an arbitrary
#' nesting of lists.
#'
#' @param lst A [list] of arbitrary complexity
#' @param digits The number of decimal points to round all numeric values to
#' @param print_elem_names Logical. Print names of each list element to the screen
#'
#' @return A [list] in the same format as `lst` but with all values rounded to
#' `digits` decimal points
#' @export
round_list <- function(lst = NULL,
                       digits = 2,
                       print_elem_names = FALSE){

  if(is.null(lst)){
    return(NULL)
  }
  cls <- class(lst)
  if("list" %in% cls && !length(lst)){
    return(NULL)
  }
  if(!"list" %in% cls){
    # At this point lst is a single non-list object (data frame, matrix, vector, etc)
    if("character" %in% cls){
      return(lst)
    }else if("data.frame" %in% cls){
      return(round_data_frame(lst, digits))
    }else if("matrix" %in% cls){
      return(as.matrix(round_data_frame(as.data.frame(lst), digits)))
    }else if("integer" %in% cls){
      return(lst)
    }else if("array" %in% cls){
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
      if(!length(lst)){
        if(print_elem_names){
          stop("The last list element shown above has zero length.\n",
               call. = FALSE)
        }else{
          stop("A list element has zero length.\n",
               call. = FALSE)
        }
      }
      return(round(lst, digits))
    }
  }

  # At this point lst is guaranteed to be a list of one or greater
  nms <- names(lst)
  if(print_elem_names){
    cat(nms[1], "\n")
  }
  out_first <- suppressWarnings(round_list(lst[[1]], digits, print_elem_names))
  out_therest <- suppressWarnings(round_list(lst[-1], digits, print_elem_names))
  if("list" %in% class(out_therest)){
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
#' @importFrom purrr map_df map_chr
#' @importFrom dplyr %>% mutate_if
#' @export
round_data_frame <- function(df, digits = 2){

  cls <- class(df)
  is_tibble <- ifelse("tbl_df" %in% cls, TRUE, FALSE)
  col_cls <- map_chr(df, ~{class(.x)})
  out_df <- map_df(df,~{
    if(class(.x) == "numeric"){
      round(.x, digits)
    }else{
      .x
    }
  }) %>% mutate_if(col_cls == "integer", as.integer)
  if(is_tibble){
    out_df
  }else{
    as.data.frame(out_df)
  }
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
