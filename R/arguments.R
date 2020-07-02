#' Verify that the argument `arg` is of the correct class, length, and
#' within in a set of values
#'
#' @param arg The object to check
#' @param chk_class A character string of the name of the class to ensure `arg` is
#' @param chk_len A numeric value to ensure `arg` has the length of
#' @param chk_is_in A vector of values to ensure `arg` is in
#'
#' @return TRUE, invisibly or the function will throw an error if `arg` does not follow
#' the constraints given; FALSE is not returned
#' @export
#'
#' @examples
#' \dontrun{
#' verify_argument(23, "numeric", 1) # Succeeds
#' verify_argument(23, "numeric", 1, 1:20) # Fails
#' }
verify_argument <- function(arg = NULL,
                            chk_class = NULL,
                            chk_len = NULL,
                            chk_is_in = NULL){

  calling_func_name <- func_name(levels_up = 2)
  calling_args <- get_args()
  if(is.null(arg)){
    message("Error from calling function ", calling_func_name, ":")
    stop("is.null(", calling_args$arg, ") is TRUE",
         call. = FALSE)
  }
  if(!is.null(chk_len) & length(chk_len) != 1){
    message("Error from calling function ", calling_func_name, ":")
    stop("length(", calling_args$chk_len, ") is not equal to 1",
         call. = FALSE)
  }
  if(!is.null(chk_class)){
    if(!any(class(arg) %in% chk_class)){
      message("Error from calling function ", calling_func_name, ":")
      stop("Class requirements (", calling_args$chk_class,
           ") do not include the actual class of ", calling_args$arg, " (",
           class(arg), ")",
           call. = FALSE)
    }
  }
  if(!is.null(chk_len)){
    if(length(arg) != chk_len){
      message("Error from calling function ", calling_func_name, ":")
      stop("length(", calling_args$arg, ") == ", calling_args$chk_len, " is not TRUE",
           call. = FALSE)
    }
  }
  if(!is.null(chk_is_in)){
    if(sum(!is.na(match(chk_is_in, arg))) != length(arg)){
      message("Error from calling function ", calling_func_name, ":")
      stop("Not all values in ", calling_args$arg, " are in ", calling_args$chk_is_in,
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
