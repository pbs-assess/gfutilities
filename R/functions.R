#' Get a calling function's name from within the function
#'
#' @details Not for external use. Use [func_name()] instead
#'
#' @param skip_frames The level in the calling stack to look. 1 is in the current
#' function, 2 is one before, etc.
#' @param skip_names Names returned to skip, these are not real function names but
#' generalized values used internally
#' @param ret_stack If TRUE, return the stack trace
#' @param extra_perf_per_level This is prepended by R and will be removed from the output
#'
#' @return The name of the calling function at level `skip_frames` in the stack trace
fn_finder <- function(skip_frames = 1,
                      skip_names = "(FUN)|(.+apply)|(replicate)",
                      ret_stack = FALSE,
                      extra_perf_per_level = "\t"){

  prefix <- sapply(3 + skip_frames + 1:sys.nframe(), function(i){
    sys.call(sys.parent(n = i))[[1]]
  })
  prefix[grep(skip_names, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    stop("Could not find any calling function at stack level ", skip_frames,
         call. = FALSE)
  }else if(ret_stack){
    paste(rev(prefix), collapse = "|")
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra_perf_per_level,
                                 length(prefix) - 1),
                             collapse = ""),
                       retval)
    }
    retval
  }
}

#' Returns a calling function's name `levels_up` levels up the stack trace
#'
#' @param levels_up How many levels back in the stack trace to look for the
#' function name
#'
#' @return A calling function's name `levels_up` levels up the stack trace
#' @export
#'
#' @examples
#' f <- function(){
#'   message("You are in ", func_name())
#' }
func_name <- function(levels_up = 1){
  stopifnot(!is.null(levels_up))
  stopifnot(class(levels_up) == "numeric")
  stopifnot(length(levels_up) == 1)
  stopifnot(levels_up >= 0)

  fn_name <- fn_finder(skip_frames = levels_up)
  fn_name <- gsub("\t+", "", fn_name)
  fn_name <- gsub("\ +", "", fn_name)
  fn_name
}

#' Get a list of the arguments used within any function call
#'
#' @return A list of the argument values used in a function call
#'
#' @export
#'
#' @examples
#' library(gfutilities)
#' eg <- function(a = 1, b = 2, c = 5){
#'   get_args()
#' }
#' eg()
#' eg(10, c = 20)
get_args <- function(){
    def.call <- sys.call(-1)
    def <- get(as.character(def.call[[1]]), mode="function", sys.frame(-2))
    act.call <- match.call(definition = def, call = def.call)
    def <- as.list(def)
    def <- def[-length(def)]
    act <- as.list(act.call)[-1]

    def.nm <- names(def)
    act.nm <- names(act)
    inds <- def.nm %in% act.nm
    out <- def
    out[inds] <- act
    out
}


curr.fn.finder <- function(skipframes = 0,
                           skipnames = "(FUN)|(.+apply)|(replicate)",
                           ret.if.none = "Not in function",
                           ret.stack = FALSE,
                           extra.perf.per.level = "\t"){
  ## Get the current function name from within the function itself.
  ## Used to prepend the function name to all messages so that the
  ## user knows where the message came from.
  prefix <- sapply(3 + skipframes + 1:sys.nframe(), function(i){
    currv <- sys.call(sys.parent(n = i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    return(ret.if.none)
  }else if(ret.stack){
    return(paste(rev(prefix), collapse = "|"))
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra.perf.per.level, length(prefix) - 1), collapse = ""), retval)
    }
    return(retval)
  }
}

get.curr.func.name <- function(){
  ## Returns the calling function's name followed by ": "
  func.name <- curr.fn.finder(skipframes = 1) # skipframes=1 is there to avoid returning getCurrFunc itself
  ## Strip extraneous whitespace
  func.name <- gsub("\t+", "", func.name)
  func.name <- gsub("\ +", "", func.name)
  func.name <- paste0(func.name,": ")
  return(func.name)
}
