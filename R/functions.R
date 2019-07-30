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
