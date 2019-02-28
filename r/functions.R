#' Get a list of the arguments used within any function call
#'
#' @return A list of the argument values used in a function call
#'
#' @examples
#' eg <- function(a = 1, b = 2, c = 5){
#'   get.args()
#' }
#' eg()
#' eg(10, c = 20)
get.args <- function(){
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
