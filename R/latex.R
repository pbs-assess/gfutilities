#' Escape all instances of the percent symbol properly for latex
#'
#' @param vec Character vector
#'
#' @return The input vector with all instances of the percent symbol escaped
#' properly for latex
#'
#' @export
#'
#' @examples
#' latex.perc(c("2.5%", "50%", "97.5%"))
latex.perc <- function(vec){
  gsub("%", "\\\\%", vec)
}

#' Create a string of ampersands seperated by spaces
#'
#' @param n The number of ampersands in the string
#'
#' @return A string of ampersands seperated by spaces. The string will have
#' one leading space and one trailing space
#' @export
#'
#' @examples
#' latex.amp(10)
latex.amp <- function(n = 1){
  paste0(rep(" &", n), " ", collapse = "")
}

#' Create a string comprised of each element in the vector with an
#' ampersand in between
#'
#' @param vec A vector of strings
#'
#' @return A string comprised of each element in the vector with an
#' ampersand in between. The string will have one leading and one
#' trailing space
#' @export
#'
#' @examples
#' latex.paste(c("a", "b", "c"))
latex.paste <- function(vec){
  paste(" ", vec, " ", collapse = " & ")
}

#' Place \\textbf macro around text for latex boldface
#'
#' @param txt The text
#'
#' @return The text surrounded by \\textbf{}
#' @export
#'
#' @examples
#' latex.bold("Test")
latex.bold <- function(txt){
  paste0("\\textbf{", txt, "}")
}

#' Place \\mathbf macro and dollar signs around text for latex math boldface
#'
#' @param txt The text
#'
#' @return The text surrounded by \\mathbf{} and dollar signs
#' @export
#'
#' @examples
#' latex.math.bold("Test")
latex.math.bold <- function(txt){
  paste0("$\\mathbf{", txt, "}$")
}

#' Place \\emph macro around text for latex italics
#'
#' @param txt The text
#'
#' @return The text surrounded by \\emph{}
#' @export
#'
#' @examples
#' latex.italics("Test")
latex.italics <- function(txt){
  paste0("\\emph{", txt, "}")
}

#' Place \\underline macro around text for latex underlining
#'
#' @param txt The text
#'
#' @return The text surrounded by \\underline{}
#' @export
#'
#' @examples
#' latex.under("Test")
latex.under <- function(txt){
  paste0("\\underline{", txt, "}")
}

#' Create a string which has been glued together using multi-line-cell
#' macro for latex
#'
#' mlc must be defined as follows in the latex code:
#' \\newcommand{\\mlc}[2][c]{\\begin{tabular}[#1]{@{}c@{}}#2\\end{tabular}}
#'
#' @param latex.vec The vector of strings to glue together
#' @param make.bold the \\textbf macro will be inserted unless math.bold is TRUE
#' @param math.bold if TRUE, the \\mathbf macro will be used
#'
#' @return A string which has been glued together using multi-line-cell
#' macro for latex
#' @export
#'
#' @examples
#' latex.mlc(c("This", "is a", "test"), make.bold = TRUE)
latex.mlc <- function(latex.vec, make.bold = TRUE, math.bold = FALSE){
  if(make.bold){
    if(math.bold){
      latex.vec <- sapply(latex.vec, latex.math.bold)
    }else{
      latex.vec <- sapply(latex.vec, latex.bold)
    }
  }
  latex.str <- paste(latex.vec, collapse = latex.nline)
  paste0("\\mlc{", latex.str, "}")
}

#' Place \\multicolumn macro around text
#'
#' @param ncol The number of columns to span
#' @param just The justification e.g. "l", "c", or "r" for left, center, right
#' @param txt The text
#'
#' @return The given text with the latex \\multicolumn{} macro around it
#' @export
#'
#' @examples
#' latex.mcol(5, "l", "This is a test column header")
latex.mcol <- function(ncol, just, txt){
  paste0("\\multicolumn{", ncol, "}{", just, "}{", txt, "}")
}

#' Place \\multirow macro around text
#'
#' @param nrow The number of rows to span
#' @param just The justification e.g. "l", "c", or "r" for left, center, right
#' @param txt The text
#'
#' @return The given text with the latex \\multirow{} macro around it
#' @export
#'
#' @examples
#' latex.mrow(5, "l", "This is a test row header")
latex.mrow <- function(nrow, just, txt){
  paste0("\\multirow{", nrow, "}{", just, "}{", txt, "}")
}

#' Create a latex string for setting the given font size and space size
#'
#' @param fnt.size Font size
#' @param spc.size Space between text size
#'
#' @return A latex string for setting the given font size and space size
#' @export
#'
#' @examples
#' latex.size.str(10, 11)
latex.size.str <- function(fnt.size, spc.size){
  paste0("\\fontsize{", fnt.size, "}{", spc.size, "}\\selectfont")
}

#' Create a latex string to draw a horizontal line across the columns specified
#'
#' @param cols A string representing the columns to span e.g. "1-5"
#'
#' @return A latex string to draw a horizontal line across the columns specified
#' @export
#'
#' @examples
#' latex.cline("1-5")
latex.cline <- function(cols){
  paste0("\\cline{", cols, "}")
}

#' Create a latex string to draw a horizontal line across the columns specified
#'
#' @param cols A string representing the columns to span e.g. "1-5"
#' @param trim Tells latex to trim the line a bit so that if there are two lines
#' close together they don't touch in the middle (See booktabs package).
#' Can be "l" for left, "r" for right, or "lr" for both sides of line
#'
#' @return A latex string to draw a horizontal line across the columns specified
#' @export
#'
#' @examples
#' latex.cmidr("1-5", "lr")
latex.cmidr <- function(cols, trim = "r"){
  paste0("\\cmidrule(", trim, "){", cols, "}")
}

#' Create a subscript in latex
#'
#' @param main.txt The main part of the text
#' @param subscr.txt The subscripted part of the text
#'
#' @return A latex string with subscripting
#' @export
#'
#' @examples
#' latex.subscr("B", "0")
latex.subscr <- function(main.txt, subscr.txt){
  paste0(main.txt, "\\subscr{", subscr.txt, "}")
}

#' Create a superscript in latex
#'
#' @param main.txt The main part of the text
#' @param supscr.txt The superscripted part of the text
#'
#' @return A latex string with superscripting
#' @export
#'
#' @examples
#' latex.supscr("x", "2")
latex.supscr <- function(main.txt, supscr.txt){
  paste0(main.txt, "\\supscr{", supscr.txt, "}")
}

#' Create a character vector to use in an align argument for xtable()
#'
#' @param num The number of columns
#' @param first.left If TRUE, set the first column to be left-justified
#' @param just The justification of all columns, unless first.left is TRUE.
#' "r", "l", or "c"
#'
#' @return A character vector to use in an align argument for xtable()
#' @export
#'
#' @examples
#' library(gfutilities)
#' library(xtable)
#' d <- data.frame(a = c(1,2,3), b = c(10,20,30))
#' print(xtable(d,
#'              caption = "The table caption",
#'              label = "tab:example",
#'              align = get.align(ncol(d), just = "c")),
#'       caption.placement = "top",
#'       include.rownames = FALSE,
#'       table.placement = "H",
#'       sanitize.text.function = function(x){x},
#'       size = latex.size.str(12, 11))
get.align <- function(num,
                      first.left = TRUE,
                      just = "r"){
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  align
}
