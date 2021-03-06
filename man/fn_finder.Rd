% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{fn_finder}
\alias{fn_finder}
\title{Get a calling function's name from within the function}
\usage{
fn_finder(
  skip_frames = 1,
  skip_names = "(FUN)|(.+apply)|(replicate)",
  ret_stack = FALSE,
  extra_perf_per_level = "\\t"
)
}
\arguments{
\item{skip_frames}{The level in the calling stack to look. 1 is in the current
function, 2 is one before, etc.}

\item{skip_names}{Names returned to skip, these are not real function names but
generalized values used internally}

\item{ret_stack}{If TRUE, return the stack trace}

\item{extra_perf_per_level}{This is prepended by R and will be removed from the output}
}
\value{
The name of the calling function at level `skip_frames` in the stack trace
}
\description{
Get a calling function's name from within the function
}
\details{
Not for external use. Use [func_name()] instead
}
