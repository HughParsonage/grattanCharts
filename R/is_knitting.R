#' Is knitting
#' @export
#' @return \code{TRUE} if knitr is progress

is_knitting <- function() isTRUE(getOption('knitr.in.progress'))
