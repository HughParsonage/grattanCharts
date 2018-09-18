#' Run code conditional on knitting
#' @param yes_expr,no_expr Expressions to run if knitr is running or if it is not running.
#' @export 

if_knitting <- function(yes_expr, no_expr) {
  if (is_knitting()) {
    yes_expr
  } else {
    no_expr
  }
}
