#' Install the dependencies for powerpoints
#' @description Installs the now-deprecated dependencies necessary to save reasonable-fidelity
#' graphics into PowerPoint.
#' @export install_reporters

install_reporters <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is not a strict dependency of 'grattanCharts', ",
         "but is required for `install_reporters()`.")
  }
  devtools::install_github("hughparsonage/ReporteRsjars")
  devtools::install_github("hughparsonage/ReporteRs")
}

