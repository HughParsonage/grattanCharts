#' Install the dependencies for powerpoints
#' @description Installs the now-deprecated dependencies necessary to save reasonable-fidelity
#' graphics into PowerPoint.
#' @export install_reporters

install_reporters <- function() {
  devtools::install_github("hughparsonage/ReporteRsjars")
  devtools::install_github("hughparsonage/ReporteRs")
}

