#' Create a knitr document to generate charts
#' @description Not to be used for reports: clone the grattex repository instead. Use-case is
#' creating an atlas of charts.
#' @param path A file path, either to a filename to create, or a directory into which to create a file.
#' Will be assumed to be a directory, unless it ends in \code{.Rnw} in which case it will be taken
#' to be the filename.
#' @param rstudio Should the RStudio API be used (if available)? 
#' @details
#' If using RStudio, will pop to the document when completed.
#' @export
#' 

start_rnw <- function(path, rstudio = TRUE) {
  if (length(path) != 1L) {
    stop("`path` had length ", length(path), ", but must be length-one.")
  }
  if (!is.character(path)) {
    stop("`path = ", deparse(substitute(path)), "` was type ", typeof(path), ", ",
         "but must be a string.")
  }
  
  if (grepl("\\.Rnw$", path, ignore.case = TRUE)) {
    .dir <- dirname(path)
    .file <- path
  } else {
    .dir <- path
    .file <- file.path(path, "Report.Rnw")
  }
  
  hutils::provide.dir(.dir)
  
  download_failure <-
    download.file("https://raw.githubusercontent.com/HughParsonage/grattex/master/grattan.cls",
                  destfile = file.path(.dir, "grattan.cls"),
                  quiet = TRUE)
  
  if (download_failure) {
    stop("grattan.cls failed to download from master branch (and be updated).")
  }
  
  
  
  hutils::provide.dir(paste0(.dir, "/logos"))
  logos <-
    c("Bhp.pdf",
      "GrattanSVGLogo.pdf",
      "UOM-Pos_S_PMS.pdf",
      "Vic_Gov_Logo-2016.pdf",
      "aus-gov-logo-stacked-black.pdf")
  
  for (l in logos) {
    download_logos_failure <-
      download.file(paste0("https://raw.githubusercontent.com/HughParsonage/grattex/master/logos/", l),
                    destfile = file.path(.dir, "logos", l),
                    mode = "wb",
                    quiet = TRUE)
    
    if (download_logos_failure) {
      stop(l, " failed to download from master branch. (May be out-of-date.)")
    }
  }
  
  writeLines(c("\\documentclass{grattan}", 
               "", 
               "\\title{Title}", 
               "\\author{Author}", 
               "", 
               "\\GrattanReportNumber{0}", 
               "", 
               "\\begin{document}", 
               "<<knitrOpts, include=FALSE>>", 
               "library(knitr)", 
               "@",
               "",
               "<<grattanCharts-setup, include=FALSE>>=",
               "library(grattanCharts)", 
               "setup_knitr_for_grattex()",
               "@",
               "",
               "",
               "",
               "\\end{document}", 
               ""),
             .file)
  
  if (rstudio && 
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::verifyAvailable()) {
    rstudioapi::navigateToFile(.file, line = 18L)
  }
  invisible(.file)
}

