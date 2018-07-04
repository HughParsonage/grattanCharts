#' Setup knitr for Grattan reports
#' @param chunktimings.txt The file to write chunk timings
#' @param bundle_chart_data Whether or not to bundle chart data.
#' @param theGlobalEnv Must be .GlobalEnv.
#' @param error Passed to \code{knitr::opts_chunk$set(error)}.
#' @param chunktimings_width Passed to \code{format} the number of characters to display in the chunktimings time field. By default, 5, which is suitable for 
#' @return Called for its side-effect: when knitr is in progress, sets up
#' knitr for consistent formatting.
#' 
#' @export

setup_knitr_for_grattex <- function(chunktimings.txt = "CHUNKTIMINGS.txt",
                                    bundle_chart_data = FALSE,
                                    theGlobalEnv = .GlobalEnv,
                                    chunktimings_width = 5,
                                    error = FALSE){
  if (isTRUE(getOption('knitr.in.progress'))){
    BUNDLE.CHART.DATA <- bundle_chart_data
    
    .finished <- FALSE
    # invisible to hide 'TRUE'
    invisible(file.create(chunktimings.txt))
    knitr::knit_hooks$set(timeit = function(before) {
      if (before) {
        assign(".current.time", value = Sys.time(), envir = theGlobalEnv)
      } else {
        .duration <- formatC(as.numeric(round(difftime(Sys.time(), .current.time, units = "secs"), 1)), 
                             # ensure one decimal place, right-aligned (width = 5)
                             format = "f", flag = "#", digits = 1, width = 5)
        if(!.finished)
          write(
            paste0(.duration, 
                   "\t", 
                   knitr::opts_current$get(name = "label")),
            file = chunktimings.txt,
            ncolumns = 1,
            append = TRUE)
      }
    })
    
    if (requireNamespace("sysfonts", quietly = TRUE)) {
      helvets <- function(font = c("regular", "bold", "italic")) {
        font <- match.arg(font)
        system.file("extdata", 
                    "logos",
                    "Fonts",
                    "helvetic",
                    switch(font, 
                           regular = "uhvr8a.pfb",
                           bold = "uhvb8a.pfb",
                           italic = "uhvro8a.pfb"),
                    package = "grattanCharts")
      }
      
      sysfonts::font_add("helvet",
                         regular = helvets("regular"),
                         bold = helvets("bold"),
                         italic = helvets("italic"))
    }
    
    knitr::opts_chunk$set(echo = FALSE, 
                          message = FALSE, 
                          warning = TRUE,
                          error = error,  # continue on errors
                          
                          cache = FALSE,
                          
                          dev = if (BUNDLE.CHART.DATA) c('pdf', 'png') else 'pdf',
                          fig.ext = if (BUNDLE.CHART.DATA) c('pdf', 'png') else 'pdf',
                          fig.showtext = TRUE,
                          fig.path = "atlas/", 
                          fig.width = fig_width,
                          out.width = paste0(out_width, "in"), 
                          fig.height = fig_height,
                          timeit = TRUE,
                          out.height = paste0(out_height, "in"))
    START.TIME <- Sys.time()
  }
  invisible(NULL)
}

