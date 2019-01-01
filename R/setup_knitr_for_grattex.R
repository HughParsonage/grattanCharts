#' Setup knitr for Grattan reports
#' @param chunktimings.txt The file to write chunk timings
#' @param bundle_chart_data Whether or not to bundle chart data.
#' @param theGlobalEnv Must be .GlobalEnv.
#' @param chunktimings_width Passed to \code{format} the number of characters to display in the chunktimings time field. By default, 5, which allows less than 1000 seconds
#' per chunk.
#' @param alignAllowedExtra A character vector of single characters to add
#' as permitted values to pass
#' to the \code{align} argument to \code{\link[xtable]{xtable}}. By default, 
#' \code{c("R", "N")} to avoid spurious warnings when these are used.
#' @param halt_if_xtable_unattached If \code{TRUE}, \code{xtable} must
#' be attached when the function is called. If \code{NULL}, the default
#' a message is printed recommending it be attached.  It should be 
#' attached so that the parts of this function affecting \code{xtable}
#' have a chance of being effected. If \code{FALSE}, neither an error
#' nor a message is given.
#' @param error Passed to \code{knitr::opts_chunk$set(error = < >)}. By default,
#' \code{FALSE}, so that errors in chunks halt compilation.
#' 
#' @return Called for its side-effect: when knitr is in progress, sets up
#' knitr for consistent formatting.
#' 
#' @export

setup_knitr_for_grattex <- function(chunktimings.txt = "CHUNKTIMINGS.txt",
                                    bundle_chart_data = FALSE,
                                    theGlobalEnv = .GlobalEnv,
                                    chunktimings_width = 5,
                                    alignAllowedExtra = c("R", "N"),
                                    halt_if_xtable_unattached = NULL,
                                    error = FALSE) {
  if (is.null(halt_if_xtable_unattached) || halt_if_xtable_unattached) {
    if (!{"xtable" %in% .packages()}) {
      if (is.null(halt_if_xtable_unattached)) {
        message("package:xtable does not appear to be attached, ",
                "yet `halt_if_xtable_unattached = NULL`. ", 
                "Placing\n\t",
                "library(xtable)\n",
                "before library(grattanCharts) is recommended. ")
      } else {
        stop("package:xtable does not appear to be attached, ",
             "yet `halt_if_xtable_unattached = NULL`. ", 
             "Placing\n\t",
             "library(xtable)\n",
             "before library(grattanCharts) is recommended.")
      }
    }
  }
  
  
  
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
      ggplot2::update_geom_defaults("text", list(size = 18/(14/5), family = "helvet"))
      ggplot2::update_geom_defaults("label", list(size = 18/(14/5), family = "helvet"))
    } else {
      ggplot2::update_geom_defaults("text", list(size = 18/(14/5)))
      ggplot2::update_geom_defaults("label", list(size = 18/(14/5)))
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
  
  options("xtable.tabular.environment" = "tabularx")
  options("xtable.width" = "\\linewidth")
  options("xtable.booktabs" = TRUE,
          "xtable.floating" = FALSE,
          "xtable.table.placement" = NULL)
  options("xtable.sanitize.text.function" = identity)
  options("xtable.include.rownames" = FALSE)
  options("xtable.sanitize.colnames.function" = function(x) sprintf("\\textbf{%s}", x))
  
  
  assignInNamespace(".alignStringToVector", ns = "xtable",
                    value = {
                      .alignStringToVector <- function(aString) {
                        ## poor mans parsing - separating string of form "l{2in}llr|p{1in}c|{1in}"
                        ## into "l{2in}" "l"  "l"  "r" "|" "p{1in}" "c" "|{1in}"
                        aString.Align <- character(0);
                        aString.Width <- character(0);
                        wString <- aString
                        while( nchar(wString) > 0) {
                          aString.Align <- c(aString.Align, substr(wString, 1, 1))
                          ## is it followed by a brace?
                          thisWidth <- ""
                          if ( nchar(wString) > 1 & substr(wString, 2, 2) == "{") {
                            beforeNextBrace <- regexpr("[^\\]\\}", wString)
                            if (beforeNextBrace <0 ) {
                              stop("No closing } in align string")
                            }
                            thisWidth <- substr(wString, 2, beforeNextBrace + 1)
                            wString <- substr(wString, beforeNextBrace + 2, nchar(wString))
                          } else {
                            wString <- substr(wString, 2, nchar(wString))
                          }
                          aString.Width <- c(aString.Width, thisWidth)
                        }
                        #change here:
                        alignAllowed <- c("l","r","p","c","|","X", alignAllowedExtra)
                        
                        if (any( !(aString.Align %in% alignAllowed))) {
                          warning("Nonstandard alignments in align string")
                        }
                        res <- paste(aString.Align, aString.Width, sep = "")
                        res
                      }
                    })
  invisible(NULL)
}

