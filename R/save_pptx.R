#' Save plot as PowerPoint
#' @param p A \code{ggplot2} object.
#' @param filename Filename for the PowerPoint file.
#' @param template Either \code{presentation} or \code{report}, the template on Dropbox to be used. 
#' Only useful if connected to Dropbox. Can be overriden by \code{template.file}.
#' @param template.file If set, the file (must have extension \code{.pptx}) to be used in the template.
#' @examples 
#' \dontrun{
#' source("https://gist.githubusercontent.com/HughParsonage/60581c4595f3f10ba201faeacc46cca5/raw/31316307432ee6fef3164e212d21f173ec49d25f/employment-QLD")
#' }
#' 
#' @importFrom magrittr use_series
#' @export
save_pptx <- function(p, filename, template = c("presentation", "report"), template.file = NULL) {
  if (is.null(template.file)) {
    DropboxInfo <- 
      if (Sys.getenv("OS") == "Windows_NT") {
        file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
      } else {
        "~/.dropbox/info.json"
      }
    
    if (file.exists(DropboxInfo)) {
      Path2Dropbox <- 
        jsonlite::fromJSON(DropboxInfo) %>%
        use_series("business") %>%
        use_series("path")
      
      template <- match.arg(template)
      
      template.potx <-
        file.path(Path2Dropbox,
                  "Grattan Team",
                  "Templates", 
                  "Charts",
                  paste0("r-pkg-Charts-for-", template, "s.pptx"))
      
      if (!file.exists(template.potx)) {
        stop("Could not find the ",
             paste0("r-pkg-Charts-for-", template, "s.pptx"),
             " template file in ",
             file.path(Path2Dropbox,
                       "Grattan Team",
                       "Templates", 
                       "Charts"))
      }
      
      template.file <- tempfile(fileext = ".pptx") 
      file.copy(template.potx, template.file, overwrite = TRUE)
    } else {
      stop("Could not find the chart template on Dropbox. ",
           "Ensure you have access to the Grattan Team directory.")
    }
  }
  
  
  fun <- function() print(p)
  ReporteRs::pptx(template = template.file) %>%
  ReporteRs::addSlide(slide.layout = "Slide with chart") %>%
  ReporteRs::addPlot(fun = fun, 
                     fontname_sans = "Arial",
                     vector.graphic = TRUE, 
                     width = 22.16/2.5,
                     height = 14.5/2.5,
                     offx = if (template == "presentation") 1 else 0,
                     offy = if (template == "presentation") 2 else 0) %>%
  ReporteRs::writeDoc(file = filename)
}
