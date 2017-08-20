#' Automatic palette adjustment
#' @description The grattan palette from darkest to lightest given a number of colours.
#' @name gpal
#' @param n The number of variables/factors over which the palette is to paint.
#' @param dark Should a dark palette be used? (Only available for n = 2.)
#' @param reverse (logical) Option to reverse the palette.
#' @author Hugh Parsonage
#' @examples 
#' library(ggplot2)
#' dat <- data.frame(i = 1:6,
#'                   xmin = 1:6,
#'                   xmax = 1:6 + 1,
#'                   ymin = 0,
#'                   ymax = 1,
#'                   fill = gpal(6),
#'                   stringsAsFactors = FALSE) 
#' ggplot(dat, aes(xmin = xmin, ymin = ymin,
#'                 xmax = xmax, ymax = ymax,
#'                 fill = fill)) +
#'   geom_rect() +
#'   geom_text(aes(x = (xmin + xmax) / 2,
#'                 y = (ymin + ymax) / 2,
#'                 label = i)) +
#'   scale_fill_identity()
#'   
#' # Extended palette available up to 9 colours.
#' dat <- data.frame(i = 1:9,
#'                   xmin = 1:9,
#'                   xmax = 1:9 + 1,
#'                   ymin = 0,
#'                   ymax = 1,
#'                   fill = gpal(9),
#'                   stringsAsFactors = FALSE) 
#' ggplot(dat, aes(xmin = xmin, ymin = ymin,
#'                 xmax = xmax, ymax = ymax,
#'                 fill = fill)) +
#'   geom_rect() +
#'   geom_text(aes(x = (xmin + xmax) / 2,
#'                 y = (ymin + ymax) / 2,
#'                 label = i)) +
#'   scale_fill_identity()
#' @return A vector of HTML colours to be used.
#' @export

gpal <- function(n, dark = TRUE, reverse = FALSE){
  grattan.palette <- list(pal.1, pal.2dark, pal.3, pal.4, pal.5, pal.6)
  
  if(n > 6){
    if(n > 9){
      message('No palette available for that number of categories.', '\n', 'Using gpalx')
      if (reverse) 
        return(gpalx(n)) 
      else 
        return(rev(gpalx(n)))
    } else {
      grattan.palette <- list(pal.1, pal.2, pal.3, pal.4, pal.5, pal.6, pal.7, pal.8, pal.9)
      message("I'm going off-piste: The Palette Of Nine is thine. May John have mercy on your soul.")
    }
  }
  if(!dark) {
    if (n == 2){
      out <- pal.2
    } else {
      warning("no light option for palette ", n)
      out <- grattan.palette[[n]]
    }             
  } else {
    out <- grattan.palette[[n]]
  }
  if (reverse){
    rev(out)
  } else {
    out
  }
}
