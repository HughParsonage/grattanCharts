#' Automatic palette adjustment
#' 
#' @name gpal
#' @param n The number of variables/factors over which the palette is to paint.
#' @param dark Should a dark palette be used? (Only available for n=2.)
#' @param reverse (logical) Option to reverse the palette.
#' @author Hugh Parsonage
#' @export
#' @return A vector of HTML colours to be used.

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
