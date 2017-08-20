#' Attempt to harness ggplot but set as much to default as possible
#' 
#' @name grplot
#' @param ... arguments passed to ggplot
#' @param reverse (logical) Option to reverse the default palette.
#' @export 


grplot <- function(..., reverse = FALSE){
  if (isTRUE(getOption("grattan.bigplot"))) {
    theme_grattan <- function() theme_hugh(base_size = 23)
  } else {
    theme_grattan <- function() theme_hugh(base_size = 10)
  }
  
  ggplot2::update_geom_defaults("point", list(colour = Orange))  #but cf. col.3
  ggplot2::update_geom_defaults("bar", list(fill = DarkOrange, colour = NA))
  ggplot2::update_geom_defaults("line", list(fill = Orange, colour = Orange, size = 2))
  ggplot2::ggplot(...) + 
    scale_color_discrete_grattan(reverse = reverse) + 
    scale_fill_discrete_grattan(reverse = reverse) + 
    theme_grattan()
}