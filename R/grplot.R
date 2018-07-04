#' Attempt to harness ggplot but set as much to default as possible
#' 
#' @name grplot
#' @param ... arguments passed to ggplot
#' @param reverse (logical) Option to reverse the default palette.
#' @param continuous Expect a continuous colour scale. Using \code{scale_color_continuous} later should still work, but may emit an unwelcome message.
#' 
#' @details Since version 0.8.0 \code{grplot} uses the helvetic fonts automtically if available. 
#' Ensure you have sysfonts installed.
#' 
#' @export 


grplot <- function(..., reverse = FALSE, continuous = FALSE) {
  if (isTRUE(getOption("grattan.bigplot"))) {
    theme_grattan <- function() theme_hugh(base_size = 18)
  } else {
    theme_grattan <- function() theme_hugh(base_size = 10)
  }
  
  ggplot2::update_geom_defaults("point", list(colour = Orange))  #but cf. col.3
  ggplot2::update_geom_defaults("bar", list(fill = DarkOrange, colour = NA))
  ggplot2::update_geom_defaults("line", list(fill = Orange, colour = Orange, size = 2))
  if (continuous) {
    ggplot2::ggplot(...) + 
      ggplot2::scale_color_gradientn(colours = gpal(6, reverse = reverse)) + 
      ggplot2::scale_fill_gradientn(colours = gpal(6, reverse = reverse)) +
      theme_grattan()
  } else {
    ggplot2::ggplot(...) + 
      scale_color_discrete_grattan(reverse = reverse) + 
      scale_fill_discrete_grattan(reverse = reverse) + 
      theme_grattan()
  }
}