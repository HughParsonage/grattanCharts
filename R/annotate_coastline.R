#' Annotate coastline
#' @description Used in a \code{ggplot2} call to add Sydney or Melbourne's
#' @param city Which city is to be annotated with its coastline?
#' @param coastline.color Colour of the path.
#' @examples 
#' \dontrun{
#' ggplot(...) + 
#'   geom_polygon() +
#'   annotate_coastline("Sydney")
#' }
#' @export

annotate_coastline <- function(city = c("Melbourne", "Sydney", "Brisbane"), coastline.color = theGrey) {
  city <- match.arg(city)
  coastline <- switch(city,
                      "Melbourne" = Melbourne_coastline,
                      "Sydney" = Sydney_coastline,
                      "Brisbane" = Brisbane_coastline)
  ggplot2::annotate("path",
                    x = coastline$long,
                    y = coastline$lat,
                    group = 1,
                    color = coastline.color,
                    alpha = 0.3)
}
