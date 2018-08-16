#' Draw total number of seats in the middle of the parliament
#' @param n  The number of total seats in the legislature.
#' @param inherit.aes Inherits aes
#' @param size Size of font
#' @param colour Colour of label
#' @param type Type of parliament (horseshoe, semicircle)
#' @description This function currently supports the semicircle and horseshoe parliament layouts.
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", 
#' party_seats = data$seats, 
#' parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color=party_long)) + 
#' geom_parliament_seats() + 
#' draw_totalseats(n = 435, type = 'semicircle') + 
#' theme_ggparliament()
#' @author Zoe Meers
#' @export


draw_totalseats <- function(n = NULL,
                            inherit.aes = TRUE,
                            size = 8,
                            colour = "black",
                            type = c("horseshoe", "semicircle")) {
  structure(
    list(
      n = n,
      inherit.aes = inherit.aes,
      size = size,
      type = type,
      colour = colour
    ),
    class = "totalLabels"
  )
}

ggplot_add.totalLabels <- function(object, plot, object_name) {
  if (object$type == "horseshoe") {
    plot + ggplot2::annotate("text",
      x = 0, y = 3,
      label = object$n,
      fontface = "bold",
      size = object$size,
      colour = object$colour
    )
  }
  if (object$type == "semicircle") {
    plot + ggplot2::annotate("text",
      x = 0, y = 0.2,
      label = object$n,
      fontface = "bold",
      size = object$size,
      colour = object$colour
    )
  } else {
    warning("Warning: parliament layout is not supported.")
  }
}
