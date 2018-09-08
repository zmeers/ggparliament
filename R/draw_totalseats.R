#' Draw total number of seats in the middle of the parliament
#' @param n  The number of total seats in the legislature.
#' @param inherit.aes Inherits aes
#' @param size Size of font
#' @param colour Colour of label
#' @param angle Angle of text (to flip text when flipping the chart sideways)
#' @param type Type of parliament (horseshoe, semicircle, circle, opposing benches, classroom)
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
                            size = 12,
                            colour = "black",
                            angle = 0,
                            type = c("horseshoe", "semicircle", "opposing_benches", "circle", "classroom")) {
  structure(
    list(
      n = n,
      inherit.aes = inherit.aes,
      size = size,
      type = type,
      colour = colour,
      angle = angle
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
      angle = object$angle,                       
      colour = object$colour
    )
  }
  if (object$type == "semicircle") {
    plot + ggplot2::annotate("text",
      x = 0, y = 0.2,
      label = object$n,
      fontface = "bold",
      angle = object$angle,
      size = object$size,
      colour = object$colour
    )
  } 
  if (object$type == "opposing_benches") {
    plot + ggplot2::annotate("text",
      x = 15, y = 15,
      label = object$n,
      fontface = "bold",
      size = object$size,
      angle = object$angle,
      colour = object$colour
    )
  } 
  if (object$type == "circle") {
    plot + ggplot2::annotate("text",
      x = 0, y = 0,
      label = object$n,
      fontface = "bold",
      size = object$size,
      angle = object$angle,
      colour = object$colour
    )
  } 
  if (object$type == "classroom") {
    plot + ggplot2::annotate("text",
      x = 30, y = max(plot$data$y)+0.5,
      label = object$n,
      fontface = "bold",
      size = object$size,
      angle = object$angle,
      colour = object$colour
    )
  } 
}
