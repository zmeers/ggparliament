#' Draw total number of seats in the middle of the parliament
#' @param n  The number of total seats in the legislature.
#' @param size Size of font
#' @param colour Colour of label
#' @param type Type of parliament (horseshoe, semicircle, circle, opposing benches, classroom)
#' @examples
#' \donttest{
#' data <- election_data[election_data$country == "USA" &
#' election_data$house == "Representatives" &
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data,
#' type = "semicircle",
#' party_seats = data$seats,
#' parl_rows = 8)
#' ggplot2::ggplot(usa_data, ggplot2::aes(x, y, color=party_long)) +
#' geom_parliament_seats() +
#' draw_totalseats(n = 435, type = 'semicircle') +
#' theme_ggparliament()
#' }
#' @author Zoe Meers
#' @export
#' @importFrom ggplot2 ggplot_add


draw_totalseats <- function(n = NULL,
                            size = 12,
                            colour = "black",
                            type = c(
                              "horseshoe",
                              "semicircle",
                              "opposing_benches",
                              "circle",
                              "classroom"
                            )) {
  structure(
    list(
      n = n,
      size = size,
      type = type,
      colour = colour
    ),
    class = "totalLabels"
  )
}
#' @export
ggplot_add.totalLabels <- function(object, plot, object_name) {
  n <- rlang::enquo(n)
  if (object$type == "horseshoe") {
    plot +
      ggplot2::annotate("text",
        x = 0, y = 3,
        label = object$n,
        fontface = "bold",
        size = object$size,
        colour = object$colour
      )
  } else if (object$type == "classroom") {
    plot +
      ggplot2::annotate("text",
        x = max(plot$data$x) / 2,
        y = max(plot$data$y) + 0.2,
        label = object$n,
        fontface = "bold",
        size = object$size,
        colour = object$colour
      )
  } else if (object$type == "opposing_benches") {
    plot +
      ggplot2::annotate("text",
        x = max(plot$data$x) / 2,
        y = max(plot$data$y) / 2,
        label = object$n,
        fontface = "bold",
        size = object$size,
        colour = object$colour
      )
  } else if (object$type == "semicircle") {
    plot +
      ggplot2::annotate("text",
        x = 0, y = 0.2,
        label = object$n,
        fontface = "bold",
        size = object$size,
        colour = object$colour
      )
  } else {
    plot +
      ggplot2::annotate("text",
        x = max(plot$data$x) / 2,
        y = max(plot$data$y) / 2,
        label = object$n,
        fontface = "bold",
        size = object$size,
        colour = object$colour
      )
  }
}
