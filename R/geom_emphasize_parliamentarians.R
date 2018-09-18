#' Emphasize certain parliamentarians (for example, female members of parliament) by increasing transparency on the remaining observations.
#' @param expr The observation that you wish to emphasize
#' @examples
#' data <- election_data[election_data$country == "USA"
#' & election_data$house == "Representatives" &
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data,
#' type = "semicircle",
#' party_seats = data$seats,
#' parl_rows = 8)
#'
#' women_in_congress <- c(1, 0, 0, 1)
#' number_of_women <- c(23, 218, 133, 61)
#'
#' usa_data$women <- rep(women_in_congress, number_of_women)
#'
#' ggplot2::ggplot(usa_data, ggplot2::aes(x, y, color=party_long)) +
#' geom_parliament_seats() +
#' geom_emphasize_parliamentarians(women == 1) +
#' theme_ggparliament(legend = FALSE) +
#' ggplot2::scale_colour_manual(values = usa_data$colour, limits = usa_data$party_long) +
#' ggplot2::labs(title = "Women in Congress")
#' @usage
#' geom_emphasize_parliamentarians(expr)
#' @author Zoe Meers
#' @export
#' @importFrom ggplot2 ggplot_add

geom_emphasize_parliamentarians <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "emphMPs")
}


#' @export
ggplot_add.emphMPs <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !(!!object$expr))
  new_layer <- ggplot2::geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = alpha(0.6),
    alpha = 0.6,
    show.legend = FALSE,
    size = 3.5
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}
