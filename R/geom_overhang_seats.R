
#' Draw overhang seats in MMP electoral systems
#' @param expr Expr refers to the designated overhang seats.
#' @examples
#' \donttest{
#' germany <- data.frame(
#'    year = 2013,
#'    seats = c(64, 63, 311, 193),
#'    government = c(0, 0, 1, 1),
#'    color = c("#BE3075",
#'    "#64A12D", "#000000",
#'     "#EB001F"),
#'    party = c("The Left",
#'    "Alliance 90/The Greens",
#'    "Christian Democratic Union",
#'    "Social Democratic Party")
#' )
#' german_data <- parliament_data(
#' election_data = germany,
#' parl_rows = 11,
#' party_seats = germany$seats,
#' type = "semicircle"
#' )
#' german_data$overhang_seats <- rep(c(1, 0, 1, 0, 1, 0, 1, 0),
#' c(16, 295, 11, 182, 3, 61, 3, 60))
#' ggplot2::ggplot(german_data, ggplot2::aes(x,y,colour = party_short)) +
#'  geom_parliament_seats() +
#'  geom_overhang_seats(overhang_seats == 1) +
#'  theme_ggparliament +
#'  ggplot2::scale_color_manual(values = as.character(german_data$color),
#'  limits = as.character(german_data$party))
#'  }
#' @usage
#' geom_overhang_seats(expr)
#' @author Zoe Meers
#' @export


geom_overhang_seats <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "hangingseats")
}

ggplot_add.hangingseats <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- ggplot2::geom_point(
    data = new_data,
    mapping = plot$mapping,
    fill = "white",
    show.legend = FALSE,
    shape = 21,
    size = 2,
    stroke = 0.5
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}
