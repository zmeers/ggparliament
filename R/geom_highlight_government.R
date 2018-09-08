#' Highlight governments or parties in control of the legislature by encircling the points.
#' @param expr Expr refers to the observation that you wish to highlight.
#' @param colour Colour of the highlight
#' @param size Size of highlighter
#' @param shape Shape of highlight
#' @param stroke Size of stroke shape
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", 
#' party_seats = data$seats, 
#' parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color = party_long)) + 
#' geom_parliament_seats() + 
#' geom_highlight_government(government == 1) + 
#' theme_ggparliament()
#' @usage geom_highlight_government(expr)
#' @author Zoe Meers
#' @source https://yutani.rbind.io/post/2017-11-07-ggplot-add/
#' @export
geom_highlight_government <- function(expr, colour = "black", size = 2, shape = 19, stroke = 1.5) {
  structure(list(expr = rlang::enquo(expr),
                 colour = colour,
                 size = size,
                 shape = shape,
                 stroke = stroke), class = "highlight")
}

ggplot_add.highlight <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- ggplot2::geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = object$colour,
    show.legend = FALSE,
    size = object$size,
    stroke = object$stroke
    shape = object$shape
  )
  plot$layers <- append(new_layer, plot$layers)
  plot
}
