#' Parliament seats
#' The parliament seats geom is used for plotting data from parliament_data()
#' @param mapping Mapping the aesthetics (the x and y coordinates, 
#' as well as the colour of each political party).
#' @param data The outputted parliament_data data frame.
#' @param stat "identity"
#' @param position "identity"
#' @param na.rm If `FALSE`, the default, missing values are 
#' removed with a warning. 
#' If `TRUE`, missing values are silently removed. 
#' @param show.legend If `TRUE`, print legend. 
#' If `FALSE` do not print legend.
#' @param inherit.aes Inherit aes from other ggplot2 functions.
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", party_seats = data$seats,
#' parl_rows = 8)
#' ggplot(usa_data, aes(x = x, y = y, color = party_long)) + 
#' geom_parliament_seats() + 
#' theme_ggparliament()
#' @author Zoe Meers
#' @export
geom_parliament_seats <- function(mapping = NULL, 
                                  data = NULL,
                                  stat = "identity", 
                                  position = "identity",
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomParliamentSeats,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      size = 2
    )
  )
}

#' ggplot2-ggproto
#' @rdname ggplot2-ggproto
#' @name GeomParliamentSeats
#' @format NULL
#' @usage NULL
#' @export
GeomParliamentSeats <- ggplot2::ggproto("GeomParliamentSeats", ggplot2::Geom,
  required_aes = c("x", "y", "colour"),
  non_missing_aes = c("size", "shape"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    size = 2,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)
    ggplot2:::ggname(
      "geom_parliament_seats",
      grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          fontsize = coords$size * ggplot2::.pt + coords$stroke * .stroke,
          lwd = coords$stroke * .stroke
        )
      )
    )
  },

  draw_key = ggplot2::draw_key_point
)
