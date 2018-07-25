## Functions to create parliaments for ggparliament
## Zoe Meers
##
##

#' A function that prepares data for parliamentary plots
#' @param election_data aggregate election results
#' @param parl_rows number of rows in parliament
#' @param party_seats seats per party
#' @param type type of parliament (horseshoe, semicircle, circle, classroom, opposing benches)
#'
#' @examples
#' data <- ggparliament::election_data magrittr::`%>%` filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' @author
#' Zoe Meers, Rob Hickman
#' @export
parliament_data <- function(election_data = NULL,
                            parl_rows = NULL,
                            party_seats = election_data$seats,
                            type = c(
                              "horseshoe",
                              "semicircle",
                              "circle",
                              "classroom",
                              "opposing_benches"
                            )) {
  # for horseshoe shaped parliaments- e.g. Australia
  if (type == "horseshoe") {
    # calculate the layout of the final plot from supplied data
    parl_layout <- calc_coordinates(sum(party_seats), parl_rows, c(8, 10))
    # add in a column for the party names
  }

  else if (type == "semicircle") {
    parl_layout <- calc_coordinates(sum(party_seats), parl_rows, c(1, 2))
  }

  else if (type == "circle") {
    parl_layout <- calc_coordinates(sum(party_seats), parl_rows, c(1, 2), segment = 1)
  }

  else if (type == "classroom") {
    # calculate parl_layour by expanding a grid of rows vs the length each row needs to be
    parl_layout <- expand.grid(
      y = 1:parl_rows,
      x = seq_len(ceiling(sum(party_seats) / parl_rows))
    )

    # remove the extra seats that are added by expanding a grid
    # removes from either end of back row
    ### ROB - Clean up/ find better way // and also make work for odd numbers left over? ###
    leftovers <- nrow(parl_layout) - sum(party_seats)
    parl_layout <- parl_layout[-which(parl_layout$y == max(parl_layout$y) &
      parl_layout$x %in% c(
        tail(1:max(parl_layout$x), leftovers / 2),
        head(1:max(parl_layout$x), leftovers / 2)
      )), ]
  }

  else if (type == "opposing_benches") {
    parl_layout <- expand.grid(
      x = 1:parl_rows,
      y = seq_len(ceiling(sum(party_seats) / parl_rows))
    )

    leftovers <- nrow(parl_layout) - sum(party_seats)
    parl_layout <- parl_layout[-which(parl_layout$y == max(parl_layout$y) &
      parl_layout$x %in% c(
        tail(1:max(parl_layout$x), leftovers / 2),
        head(1:max(parl_layout$x), leftovers / 2)
      )), ]
  }

  else if (type == NULL) {
    warning("Warning: parliament layout not supported.")
  }

  # bind layout results back to expanded election_data?
  if (!is.null(election_data)) {
    # bind the coordinates to the uncounted original data
    parl_data <- tidyr::uncount(election_data, party_seats)
    parl_data <- dplyr::bind_cols(parl_data, parl_layout)

    # otherwise just return the coordinates with the party names attached
  } else {
    parl_data <- parl_layout
  }
  return(parl_data)
}

#' Combine left and right bench for opposing bench-style parliaments
#' @param left left hand side
#' @param right right hand side
#' @author Zoe Meers
#' @export
combine_opposingbenches <- function(left=NA, right=NA) {
  left + patchwork::plot_spacer() + right
}

#' Highlight governments or parties in control of the legislature
#' @examples
#' data <- ggparliament::election_data magrittr::`%>%` filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color = party_long)) + geom_parliament_seats() + geom_highlight_government(government == 1) + theme_ggparliament()
#' @author Zoe Meers
#' @source https://yutani.rbind.io/post/2017-11-07-ggplot-add/
#' @export
geom_highlight_government <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "highlight")
}

ggplot_add.highlight <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = "black",
    show.legend = FALSE,
    size = 1,
    shape = 21,
    stroke = 2
  )
  plot$layers <- append(new_layer, plot$layers)
  plot
}

#' Parliament seats
#' The parliament seats geom is used for plotting data from parliament_data().
#' @param x the x coordinates
#' @param y the y coordinates
#' @param colour the colour variable
#' @param type the type of parliament ("semicircle", "circle", "horseshoe", "opposing_benches", "classroom")
#' @examples
#' data <- ggparliament::election_data magrittr::`%>%` filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color = party_long)) + geom_parliament_seats() + theme_ggparliament()
#' @author Zoe Meers
#' @export
geom_parliament_seats <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
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
      size = 1,
      ...
    )
  )
}

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
    size = 1,
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

#' Highlight elected women
#' Define the "women" variable in the function.
#' @examples
#' geom_emphasize_parliamentarians(female == 1)
#' @author Zoe Meers
#' @export

geom_emphasize_parliamentarians <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "emphMPs")
}

ggplot_add.emphMPs <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !(!!object$expr))
  new_layer <- geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = alpha(0.4),
    alpha = 0.4,
    show.legend = FALSE,
    size = 5
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}



#' Show electoral quotas
#' Define the quota variable in the function.
#' @examples
#' geom_electoral_quota(quota == 1)
#' @author Zoe Meers
#' @export

geom_electoral_quota <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "quota")
}

ggplot_add.quota <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = "black",
    shape = 13,
    show.legend = FALSE,
    size = 3
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}



#' Show hangins seats
#' Define hanging seats in the function
#' @examples
#' geom_hanging_seats(hanging_seats == 1)
#' @author Zoe Meers
#' @export


geom_hanging_seats <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "hangingseats")
}

ggplot_add.hangingseats <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- geom_point(
    data = new_data,
    mapping = plot$mapping,
    fill = "white",
    shape = 21,
    size = 3,
    stroke = 0.5
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}
