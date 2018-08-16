#' A function that prepares data for parliamentary plots
#' @param election_data aggregate election results
#' @param parl_rows number of rows in parliament
#' @param party_seats seats per party
#' @param type type of parliament (horseshoe, semicircle, circle, classroom, opposing benches)
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", 
#' party_seats = data$seats, 
#' parl_rows = 8)
#' @author
#' Zoe Meers, Rob Hickman
#' @export
parliament_data <- function(election_data = NULL,
                            parl_rows = NULL,
                            party_seats = election_data$seats,
                            type = c("horseshoe", "semicircle",
                                     "circle", "opposing_benches",
                                     "classroom")) {
  tail <- head <- NULL
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

  if (is.null(type)) {
    stop("`Type` is a necessary argument. Be sure to define the parliament layout in the function!")
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
