#' A function that prepares data for parliamentary plots
#' @param election_data aggregate election results
#' @param parl_rows number of rows in parliament
#' @param party_seats seats per party
#' @param plot_order plot the data in a specified order
#' @param group grouping variable for separate chunks. e.g. opposing benches in UK parliament
#' @param type type of parliament (horseshoe, semicircle, circle, classroom, opposing benches)
#'
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
                            group = NULL,
                            plot_order = NULL,
                            type = c(
                              "horseshoe",
                              "semicircle",
                              "circle",
                              "classroom",
                              "opposing_benches"
                            )) {
  tail <- head <- first <- NULL

  # for horseshoe shaped parliaments- e.g. Australia
  if (type == "horseshoe") {
    # calculate the layout of the final plot from supplied data
    parl_layout <- calc_coordinates(sum(party_seats), parl_rows, c(8, 10))
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
    leftovers <- nrow(parl_layout) - sum(party_seats)
    parl_layout <- parl_layout[-which(parl_layout$y == max(parl_layout$y) &
      parl_layout$x %in% c(
        tail(1:max(parl_layout$x), leftovers / 2),
        head(1:max(parl_layout$x), leftovers / 2)
      )), ]
  }

  else if (type == "opposing_benches") {
    # get the seats for each group
    # opposing benches by definition needs grouping for each bench
    gov_seats <- sum(party_seats[which(group == 1)])
    opp_seats <- sum(party_seats[which(group == 0)])

    bench_seats <- c(gov_seats, opp_seats)

    threshold <- ceiling(sum(gov_seats, opp_seats) / 2)

    # if there is a nice divisor for the majority threshold use this
    # else use 12
    nrows <- which(threshold %% seq_len(threshold) == 0)

    if (any(nrows > 10 & nrows < 15)) {
      nrows <- nrows[first(which(nrows > 10))]
    } else {
      nrows <- 12
    }

    # lapply expanding the group
    parl_layout <- lapply(bench_seats, function(seats, rows) {
      parl_layout <- expand.grid(x = 1:rows, y = seq_len(ceiling(seats / rows)))
      leftovers <- nrow(parl_layout) - seats

      # legacy code for taking leftovers from both sides
      # decided to do it from right side only
      # parl_layout <- parl_layout[-which(parl_layout$y == max(parl_layout$y) &
      #                                     parl_layout$x %in% c(
      #                                       tail(1:max(parl_layout$x), leftovers / 2),
      #                                       head(1:max(parl_layout$x), leftovers / 2)
      #                                      )), ]

      parl_layout <- parl_layout[-((nrow(parl_layout) - leftovers) + 1:nrow(parl_layout)), ]

      return(parl_layout)
    }, rows = nrows)

    # the x axis space between the two benches
    spacer <- 5

    #space the benches and rbind
    parl_layout[[2]]$x <- parl_layout[[2]]$x + max(parl_layout[[1]]$x + spacer)
    parl_layout <- rbind(parl_layout[[1]], parl_layout[[2]])
    
    #bind in the election data
    #can probably be parsed out to later if we include grouping in other geoms
    election_data <- election_data[order(-group, -party_seats),]
  }
  
  else if (is.null(type) | !type %in% c("horseshoe","semicircle","circle","classroom","opposing_benches")) {
    warning("Warning: parliament layout 'type' not supported.")
  }
  
  #if election data is not null, bind layout to original data
  if(!is.null(election_data)) {
    
    if (type != "opposing_benches") {
      parl_data <- as.data.frame(election_data[rep(row.names(election_data),party_seats),])
      
      if(!is.null(plot_order)) {
        #order the data not by seats- e.g. by government then seats
        parl_data <- parl_data[order(-rep(plot_order, party_seats)),]
      }
      
    } else {
      parl_data <- as.data.frame(election_data[rep(row.names(election_data), party_seats[order(-group, -party_seats)]),])
      #not implemented ordering yet- seems unlikely it would ever not be by largest party in bench system?
    }
    
    parl_layout <- cbind(parl_data, parl_layout)
  }
  
  return(parl_layout)
}