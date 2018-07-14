#' Draw total number of seats in the middle of the parliament
#' @param n  The number of total seats
#' @param type Type of parliament - supports "horseshoe" or "semicircle"
#' @examples
#' data <- ggparliament::election_data %>% filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, party_names = data$party_short, parl_rows = 8, total_seats = sum(data$seats))
#' ggplot(usa_data, aes(x, y, color=party_long)) + geom_parliament_seats() + draw_totalseats(n = 435, type = 'semicircle') + theme_void()
#' @author Zoe Meers
#' @export


draw_totalseats <- function(n = NULL,
                            inherit.aes = TRUE,
                            type = c(
                              "horseshoe",
                              "semicircle"
                            ),
                            size = 8,
                            colour = "black",
                            ...) {
  structure(
    list(
      n = n,
      type = type,
      size = size,
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
  } else{
    warning("Warning: parliament layout is not supported.")
  }
}


#' Draw labels for political parties and seats per party
#' @param party_names  If TRUE, finds party_names from data. Default is set on TRUE.
#' @param party_seats If TRUE, finds party_seats from data. Default is set on TRUE.
#' @examples
#' data <- ggparliament::election_data %>% filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, party_names = data$party_short, parl_rows = 8, total_seats = sum(data$seats))
#' ggplot(usa_data, aes(x, y, color=party_long)) + geom_parliament_seats() + draw_partylabels(type = 'semicircle', party_names = TRUE, party_seats = TRUE) + scale_colour_manual(values = usa_data$colour, limits = usa_data$party_long)  + theme_void()
#' @author Zoe Meers
#' @export


draw_partylabels <- function(inherit.aes = TRUE,
                             party_names = TRUE,
                             party_seats = TRUE,
                             type = c(
                               "horseshoe",
                               "semicircle"
                             ),
                             ...) {
  structure(
    list(
      inherit.aes = inherit.aes,
      party_names = party_names,
      party_seats = party_seats,
      type = type
    ),
    class = "partyLabels"
  )
}

ggplot_add.partyLabels <- function(object, plot, object_name) {
  new_dat <- plot$data %>%
    dplyr::filter(row == max(row)) %>%
    dplyr::group_by(party_short, seats, colour) %>%
    dplyr::summarise(mean_x = mean(x), mean_y = mean(y))

  pos_movement_horseshoe <- c(new_dat$mean_x + 2)
  pos_movement_semicircle <- c(new_dat$mean_x + 0.8)
  neg_movement_horseshoe <- c(new_dat$mean_x - 2)
  neg_movement_semicircle <- c(new_dat$mean_x - 0.8)


  if (object$type == "horseshoe") {
    if (!(object$party_names) & !(object$party_seats)) {
      plot
    } else if (!(object$party_names) & object$party_seats) {
      plot + ggplot2::annotate("text",
        x = ifelse(new_dat$mean_x > 0,
          pos_movement_horseshoe,
          neg_movement_horseshoe
        ),
        y = new_dat$mean_y,
        label = new_dat$seats,
        colour = new_dat$colour,
        vjust = "outward"
      )
    } else if (object$party_names & !(object$party_seats)) {
      plot +
        ggplot2::annotate("text",
          x = ifelse(new_dat$mean_x > 0,
            pos_movement_horseshoe,
            neg_movement_horseshoe
          ),
          y = new_dat$mean_y,
          label = new_dat$party_short,
          colour = new_dat$colour,
          vjust = "outward"
        )
    } else if (object$party_names & object$party_seats) {
      plot +
        ggplot2::annotate("text",
          x = ifelse(new_dat$mean_x > 0,
            pos_movement_horseshoe,
            neg_movement_horseshoe
          ),
          y = new_dat$mean_y,
          label = paste0(new_dat$party_short, "\n(", new_dat$seats, ")"),
          colour = new_dat$colour,
          vjust = "outward"
        )
    }
  }
  if (object$type == "semicircle") {
    if (!(object$party_names) & !(object$party_seats)) {
      plot
    } else if (!(object$party_names) & object$party_seats) {
      plot +
        ggplot2::annotate("text",
          x = ifelse(new_dat$mean_x > 0,
            pos_movement_semicircle,
            neg_movement_semicircle
          ),
          y = new_dat$mean_y,
          label = new_dat$seats,
          colour = new_dat$colour,
          vjust = "outward"
        )
    } else if (object$party_names & !(object$party_seats)) {
      plot +
        ggplot2::annotate("text",
          x = ifelse(new_dat$mean_x > 0,
            pos_movement_semicircle,
            neg_movement_semicircle
          ),
          y = new_dat$mean_y,
          label = new_dat$party_short,
          colour = new_dat$colour,
          vjust = "outward"
        )
    }
    else if (object$party_names & object$party_seats) {
      plot +
        ggplot2::annotate("text",
          x = ifelse(new_dat$mean_x > 0,
            pos_movement_semicircle,
            neg_movement_semicircle
          ),
          y = new_dat$mean_y,
          label = paste0(new_dat$party_short, "\n(", new_dat$seats, ")"),
          colour = new_dat$colour,
          vjust = "outward"
        )
    }
  } else{
    warning("Warning: parliament layout is not supported.")
  }
}
