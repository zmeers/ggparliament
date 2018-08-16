#' Draw labels for political parties and seats per party
#' @param names  If TRUE, finds party names from data. Defaults to TRUE.
#' @param seats If TRUE, finds party seats from data. Defaults to TRUE. .
#' @param type Define type. Currently only supports semicircle and horseshoe style parliaments.
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", 
#' seats = data$seats,  
#' parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color=party_long)) + 
#' geom_parliament_seats() + 
#' draw_partylabels(type = "semicircle") + 
#' scale_colour_manual(values = usa_data$colour, limits = usa_data$party_long)  + 
#' theme_ggparliament()
#' @author Zoe Meers
#' @export


draw_partylabels <- function(type = c('semicircle','horseshoe'),
                             names = TRUE,
                             seats = TRUE) {
  structure(
    list(
      type = type,
      names = names,
      seats = seats
    ),
    class = "partyLabels"
  )
}

ggplot_add.partyLabels <- function(object, plot, object_name) {
  party_short <- seats <- colour <- x <- y <- NULL
  new_dat <- plot$data %>%
    dplyr::filter(row == max(row)) %>%
    dplyr::group_by(party_short, seats, colour) %>%
    dplyr::summarise(mean_x = mean(x), mean_y = mean(y))

  pos_movement_horseshoe <- c(new_dat$mean_x + 2)
  pos_movement_semicircle <- c(new_dat$mean_x + 0.8)
  neg_movement_horseshoe <- c(new_dat$mean_x - 2)
  neg_movement_semicircle <- c(new_dat$mean_x - 0.8)


  if (object$type == "horseshoe") {
    if (!(object$names) & !(object$seats)) {
      plot
    } else if (!(object$names) & object$seats) {
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
    } else if (object$names & !(object$seats)) {
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
    } else if (object$names & object$seats) {
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
  } else if (object$type == "semicircle") {
    if (!(object$names) & !(object$seats)) {
      plot
    } else if (!(object$names) & object$seats) {
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
    } else if (object$names & !(object$seats)) {
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
    else if (object$names & object$seats) {
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
  }

  else {
    warning("Warning: parliament layout is not supported.")
  }
}
