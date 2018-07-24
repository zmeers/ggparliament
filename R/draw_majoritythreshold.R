#' Draw majority line
#' @param n The number of seats required for a majority
#' @param label A logical variable for labelling majority threshold. Defaults to TRUE.
#' @param type Type of parliament (horseshoe, semicircle,opposing benches)
#' @param colour The colour of the majority line. Defaults to grey.
#' @examples
#' data <- ggparliament::election_data %>% filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color=party_long)) + geom_parliament_seats() + draw_majoritythreshold(n = 316, label = FALSE, colour = "black", type = 'semicircle') + theme_ggparliament()
#' @author Zoe Meers
#' @export


draw_majoritythreshold <- function(...,
                                   n = NULL,
                                   label = TRUE,
                                   type = NULL,
                                   colour = "grey"
                                   ) {
  structure(
    list(
      n = n,
      type = type,
      label = label,
      colour = colour
    ),
    class = "majorityLine"
  )
}


ggplot_add.majorityLine <- function(object, plot, object_name) {
  new_dat <- plot$data %>%
    dplyr::filter(government == 1) %>%
    dplyr::filter(dplyr::row_number() == object$n)
  x_pos <- new_dat$x
  y_pos_oppbenches <- new_dat$y

  if (object$type == "semicircle") {
    if (!object$label) {
      plot +
        ggplot2::geom_segment(aes(y = 0.8, yend = max(plot$data$y) + 0.2, x = 0, xend = 0), colour = object$colour, size = 0.8, linetype = 2, alpha = 1)
    } else {
      plot +
        ggplot2::geom_segment(aes(y = 0.8, yend = max(plot$data$y) + 0.2, x = 0, xend = 0), colour = object$colour, size = 0.8, linetype = 2, alpha = 1) +
        ggplot2::annotate("text", x = 0.85, y = max(plot$data$y) + 0.2, label = paste0(object$n, " seats needed for a majority."))
    }
  } else {
    if (object$type == "horseshoe") {
      if (!object$label) {
        plot +
          ggplot2::geom_segment(aes(y = 7.5, yend = 10.5, x = 0, xend = 0), colour = object$colour, size = 0.8, linetype = 2, alpha = 1)
      } else {
        plot +
          ggplot2::geom_segment(aes(y = 7.5, yend = 10.5, x = 0, xend = 0), colour = object$colour, size = 0.8, linetype = 2, alpha = 1) +
          ggplot2::annotate("text", x = 0, y = 6, label = paste0(object$n, " seats\nneeded for a\nmajority."))
      }
    } else {
      if (object$type == "opposing_benches") {
        if (!object$label) {
          plot +
            ggplot2::geom_segment(aes(y = y_pos_oppbenches, yend = y_pos_oppbenches, x = min(plot$data$x), xend = max(plot$data$x)), colour = object$colour, size = 0.8, linetype = 2, alpha = 1)
        } else {
          plot +
            ggplot2::geom_segment(aes(y = y_pos_oppbenches, yend = y_pos_oppbenches, x = min(plot$data$x), xend = max(plot$data$x)), colour = object$colour, size = 0.8, linetype = 2, alpha = 1) +
            ggplot2::annotate("text", x = 0.1, y = y_pos_oppbenches - 5, label = paste0(object$n, " seats needed for a majority."), angle = 90)
        }
      } else {
        warning("Warning: parliament layout not supported.")
      }
    }
  }
}
