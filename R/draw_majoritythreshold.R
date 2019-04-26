#' Draw majority threshold
#' @param n The number of seats required for a majority
#' @param label A logical variable for labelling majority threshold. Defaults to TRUE.
#' @param type Type of parliament (horseshoe, semicircle,opposing benches)
#' @param linecolour The colour of the majority line. Defaults to gray.
#' @param linesize The size of the line. Defaults to 1.
#' @param linetype The style of the line. Defaults to 2, or a dashed line.
#' @param linealpha Set the transparency of the line. Defaults to 1.
#' @examples
#' data <- election_data[
#'   election_data$country == "USA" &
#'     election_data$house == "Representatives" &
#'     election_data$year == "2016",
#' ]
#' usa_data <- parliament_data(
#'   election_data = data,
#'   type = "semicircle",
#'   party_seats = data$seats,
#'   parl_rows = 8
#' )
#' ggplot2::ggplot(usa_data, ggplot2::aes(x, y, colour = party_long)) +
#'   geom_parliament_seats() +
#'   draw_majoritythreshold(
#'     n = 218,
#'     label = TRUE,
#'     type = "semicircle"
#'   ) +
#'   theme_ggparliament()
#' @author Zoe Meers
#' @export
#' @importFrom ggplot2 ggplot_add



draw_majoritythreshold <- function(n = NULL,
                                   label = TRUE,
                                   type = c("horseshoe", "semicircle", "opposing_benches"),
                                   linecolour = "black",
                                   linesize = 1,
                                   linetype = 2,
                                   linealpha = 1) {
  structure(
    list(
      n = n,
      type = type,
      label = label,
      linecolour = linecolour,
      linesize = linesize,
      linetype = linetype,
      linealpha = linealpha
    ),
    class = "majorityLine"
  )
}

# calculate the positions of the thresholds
#' @export
ggplot_add.majorityLine <- function(object, plot, object_name) {
  filter <- group_by <- NULL
  new_dat <- dplyr::filter(plot$data, dplyr::row_number() == object$n)
  x_pos <- new_dat$x
  y_pos_oppbenches <- new_dat$y[1]

  if (object$type == "semicircle") {
    if (!object$label) {
      plot +
        ggplot2::geom_segment(ggplot2::aes(y = 0.8, yend = max(plot$data$y) + 0.1, x = 0, xend = 0), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha)
    } else {
      plot +
        ggplot2::geom_segment(ggplot2::aes(y = 0.8, yend = max(plot$data$y) + 0.1, x = 0, xend = 0), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha) +
        ggplot2::annotate("text", x = 0.85, y = max(plot$data$y) + 0.1, label = paste0(object$n, " seats needed for a majority."))
    }
  } else {
    if (object$type == "horseshoe") {
      if (!object$label) {
        plot +
          ggplot2::geom_segment(ggplot2::aes(y = 7.5, yend = 10.5, x = 0, xend = 0), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha)
      } else {
        plot +
          ggplot2::geom_segment(ggplot2::aes(y = 7.5, yend = 10.5, x = 0, xend = 0), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha) +
          ggplot2::annotate("text", x = 0, y = 6, label = paste0(object$n, " seats\nneeded for a\nmajority."))
      }
    } else {
      if (object$type == "opposing_benches") {
        if (!object$label) {
          plot +
            ggplot2::geom_segment(aes(y = y_pos_oppbenches + 0.5, yend = y_pos_oppbenches + 0.5, x = min(plot$data$x), xend = max(plot$data$x) / 2),
              colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha
            )
        } else {
          plot +
            ggplot2::geom_segment(aes(y = y_pos_oppbenches + 0.5, yend = y_pos_oppbenches + 0.5, x = min(plot$data$x), xend = max(plot$data$x) / 2),
              colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = object$linealpha
            ) +
            ggplot2::annotate("text", x = max(plot$data$x) / 1.9, y = y_pos_oppbenches - (y_pos_oppbenches / 15), label = paste0(object$n, " seats needed\nfor a majority."))
        }
      } else {
        warning("Warning: parliament layout not supported.")
      }
    }
  }
}
