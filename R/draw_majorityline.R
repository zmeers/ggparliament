#' Draw majority line
#' @examples
#' draw_majorityline(n = 76, type = 'horseshoe')
#' @author Zoe Meers
#' @export


draw_majoritythreshold <- function(...,
                                   n = NULL,
                                   type = c(
                                     "horseshoe",
                                     "semicircle",
                                     "opposing_benches"
                                   )) {
  structure(
    list(
      type = type,
      n = n
    ),
    class = "majorityLine"
  )
}


ggplot_add.majorityLine <- function(object, plot, object_name) {
  new_dat <- plot$data %>%
    dplyr::filter(government == 1) %>%
    dplyr::filter(dplyr::row_number() == object$n & object$n+2)
  x_pos <- new_dat$x
  y_pos_oppbenches <- new_dat$y

  


  if (object$type == "semicircle") {
    plot +
      ggplot2::geom_segment(aes(y = 0.8, yend = max(plot$data$y)+0.2, x = 0, xend = 0), colour = "grey", size = 0.8, linetype = 2, alpha = 0.5) +
      ggplot2::annotate("text", x = 0.85, y = max(plot$data$y) + 0.2, label = paste0(object$n, " seats needed for a majority."))
  } else {
    if (object$type == "horseshoe") {
      plot +
        ggplot2::geom_segment(aes(y = 7.5, yend = 10.5, x = 0, xend = 0), colour = "grey", size = 0.8, linetype = 2, alpha = 0.5) +
        ggplot2::annotate("text", x = 0, y = 6, label = paste0(object$n, " seats\nneeded for a\nmajority."))
    } else {
      if (object$type == "opposing_benches") {
        plot +
          ggplot2::geom_segment(aes(y = y_pos_oppbenches, yend = y_pos_oppbenches, x = min(plot$data$x), xend = max(plot$data$x)), colour = "grey", size = 0.8, linetype = 2, alpha = 0.5) +
          ggplot2::annotate("text", x = 0.1, y = y_pos_oppbenches - 5, label = paste0(object$n, " seats needed for a majority."), angle = 90)
      } else {
        warning("parliament layout not supported")
      }
    }
  }
}
