#' Draw majority line
#' @param n The number of seats required for a majority
#' @param label A logical variable for labelling majority threshold. Defaults to TRUE.
#' @param colour The colour of the majority line. Defaults to grey.
#' @examples
#' data <- ggparliament::election_data %>% filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color=party_long)) + geom_parliament_seats() + draw_majoritythreshold3(n = 218, label = FALSE, linecolour = "black") + theme_ggparliament()
#' @author Zoe Meers
#' @export


draw_majoritythreshold3 <- function(...,
                                   n = NULL,
                                   label = TRUE,
                                   linecolour = "black",
                                   linesize = 1,
                                   linetype = 1
                                   ) {
  structure(
    list(
      n = n,
      label = label,
      linecolour = linecolour,
      linesize = linesize,
      linetype = linetype
    ),
    class = "majorityLine3"
  )
}


ggplot_add.majorityLine3 <- function(object, plot, object_name) {
# Sorry, it's in dplyr LOL.
    new_dat_filter <- plot$data %>% 
      mutate(x_round = round(x, 1)) %>% 
      filter(row_number() == object$n)
    x_round_var <- new_dat_filter$x_round

    new_dat <- plot$data %>% 
      mutate(x_round = round(x, 1)) %>% 
      filter(x_round == x_round_var)
 
  if (!object$label) {
    plot +
      ggplot2::geom_segment(aes(y = min(new_dat$y) - 0.5, yend = max(new_dat$y) + 0.5, x = min(new_dat$x), xend = min(new_dat$x)), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = 1)
  } else {
    plot +
      ggplot2::geom_segment(aes(y = min(new_dat$y), yend = max(new_dat$y), x = min(new_dat$x), xend = min(new_dat$x)), colour = object$linecolour, size = object$linesize, linetype = object$linetype, alpha = 1) + 
      ggplot2::annotate("text", x = 0.85, y = max(plot$data$y) + 0.2, label = paste0(object$n, " seats needed for a majority."))
  }

}
