#' Draw majority line
#' @param n The number of seats required for a majority
#' @param label A logical variable for labelling majority threshold. Defaults to TRUE.
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
                                   colour = "grey"
                                   ) {
  structure(
    list(
      n = n,
      label = label,
      colour = colour
    ),
    class = "majorityLine"
  )
}


ggplot_add.majorityLine <- function(object, plot, object_name) {
  # group each sequence of rows to find minimum and maximum value of each row.
  new_dat <- plot$data %>% 
    dplyr::mutate(grouped_rows = if_else(row == max(row), row_number(), NA_integer_))%>%
    tidyr::fill(grouped_rows) %>% 
    dplyr::filter(government == 1) %>% 
    dplyr::filter(dplyr::row_number() == object$n)
  
  #in base
#  new_dat <- plot$data
#  row_names <- as.numeric(row.names(new_dat)[which(new_dat$row == max(new_dat$row))])
#  row_gaps <- append(row_names[2:length(row_names)], nrow(new_dat) + 1)
  
#  new_dat$grouped_rows <- rep(row_names, row_gaps - row_names)
#  new_dat <- new_dat[which(government == 1),]
  
  # X position
  x_pos <- new_dat$x
  # Y position
  y_pos <- new_dat$y
  # row number for the majority line row
  group_var_majline <- new_dat$grouped_rows
  # go back to original data and filter based on that group
  # Note: This should be refactored, it's pretty shoddy code.
  new_data1 <- plot$data %>% 
    dplyr::mutate(grouped_rows = if_else(row == max(row), row_number(), NA_integer_))%>%
    tidyr::fill(grouped_rows) %>% 
    dplyr::filter(grouped_rows == group_var_majline)
  
  
  if (!object$label) {
    plot +
      ggplot2::geom_segment(aes(y = min(new_dat$y), yend = max(new_dat$y), x = min(new_dat$x), xend = min(new_dat$x)), colour = object$colour, size = 0.8, linetype = 2, alpha = 1)
  } else {
    plot +
      ggplot2::geom_segment(aes(y = min(new_dat$y), yend = max(new_dat$y), x = min(new_dat$x), xend = min(new_dat$x)), colour = object$colour, size = 0.8, linetype = 2, alpha = 1) + 
      ggplot2::annotate("text", x = 0.85, y = max(plot$data$y) + 0.2, label = paste0(object$n, " seats needed for a majority."))
  }

}
