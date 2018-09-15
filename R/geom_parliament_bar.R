#' Add a bar showing proportion of seats by party in parliament
#' @param party The party name variable in your data frame.
#' @param colour The colours associated with each political party.
#' @param label If label = TRUE, print the percentage above the bar.
#' @examples
#' \donttest{
#' data <- election_data[election_data$country == "USA" &
#' election_data$house == "Representatives" &
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data,
#' type = "semicircle",
#' party_seats = data$seats,
#' parl_rows = 8)
#' ggplot2::ggplot(usa_data, ggplot2::aes(x, y, color = party_long)) +
#' geom_parliament_seats() +
#' geom_parliament_bar(colour, party_long) +
#' ggplot2::scale_colour_manual(values = usa_data$colour, limits = usa_data$party_long)  +
#' theme_ggparliament()
#' }
#' @author Zoe Meers
#' @export
#' @importFrom ggplot2 ggplot_add


geom_parliament_bar <- function(colour = colour, party = party, label = TRUE) {
  structure(
    list(
      colour = rlang::enquo(colour),
      label = label,
      party = rlang::enquo(party)
    ),
    class = "parliamentBar"
  )
}

#' @export
ggplot_add.parliamentBar <- function(object, plot, object_name) {
  count <- mutate <- n <- proportion <- proportion1 <- start <- end <- p <- group_no <- label <- NULL


  y_min <- min(plot$data$y, na.rm = TRUE)
  y_max <- max(plot$data$y, na.rm = TRUE)

  x_max <- max(plot$data$x, na.rm = TRUE)
  x_min <- min(plot$data$x, na.rm = TRUE)
  
  difference <- x_max - x_min 
  
  new_dat <- plot$data
  new_dat <- dplyr::mutate(new_dat, group_no = match(!!object$party, unique(!!object$party)))
  new_dat <- dplyr::count(new_dat, group_no, p = !!object$party, c = !!object$colour)
  new_dat$proportion <- new_dat$n/sum(new_dat$n)
  new_dat$proportion1 <- new_dat$proportion * difference
  new_dat$start = cumsum(new_dat$proportion1) - x_max
  new_dat$end <- c(x_min, (cumsum(new_dat$proportion1)[-nrow(new_dat)] - x_max))

  
  if(object$label == TRUE){
    plot + 
      geom_rect(data = new_dat, 
                ggplot2::aes(xmin = start, xmax = end, fill = p, 
                    ymin = max(plot$data$y)+0.2, ymax = max(plot$data$y)+0.5),
                inherit.aes = FALSE, show.legend = FALSE) + 
      ggrepel::geom_text_repel(ggplot2::aes(x = rowMeans(cbind(start, end)), y = max(plot$data$y)+0.55, label = scales::percent(proportion)), vjust = 0, segment.size = 0.02, nudge_y = 0.05, direction = "x", data = new_dat, inherit.aes = FALSE) +
      ggplot2::scale_fill_manual(values = new_dat$c, limits = new_dat$p)
  } else{
    plot + 
      geom_rect(data = new_dat, 
                ggplot2::aes(xmin = start, xmax = end, fill = p, 
                    ymin = max(plot$data$y)+0.2, ymax = max(plot$data$y)+0.5),
                inherit.aes = FALSE, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = new_dat$c, limits = new_dat$p)
  }
}
