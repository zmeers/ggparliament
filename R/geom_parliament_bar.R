#' Add a bar showing proportion of seats by party in parliament
#' @param party The party name variable in your data frame.
#' @param colour The colours associated with each political party.
#' @examples
#' data <- election_data[election_data$country == "USA" & 
#' election_data$house == "Representatives" & 
#' election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data, 
#' type = "semicircle", 
#' party_seats = data$seats,  
#' parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color = party_long)) + 
#' geom_parliament_seats() + 
#' geom_parliament_bar(colour, party_long) + 
#' scale_colour_manual(values = usa_data$colour, limits = usa_data$party_long)  +
#' theme_ggparliament()
#' @author Zoe Meers
#' @export


geom_parliament_bar <- function(colour = colour, party = party) {
  structure(
    list(colour = enquo(colour),
         party  = enquo(party)),
    class = "parliamentBar"
  )
}

ggplot_add.parliamentBar <- function(object, plot, object_name) {
  
  max_y <- max(plot$data$y, na.rm = TRUE)
  min_y <- min(plot$data$y, na.rm = TRUE)
  max_x <- max(plot$data$x, na.rm = TRUE)
  min_x <- min(plot$data$x, na.rm = TRUE)
  
  difference <- max_x - min_x
  y_min <- min_y
  y_max <- max_y
  
  x_max <- max_x
  x_min <- min_x
  
  plot_data <- plot$data
  
  new_data1 <- plot$data %>% 
    count(p = !!object$party, c = !!object$colour) %>% 
    mutate(proportion = n/sum(n)) %>% 
    mutate(proportion1 = proportion * difference) %>% 
    mutate(start = cumsum(proportion1) - x_max)
  new_data1$end <- c(x_min, (cumsum(new_data1$proportion1)[-nrow(new_data1)] - x_max))
  
  plot + 
    geom_rect(data = new_data1, 
              aes(xmin = start, xmax = end, fill = p, 
              ymin = max(plot$data$y)+0.2, ymax = max(plot$data$y)+0.5),
              inherit.aes = FALSE, show.legend = FALSE) + 
    scale_fill_manual(values = new_data1$c, limits = new_data1$p)
  
}
