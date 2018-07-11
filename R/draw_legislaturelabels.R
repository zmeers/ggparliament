#' Draw legislature labels
#' @param total_seats The number of total seats
#' @param party_names A logical variable for labelling different parties. Defaults to TRUE.
#' @param seats_per_party A logical variable for printing the number of seats per party next to the party name. Defaults to TRUE.
#' @examples
#' data <- election_data[which(election_data$year == 2016 & election_data$country == "USA" & election_data$house == "Representatives"),]
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, party_names = data$party_short, parl_rows = 8, total_seats = sum(data$seats))
#' ggplot(usa_data, aes(x, y, color=party_long)) + geom_parliament_seats() + draw_legislaturelabels(n = 435, party_names = TRUE, seats_per_party = TRUE, type = 'semicircle')
#' @author Zoe Meers
#' @export


draw_legislaturelabels <- function(total_parliamentary_seats = NULL,
                                   total_seats_label = TRUE,
                                   party_names = TRUE,
                                   seats_per_party = TRUE,
                                   type = c(
                                     "horseshoe",
                                     "semicircle"
                                   ),
                                   ...
                                   ) {
  structure(
    list(
      total_parliamentary_seats = total_parliamentary_seats,
      total_seats_label = total_seats_label,
      party_names = party_names,
      seats_per_party = seats_per_party,
      type = type
    ),
    class = "legislatureLabels"
  )
}


ggplot_add.legislatureLabels <- function(object, plot, object_name) {
  new_dat <- plot$data %>%
    dplyr::filter(row == max(row)) %>%
    dplyr::group_by(party_short, seats, colour) %>%
    dplyr::summarise(mean_x = mean(x), mean_y = mean(y)) 
  
  
  
  if (object$type == "semicircle") {
    if (!object$total_seats_label){
      plot
    } else {
      plot + 
      ggplot2::annotate("text", x = 0, y = 0.2, label = object$total_parliamentary_seats, fontface = "bold", size = 8)
    }
    if (!object$party_names){
      plot
    } else {
      plot +
        ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 0.8, label= new_dat$party_short, fontface="bold", colour = new_dat$colour) 
    } 
    if (!object$seats_per_party){
      plot
    } else {
      plot + 
        ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 0.8, label= new_dat$seats, fontface="bold", colour = new_dat$colour) 
    }
    if(object$total_seats_label == TRUE & object$party_names == TRUE & object$seats_per_party == TRUE) {
      plot + 
        ggplot2::annotate("text", x = 0, y = 0.2, label = object$total_parliamentary_seats, fontface = "bold", size = 8) + 
        ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 0.8, label= paste0(new_dat$party_short,"\n", "(", new_dat$seats, ")"), fontface="bold", colour = new_dat$colour) 
    }
  }else{
    if (object$type == "horseshoe") {
      if (!object$total_seats_label){
        plot
      } else {
        plot + 
          ggplot2::annotate("text", x = 0, y = 0.2, label = object$total_parliamentary_seats, fontface = "bold", size = 8)
      }
      if (!object$party_names){
        plot
      } else {
        plot +
          ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 2, label= new_dat$party_short, fontface="bold", colour = new_dat$colour) 
      } 
      if (!object$seats_per_party){
        plot
      } else {
        plot + 
          ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 2, label= new_dat$seats, fontface="bold", colour = new_dat$colour) 
      }
      if(object$total_seats_label == TRUE & object$party_names == TRUE & object$seats_per_party == TRUE) {
        plot + 
          ggplot2::annotate("text", x = 0, y = 0.2, label = object$total_parliamentary_seats, fontface = "bold", size = 8) + 
          ggplot2::annotate("text", x = new_dat$mean_x, y = new_dat$mean_y + 2, label= paste0(new_dat$party_short,"\n", "(", new_dat$seats, ")"), fontface="bold", colour = new_dat$colour) 
      }
    } 
  }
 

}



