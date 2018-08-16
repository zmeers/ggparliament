
#' Draw overhang seats in MMP electoral systems
#' @param expr Exp refers to the designated overhang seats.
#' @examples
#' data <- ggparliament::election_data %>%
#'  filter(country == "Germany" 
#'  & year == "2013") %>% 
#'  mutate(seats = gsub("255", "311", seats)) %>%
#'  mutate(seats = as.numeric(as.character(seats))) %>%
#'  filter_all(all_vars(!grepl('Christian Social Union in Bavaria',.)))
#' overhangseats <- c(1, 0, 1, 0, 1, 0, 1, 0)
#' number_overhangseats <- c(16, 295, 11, 182, 3, 61, 3, 60)
#' german_data <- parliament_data(
#'  election_data = data,
#'  parl_rows = 11,
#'  party_seats = data$seats,
#'  type = "semicircle"
#' )
#' german_data$overhang_seats <- rep(overhangseats, number_overhangseats)
#' ggplot(german_data, aes(x,y,colour = party_short)) +
#'  geom_parliament_seats() +
#'  geom_overhang_seats(overhang_seats == 1) +
#'  theme_ggparliament
#' @usage 
#' geom_overhang_seats(expr)
#' @author Zoe Meers
#' @export


geom_overhang_seats <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "hangingseats")
}

ggplot_add.hangingseats <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- ggplot2::geom_point(
    data = new_data,
    mapping = plot$mapping,
    fill = "white",
    shape = 21,
    size = 2,
    stroke = 0.5
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}
