#' A theme for ggparliament
#' @description
#' Calls the ggparliament theme. A reconstructed opinionated theme_void() ggplot2 theme.
#' @usage
#' theme_ggparliament(legend, background_colour, border)
#' @param legend If legend = `TRUE`, add legend to plot. Defaults to `TRUE`.
#' @param background_colour If background colour = `TRUE`, fill panel with a grey background. Defaults to `FALSE`.
#' @param border If `TRUE` add panel border. Defaults to `FALSE`.
#' @examples
#' data <- election_data[election_data$country == "USA"
#' & election_data$house == "Representatives"
#' & election_data$year == "2016",]
#' usa_data <- parliament_data(election_data = data,
#' type = "semicircle",
#' party_seats = data$seats,
#' parl_rows = 8)
#' ggplot2::ggplot(usa_data, ggplot2::aes(x, y, color = party_long)) +
#' geom_parliament_seats() +
#' geom_highlight_government(government == 1) +
#' theme_ggparliament(legend = TRUE, background_colour = TRUE, border = TRUE)
#' @author Zoe Meers
#' @export

theme_ggparliament <- function(legend = TRUE,
                               background_colour = FALSE,
                               border = FALSE) {
  basic_theme <- ggplot2::theme_void()


  if (legend == TRUE) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "none")
  }



  if (!background_colour) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA)) # white smoke fill
  }


  if (!border) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "#F5F5F5", fill = NA)) # white smoke colour
  }

  basic_theme
}
