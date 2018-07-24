#' A theme for ggparliament
#' @description
#' Calls the ggparliament theme. A reconstructed opinionated theme_void() ggplot2 theme. 
#' @usage 
#' theme_ggparliament()
#' @param legend If legend = `TRUE`, add legend to plot. Defaults to `TRUE`. You can also write "top", "left", "right", "bottom" and the theme will plot the legend on the specified panel edge.
#' @param background_colour If background colour = `TRUE`, fill panel with a grey background. Defaults to `FALSE`.
#' @param border If `TRUE` add panel border. Defaults to `FALSE`.
#' @examples 
#' data <- ggparliament::election_data %>% filter(year == "2016" & country == "USA" & house == "Representatives")
#' usa_data <- parliament_data(election_data = data, type = "semicircle", party_seats = data$seats, parl_rows = 8)
#' ggplot(usa_data, aes(x, y, color = party_long)) + geom_parliament_seats() + geom_highlight_government(government == 1) + theme_ggparliament(legend = TRUE, background = TRUE, border = TRUE)
#' @author 
#' Zoe Meers
theme_ggparliament <- function(legend = TRUE, 
                               background = FALSE, 
                               border = FALSE){
    basic_theme <- theme_void()
    
  
    if(inherits(legend, "character") | legend == TRUE){
        basic_theme <- basic_theme 
        
        if (inherits(legend, "character")) {
            if (regexpr("right", legend)[1] < 0) basic_theme <- basic_theme + theme(legend.position = "right")
            if (regexpr("bottom", legend)[1] < 0) basic_theme <- basic_theme + theme(legend.position = "bottom")
            if (regexpr("left", legend)[1] < 0) basic_theme <- basic_theme + theme(legend.position = "left")
            if (regexpr("top", legend)[1] < 0) basic_theme <- basic_theme + theme(legend.position = "top")
        }
    } else{
        basic_theme <- basic_theme + theme(legend.position = "none")
    }
        

    
    if(!background) {
        basic_theme <- basic_theme 
    } else {
        basic_theme <- basic_theme + theme(panel.background =   element_rect(fill = "#DCDCDC"))
    }
        

    if (!border) {
        basic_theme <- basic_theme 
    } else {
        basic_theme <- basic_theme + theme(panel.border  = element_rect(color = "#DCDCDC", fill = NA))
    }
    
    basic_theme
}
