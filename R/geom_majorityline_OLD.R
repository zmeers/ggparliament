#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_majority_line <- function(mapping = NULL, data = NULL,
                       type =  c("horseshoe", "semicircle"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA) {
  
    layer(
        data = data,
        mapping = mapping,
        stat = StatIdentity,
        geom = GeomMajorityRule,
        position = PositionIdentity,
        show.legend = show.legend,
        inherit.aes = FALSE,
        params = list(
            na.rm = na.rm,
            type = type,
            ...
        )
    )
}

#' @rdname ggplot2-ggproto
#' @name GeomMajorityRule
#' @format NULL
#' @usage NULL
#' @export
GeomMajorityRule <- ggproto("GeomMajorityRule", Geom,
                     draw_panel = function(data, panel_params, coord, type = c("horseshoe", "semicircle")) {
                         ranges <- coord$range(panel_params)
                         
                         if (type == "horseshoe"){
                           data$x    <- 0.15
                           data$xend <- 0.15
                           data$y    <- 7.5
                           data$yend <- 10.2
                         } else{
                             data$x    <- 0.15
                             data$xend <- 0.15
                             data$y    <- 0.8
                             data$yend <- 1.2
                         }
                         data$x    <- 0.15
                         data$xend <- 0.15
                         data$y    <- 0.8
                         data$yend <- 1.2
                         
                         GeomMajorityRule$draw_panel(unique(data), panel_params, coord, type)
                     },
                    # required_aes = c("type"),
                     default_aes = aes(colour = "gray", size = 0.8, linetype = 1, alpha = 0.8),
                     draw_key = draw_key_vline
)


