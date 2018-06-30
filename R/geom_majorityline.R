#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_majorityrule <- function(mapping = NULL, data = NULL,
                      # type = NA,
                       ...,
                       na.rm = FALSE,
                       show.legend = NA) {

   # Act like an annotation
    # if (!missing(type)) {
    #     data <- data.frame(type = type)
    #     mapping <- aes(type = type)
    #     show.legend <- FALSE
    # }
    
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
                     draw_panel = function(data, panel_params, coord) {
                         ranges <- coord$range(panel_params)

                             data$x    <- 0
                             data$xend <- 0
                             data$y    <- 7.8
                             data$yend <- 10.2
                         
                         
                         GeomMajorityRule$draw_panel(unique(data), panel_params, coord)
                     },
                     
                     default_aes = aes(colour = "slategray", size = 1.5, linetype = 2, alpha = 0.9),
                    
                     
                     draw_key = draw_key_vline
)
