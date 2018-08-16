#' Show electoral quotas
#' Define the quota variable in the function.
#' @param expr Exp refer to the seats that fill an electoral quota.
#' @examples
#' geom_electoral_quota(quota == 1)
#' @usage
#' geom_electoral_quota(expr)
#' @author Zoe Meers
#' @export

geom_electoral_quota <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "quota")
}

ggplot_add.quota <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- ggplot2::geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = "black",
    shape = 8,
    show.legend = FALSE,
    size = 2
  )
  plot$layers <- append(plot$layers, new_layer)
  plot
}
