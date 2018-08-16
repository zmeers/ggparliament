
#' Combine left and right bench for opposing bench-style parliaments
#' @param left left hand side
#' @param right right hand side
#' @author Zoe Meers
#' @usage 
#' combine_opposingbenches(left, right)
#' @export
combine_opposingbenches <- function(left = NA, right = NA) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  left + patchwork::plot_spacer() + right
}
