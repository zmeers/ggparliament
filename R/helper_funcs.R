#' @title Arrange Dot Layout
#' @description Function to calculate the position of polar co-ords for a dot layout parliament plot
#' @param portion A numeric value specifying what proportion of a full circle should be used for drawing the plot.
#' @param nrows If \code{style = "dots"}, a numeric value indicating how many rows to use.
#' @param group The grouping of the data to be lapply-d to the function (i.e. to work out positions for each group on a separate graph)

arrange_dot_layout <- function(portion, nrows, group, df = long_df){
  total_seats <- length(which(df$group == group))
  polar <- structure(list(azimuth = numeric(sum(total_seats)),
                          radius = numeric(sum(total_seats))),
                     class = "data.frame",
                     row.names = seq_len(sum(total_seats)))

  # seats per row is circumference of each ring
  circ <- floor(2*(portion*nrows)*pi)
  nperrow <- floor(seq(12, circ, length.out = nrows))

  # modify for based upon excess seats
  remainder <- sum(nperrow) - nrow(polar)
  i <- nrows
  while (remainder > 0) {
    nperrow[i] <- nperrow[i] - 1L
    remainder <- remainder - 1L
    if (i == 3) {
      i <- nrows
    } else {
      i <- i - 1L
    }
  }
  while (remainder < 0) {
    nperrow[i] <- nperrow[i] + 1L
    remainder <- remainder + 1L
    if (i == 1) {
      i <- nrows
    } else {
      i <- i - 1L
    }
  }

  nperrow <- sort(nperrow)

  #if any nperrow are negative this won't work
  if(any(nperrow < 0) | is.unsorted(nperrow)){
    stop("choose more suitable 'portion' or 'row; arguments")
  }

  # y position (which ring)
  ring <- rep(seq_len(nrows) + 3L, times = nperrow)
  polar[["radius"]] <- head(ring, nrow(polar))
  # x position within ring
  pos <- unlist(lapply(nperrow, function(x) seq(0, portion, length.out = x)))
  polar[["azimuth"]] <- head(pos, nrow(polar))

  polar <- polar[order(polar$azimuth, polar$radius),]

  return(polar)
}
