#' A function that calculates the coordinates of parliamentary seats in incomplete circular parliaments
#' E.g. The US (semicircle) and Australian (horsehoe) shaped parliaments
#' @param N the total of number of seats
#' @param M the number of rows in parliament
#' @param limits the limits to seq the radii between- controls the 'shape' of the parliament
#' @param segment the percentage of a full circle for the final plot- defaults to 0.5 (a semicircle)
#' @author
#' Zoe Meers, Rob Hickman

calc_coordinates <- function(N, M, limits, segment = 0.5) {
  # controls the spread of the seats
  # tigher limits = more 'pinched' circle
  radii <- seq(limits[1], limits[2], len = M)

  counts <- numeric(M)

  pts <- do.call(
    rbind,
    lapply(1:M, function(i) {
      # find how many seats for this parl_row
      counts[i] <<- round(N * radii[i] / sum(radii[i:M]))
      # seq from 0-180degress for the row for the cartesian position
      ### ROB- Need to symmetry-ise this for non 0.5/1 values of segment ###
      theta <- seq(0, segment * 2 * pi, len = counts[i])
      # subtract the seats already plotted from N
      # N becomes 'seats left to calculate'
      N <<- N - counts[i]

      # wrap this into a df
      # calculate x and y coords
      data.frame(
        x = radii[i] * cos(theta),
        y = radii[i] * sin(theta),
        row = i,
        theta = theta
      )
    })
  )

  # arrange by angle then row
  # assume 'first' party starts in bottom left
  pts <- pts[order(-pts$theta, -pts$row), ]
  pts
}
