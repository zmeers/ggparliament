## Functions to create parliaments for ggparliament
## Zoe Meers
##
##

#' A function that prepares data for parliamentary plots
#' @param electiondata aggregate election results
#' @param type type of parliament (horseshow, semicircle, circle, classroom, opposing benches)
#' @param totalseats the total number of seats in parliament
#' @param parlrows number of rows in parliament
#' @param seatspp seats per party
#' @param party_names names of political parties in parliament
#'
#' @example
#'
#' df1 <- parliament_data(electiondata=df1, type="semicircle, seatspp=df1$Number, parlrows=6, totalseats=sum(df1$Number))
#' ggplot(df, aes(x, y, color=party_long)) + geom_point()
#'
#' @author
#' Zoe Meers
parliament_data <- function(electiondata=data,
                            totalseats=NULL, parlrows=NULL,
                            seatspp=NULL,
                            party_names=NULL,
                            type=c(
                              "horseshoe",
                              "semicircle",
                              "circle",
                              "classroom"
                              # "opposing_benches"
                            )) {
  if (type == "horseshoe") {
    seats <- function(N, M) {
      radii <- seq(5.5, 7, len = M)

      counts <- numeric(M)
      pts <- do.call(
        rbind,
        lapply(1:M, function(i) {
          counts[i] <<- round(N * radii[i] / sum(radii[i:M]))
          theta <- seq(0, pi, len = counts[i])
          N <<- N - counts[i]
          data.frame(
            x = radii[i] * cos(theta), y = radii[i] * sin(theta), r = i,
            theta = theta
          )
        })
      )
      pts <- pts[order(-pts$theta, -pts$r), ]
      pts
    }


    election <- function(seats, counts) {
      stopifnot(sum(counts) == nrow(seats))
      seats$party <- rep(1:length(counts), counts)
      seats
    }
    layout <- seats(totalseats, parlrows)
    result <- election(layout, seatspp)
    dat <- tidyr::uncount(electiondata, seatspp)
    dat <- cbind(dat, result)
    return(dat)
  }
  else if (type == "circle") {
    result <- expand.grid(
      x = 1:parlrows,
      y = seq_len(ceiling(sum(seatspp) / parlrows))
    )

    vec <- rep(party_names, seatspp)
    result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
    dat <- tidyr::uncount(electiondata, seatspp)
    dat <- cbind(dat, result)
    return(dat)
  }
  else if (type == "classroom") {
    result <- expand.grid(
      y = 1:parlrows,
      x = seq_len(ceiling(sum(seatspp) / parlrows))
    )

    vec <- rep(party_names, seatspp)
    result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
    dat <- tidyr::uncount(electiondata, seatspp)
    dat <- cbind(dat, result)
    return(dat)
  }
  # else if (type == "opposing_benches") {
  #   result <- expand.grid(
  #     x = 1:parlrows,
  #     y = seq_len(ceiling(sum(seatspp) / parlrows))
  #   )
  # 
  #   # vec <- rep(party_names, seatspp)
  #   # result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
  #   dat <- tidyr::uncount(electiondata, seatspp)
  #   dat <- cbind(dat, result)
  #   return(dat)
  # }
  else {
    seats <- function(N, M) {
      radii <- seq(1, 2, len = M)

      counts <- numeric(M)
      pts <- do.call(
        rbind,
        lapply(1:M, function(i) {
          counts[i] <<- round(N * radii[i] / sum(radii[i:M]))
          theta <- seq(0, pi, len = counts[i])
          N <<- N - counts[i]
          data.frame(
            x = radii[i] * cos(theta), y = radii[i] * sin(theta), r = i,
            theta = theta
          )
        })
      )
      pts <- pts[order(-pts$theta, -pts$r), ]
      pts
    }


    election <- function(seats, counts) {
      stopifnot(sum(counts) == nrow(seats))
      seats$party <- rep(1:length(counts), counts)
      seats
    }
    layout <- seats(totalseats, parlrows)
    result <- election(layout, seatspp)
    dat <- tidyr::uncount(electiondata, seatspp)
    dat <- cbind(dat, result)
    return(dat)
  }
}




#' A ggplot2 theme for parliament plots
theme_parliament <- function() {
  theme_void()
}

#' A ggplot2 geom for parliament plots
#' @param type type of parliament (horseshow, semicircle, circle, classroom, opposing benches)
#' @param totalseats the total number of seats in parliament
#' @param parlrows number of rows in parliament
#' @param seatspp seats per party
#' @param party_names names of political parties in parliament
#'
#' @example
#' df <- data.frame(Party = c("GUE/NGL", "S&D", "Greens/EFA", "ALDE", "EPP", "ECR", "EFD", "NA"),Number = c(35, 184, 55, 84, 265, 54, 32, 27))
#' df1 <- parliament_data(df)
#' ggplot() + geom_parliament_dots(type="semicircle, seatspp=df1$Number, parlrows=6, totalseats=sum(df1$Number))
#'
#' @author
#' Zoe Meers
geom_parliament_waffle<- function(totalseats=NULL, parlrows=NULL, seatspp=NULL, size = NULL, party_names=NULL, type="opposing_benches") {
  
  result <- expand.grid(
    x = 1:parlrows,
    y = seq_len(ceiling(sum(seatspp) / parlrows))
  )
  
  vec <- rep(party_names, seatspp)
  result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
  
  # Plot it
  geom_tile(data = result, aes(x, y, fill=as.character(party)),colour="white", size=0.8)
  
}


#' Combine left and right bench for opposing bench-style parliaments
#' @param left left hand side
#' @param right right hand side
#' @author Zoe Meers
combine_opposingbenches <- function(left=NA, right=NA) {
  left + patchwork::plot_spacer() + right
}

#' Highlight governments
#' Define the government variable in the function.
#' @example 
#' geom_highlight_parliament(government==1)
#' @author Zoe Meers
#' @source https://yutani.rbind.io/post/2017-11-07-ggplot-add/
geom_highlight_government <- function(expr) {
  structure(list(expr = rlang::enquo(expr)), class = "highlight")
}

ggplot_add.highlight <- function(object, plot, object_name) {
  new_data <- dplyr::filter(plot$data, !!object$expr)
  new_layer <- geom_point(
    data = new_data,
    mapping = plot$mapping,
    colour = alpha("black", 1),
    show.legend = FALSE,
    size = 5
  )
  plot$layers <- append(new_layer, plot$layers)
  plot
}
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomParliamentSeats <- ggplot2::ggproto("GeomParliamentSeats", ggplot2::Geom,
                     required_aes = c("x", "y", "colour"),
                     non_missing_aes = c("size", "shape"),
                     default_aes = ggplot2::aes(
                       shape = 19, colour = "black", size=3, fill = NA,
                       alpha = NA, stroke = 1
                     ),
                     
                     draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                       coords <- coord$transform(data, panel_params)
                       ggname("geom_parliament_seats",
                              grid::pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = gpar(
                                  col = alpha(coords$colour, coords$alpha),
                                  fill = alpha(coords$fill, coords$alpha),
                                  fontsize = coords$size * ggplot2::.pt + coords$stroke * .stroke,
                                  lwd = coords$stroke * .stroke
                                )
                              )
                       )
                     },
                     
                     draw_key = ggplot2::draw_key_point
)
#' Parliament seats
#' The parliament seats geom is used for plotting data from parliament_data().
#' @param x
#' @param y
#' @param colour 
#' ggplot(us_senate, aes(x,y,colour=colour))
#' @author Zoe Meers
geom_parliament_seats <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      size=3,
      ...
    )
  )
}

