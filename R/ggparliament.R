#' @title Parliament Plots
#' @description Produce a Wikipedia-style hemicycle plot of parliamentary composition
#' @param data A data frame
#' @param party The name of a variable in \code{data} containing the names of parties. If \code{data} is missing, this can be a vector of party names.
#' @param seats1 The name of a variable in \code{data} containing the number of seats for each party. If \code{data} is missing, this can be a vector of seat counts.
#' @param seats2 Optionally, the name of a variable in \code{data} containing a second set of numbers of seats for each party. If \code{data} is missing, this can be a vector of seat counts. This can be useful for showing, e.g., pre-/post-election changes in numbers of seats.
#' @param style A character string specifying the style of the graph. Either \dQuote{arc} or \dQuote{dots}.
#' @param portion A numeric value specifying what proportion of a full circle should be used for drawing the plot.
#' @param nrows If \code{style = "dots"}, a numeric value indicating how many rows to use.
#' @param \dots Additional arguments passed to \code{\link[ggplot2]{geom_point}}.
#' @details This transforms a data frame of party seat counts into ggplot scatterplot using \code{\link[ggplot2]{coord_polar}}.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @references
#' \url{http://stackoverflow.com/questions/42729174/creating-a-half-donut-or-parliamentary-seating-chart}
#'
#' @examples
#' d <- data.frame(Party = c("GUE/NGL", "S&D", "Greens/EFA", 
#'                           "ALDE", "EPP", "ECR", "EFD", "NA"),
#'                 Number = c(35, 184, 55, 84, 265, 54, 32, 27),
#'                 NumberPre = c(20, 166, 90, 40, 210, 130, 60, 20))
#' 
#' # dot-type
#' ggparliament(d, party = Party, seats1 = Number)
#' 
#' # arc-type
#' ggparliament(d, party = Party, seats1 = Number, style = "arc")
#' 
#' # arc-type with pre/post changes
#' ggparliament(d, party = Party, seats1 = Number, seats2 = NumberPre, style = "arc")
#' 
#' @import ggplot2
#' @export
ggparliament <- 
function(data, party, seats1, seats2,
         style = c("dots", "arc"), 
         portion = 0.75, 
         nrows = 10,
         ...) {

    if (!missing(data)) {
        party <- data$party <- data[[as.character(substitute(party))]]
        seats1 <- data$seats1 <- data[[as.character(substitute(seats1))]]
        if (!missing(seats2)) {
            seats2 <- data$seats2 <- data[[as.character(substitute(seats2))]]
        }
    } else {
        party <- party
        seats1 <- seats1
        if (length(party) != length(seats1)) {
            stop("'party' and 'seats1' must be vectors of the same length")
        }
        if (!missing(seats2)) {
            seats2 <- seats2
            if (length(party) != length(seats2)) {
                stop("'party', 'seats1', and 'seats2' must be vectors of the same length")
            }
        }
    }

    # remove superfluous theme elements
    simple_theme <- theme(axis.text.x = element_blank(), 
                          axis.text.y = element_blank(), 
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())

    
    # plot type
    style <- match.arg(style)
    if (style == "arc") {

        # post-election
        data$share <- portion * (seats1 / sum(seats1))
        data$xmax <- cumsum(data$share)
        data$xmin <- c(0, head(data$xmax, n= -1))
        data$xmid <- rowMeans(cbind(data$xmin, data$xmax))
        
        # pre-election
        if (!missing(seats2)) {
            data$share2 <- portion * (seats2 / sum(seats2))
            data$xmax2 <- cumsum(data$share2)
            data$xmin2 <- c(0, head(data$xmax2, n= -1))
            data$xmid2 <- rowMeans(cbind(data$xmin2, data$xmax2))
        }

        # setup plot
        p <- ggplot(data) + 
          coord_polar(theta = "x", start = -(portion*pi)) + 
          xlab("") + ylab("") +
          xlim(c(0, 1)) + ylim(c(0, 2.3))
        
        # post-election
        p <- p + 
          geom_text(aes(y = 2.2, x = xmid, label = seats1, colour = party)) +
          geom_rect(aes(fill = party, xmax = xmax, xmin = xmin, ymax = 2, ymin = 1.5), colour = NA)
          
        # pre-election
        if (!missing(seats2)) {
            p <- p +
              geom_text(aes(y = 1.2, x = xmid2, label = seats2, colour = party)) + 
              geom_rect(aes(fill = party, colour = party, xmax = xmax2, xmin = xmin2, ymax = 1, ymin = 0.5), colour = NA) 
        }
        
    } else if (style == "dots") {

        # construct new data structure
        polar <- structure(list(azimuth = numeric(sum(seats1)),
                                radius = numeric(sum(seats1))),
                           class = "data.frame",
                           row.names = seq_len(sum(seats1)))
        
        # seats per row is circumference of each ring
        circ <- floor(2*(portion*nrows)*pi)
        nperrow <- floor(seq(12, circ, length.out = nrows))
        
        # modify for based upon excess seats
        remainder <- sum(nperrow) - nrow(polar)
        i <- nrows
        while (remainder > 0) {
            ## NEED TO MODIFY THIS TO SUBTRACT MORE BASED UPON ROW
            nperrow[i] <- nperrow[i] - 1L
            remainder <- remainder - 1L
            if (i == 3) {
                i <- nrows
            } else {
                i <- i - 1L
            }
        }
        while (remainder < 0) {
            ## NEED TO MODIFY THIS TO ADD MORE BASED UPON ROW
            nperrow[i] <- nperrow[i] + 1L
            remainder <- remainder + 1L
            if (i == 3) {
                i <- nrows
            } else {
                i <- i - 1L
            }
        }
        
        # y position (which ring)
        ring <- rep(seq_len(nrows) + 3L, times = nperrow)
        polar[["radius"]] <- head(ring, nrow(polar))
        # x position within ring
        pos <- unlist(lapply(nperrow, function(x) seq(0, portion, length.out = x)))
        polar[["azimuth"]] <- head(pos, nrow(polar))
        
        polar <- polar[order(polar$azimuth, polar$radius),]
        polar[["party"]] <- rep(levels(party), times = seats1)
        
        # make plot
        p <- ggplot(polar, aes(x = azimuth, y = radius, colour = party)) + 
          geom_point(...) + 
          coord_polar(start = -(portion*pi)) + 
          xlab("") + ylab("") +
          xlim(c(0, 1)) + ylim(c(0, nrows + 3))
    }
    
    p + simple_theme
}
