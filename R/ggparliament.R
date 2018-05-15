#' @title Parliament Plots
#' @description Produce a Wikipedia-style hemicycle plot of parliamentary composition
#' @param data A data frame
#' @param party The name of a variable in \code{data} containing the names of parties. If \code{data} is missing, this can be a vector of party names.
#' @param seats1 The name of a variable in \code{data} containing the number of seats for each party. If \code{data} is missing, this can be a vector of seat counts.
#' @param seats2 Optionally, the name of a variable in \code{data} containing a second set of numbers of seats for each party. If \code{data} is missing, this can be a vector of seat counts. This can be useful for showing, e.g., pre-/post-election changes in numbers of seats.
#' @param style A character string specifying the style of the graph. Either \dQuote{arc} or \dQuote{dots}.
#' @param label A character string specifying the type of label to place next to each party group.
#' @param portion A numeric value specifying what proportion of a full circle should be used for drawing the plot.
#' @param nrows If \code{style = "dots"}, a numeric value indicating how many rows to use.
#' @param size A numeric value specifying the size of dots if \code{style = "dots"}.
#' @param total A numeric value indicating the font size of a text label indicating the number of seats in the parliament. If \code{NULL} (the default), this is omitted.
#' @param \dots Ignored
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
#' @importFrom stats aggregate
#' @importFrom utils head
#' @export
ggparliament <-
function(data, party, seats1, seats2,
         style = c("dots", "arc", "bar", "pie", "rose"),
         label = c("name", "seats", "both", "neither"),
         portion = 0.75,
         nrows = 10,
         size = 2L,
         total = NULL,
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


    # labels
    label <- match.arg(label)
    labeltext1 <- switch(label,
      name = party,
      seats = as.character(seats1),
      both = paste0(party, " (", seats1, ")"),
      neither = rep("", length(party))
    )
    if (!missing(seats2)) {
        labeltext2 <- switch(label,
          name = party,
          seats = as.character(seats2),
          both = paste0(party, " (", seats2, ")"),
          neither = rep("", length(party))
        )
    }

    # plot type
    style <- match.arg(style)
    if (style %in% c("arc", "bar")) {

        if (style == "bar") {
            portion <- 1L
        }

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
        p <- ggplot(data) + xlab("") + ylab("") + xlim(c(0, 1)) + ylim(c(0, 2.3))
        if (style == "arc") {
            p <- p + coord_polar(theta = "x", start = -(portion*pi))
        }

        # post-election
        if (label != "neither") {
            p <- p + geom_text(aes(y = 2.2, x = xmid, label = labeltext1, color = party))
        }
        p <- p + geom_rect(aes(fill = party, xmax = xmax, xmin = xmin, ymax = 2, ymin = 1.5), colour = NA)

        # pre-election
        if (!missing(seats2)) {
            if (label != "neither") {
                p <- p + geom_text(aes(y = 1.2, x = xmid2, label = labeltext2, colour = party))
            }
            p <- p + geom_rect(aes(fill = party, colour = party, xmax = xmax2, xmin = xmin2, ymax = 1, ymin = 0.5), colour = NA)
        }
        if (!is.null(total)) {
            p <- p + geom_text(aes(x = 0.25, y = 0.2), label = sum(seats1), size = total, color = "black")
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
          coord_polar(start = -(portion*pi)) +
          xlab("") + ylab("") +
          xlim(c(0, 1)) + ylim(c(0, nrows + 8))

        if (label != "neither") {
            labeldata <- aggregate(azimuth ~ party, data = polar, FUN = mean, na.rm = TRUE)
            labeldata[["labeltext"]] <- labeltext1
            labeldata[["y"]] <- nrows
            p <- p + geom_text(aes(x = azimuth, y = y, label = labeltext), data = labeldata, nudge_y = 6, inherit.aes = FALSE)
        }

        if (!is.null(total)) {
            p <- p + geom_text(aes(x = 0.25, y = 0.2), label = sum(seats1), size = total, color = "black")
        }

        p <- p + geom_point(size = size)

    } else if (style == "pie") {
        stop("style 'pie' not currently implemented")
    } else if (style == "rose") {
        p <- ggplot(, aes(x = factor(party), y = seats1, fill = factor(party))) +
          xlab("") + ylab("") +
          geom_bar(width = 1, stat = "identity") +
          coord_polar(theta = "x")
    }

    p + simple_theme
}
