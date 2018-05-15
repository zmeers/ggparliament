#' @title Parliament Plots
#' @description Produce a Wikipedia-style hemicycle plot of parliamentary composition
#' @param data A data frame
#' @param party The name of a variable in \code{data} containing the names of party. If \code{data} is missing, this can be a vector of party names.
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
#' d <- data.frame(party = c("GUE/NGL", "S&D", "Greens/EFA",
#'                           "ALDE", "EPP", "ECR", "EFD", "NA"),
#'                 Number = c(35, 184, 55, 84, 265, 54, 32, 27),
#'                 NumberPre = c(20, 166, 90, 40, 210, 130, 60, 20))
#'
#' # dot-type
#' ggparliament(d, party = party, seats1 = Number)
#'
#' # arc-type
#' ggparliament(d, party = party, seats1 = Number, style = "arc")
#'
#' # arc-type with pre/post changes
#' ggparliament(d, party = party, seats1 = Number, seats2 = NumberPre, style = "arc")
#'
#' @import ggplot2
#' @importFrom stats aggregate
#' @importFrom utils head
#' @export
ggparl_dots <-
  function(data, party, seats1, seats2,
           style = c("dots", "arc", "bar", "pie", "rose"),
           label = c("name", "seats", "both", "neither"),
           portion = 0.75,
           nrows = 10,
           size = 2L,
           total = NULL,
           government = NULL,
           group = NULL,
           colour = NULL,
           segment = NULL,
           autoscale = NULL,
           ...) {

    #old stuff, set up the data
    if (!missing(data)) {
      seats1 <- data$seats1 <- data[[as.character(seats1)]]
      #remove any rows that have zero seats
      data <- data[which(seats1 > 0),]
      seats1 <- seats1[which(seats1 > 0)]

      #order the data
      data <- data[order(-data$seats1),]

      party <- data$party <- data[[as.character(party)]]

      if (!missing(seats2)) {
        seats2 <- data$seats2 <- data[[as.character(seats2)]]
      }


      #should the data be grouped
      #i.e. for facetting
      if (!is.null(group)){
        groups <- data$group <- data[[as.character(group)]]
      } else {
        groups <- data$group <- rep(1, length(data$seats1))
      }

      #should the government be taken into account
      #for ordering and also for colouring labels
      #some string compared to NA for non-government party
      if (!is.null(government)){
        governments <- data$government <- data[[as.character(government)]]
      } else {
        governments <- data$government <- rep(1, length(data$seats1))
      }

      #find it more intuitive to convert to long form earlier
      long_df <- data.frame(party = rep(party, data$seats1),
                            group = rep(groups, data$seats1),
                            government = rep(governments, data$seats1))
      #and order for plotting
      long_df <- long_df[order(long_df$group, long_df$government),]
      long_df$seats <- as.numeric(ave(as.character(long_df$party), as.character(long_df$party), long_df$group, FUN = length))
    } else {
      stop("rewrite doesn't yet support passing in data as vectors- sorry!")
    }

    if(!is.null(autoscale)){
      warning("autoscaling size of plots")
      scaling_df <- data.frame(table(long_df$group))
      names(scaling_df) <- c("group", "freq")
      #adjust the point size by group based on the nubmer of points
      size_scalar <- min(scaling_df$freq)
      scaling_df$size <- size * (log(size_scalar) / log(scaling_df$freq))

      #work out a fair number of rows for the groups
      scaling_df$nrows <- round(sqrt(scaling_df$freq) / 2)
      long_df <- merge(long_df, scaling_df, by = "group")
    }

    #create the polar df and merge the long data into it
    if(is.null(autoscale)){
      polar <- do.call("rbind", lapply(unique(long_df$group), arrange_dot_layout, portion = portion, nrows = nrows, df = long_df))
      polar <- cbind(polar, long_df)
      polar$nrows <- nrows
    } else {
      polar <- mapply(arrange_dot_layout, group = unique(long_df$group), nrows = long_df$nrows[match(unique(long_df$group), long_df$group)],
                      MoreArgs = list(portion = portion, df = long_df))
      polar <- data.frame(azimuth = unlist(polar[1,]),
                          radius = unlist(polar[2,]))
      polar <- cbind(polar, long_df)
    }

    #make base plot
    p <- ggplot(polar, aes(x = azimuth, y = radius, colour = party)) +
      coord_polar(start = -(portion*pi)) +
      xlab("") + ylab("") +
      xlim(c(0, 1)) + ylim(c(0, max(polar$radius) + 5))

    #facet by grouping
    if (!is.null(group)){
      p <- p + facet_wrap(~group)
    }

    #add in the points
    if(!is.null(autoscale)){
      p <- p + geom_point(size = polar$size)
    } else {
      p <- p + geom_point(size = size)
    }

    #add a line in to show the cutoff (e.g. 50% for majority)
    if (!is.null(segment)) {
      segment_df <- data.frame(group = unique(polar$group))
      radii <- aggregate(radius ~ group, data = polar, FUN = function(x) c(min = min(x), max = max(x)))
      segment_df$x_pos = (max(polar$azimuth) - min(polar$azimuth)) * segment
      segment_df$y_min = radii$radius[,1] - 1
      segment_df$y_max = (radii$radius[,2] + 3)
      p <- p + geom_segment(data = segment_df, aes(x = x_pos, xend = x_pos, y = y_min, yend = y_max),
                            linetype = "dashed", size = 1, colour = "black")
    }

    #add labels to the plot
    if (label != "neither") {
      #add back in more funcs

      labeldata <- aggregate(azimuth ~ party + group + seats, data = polar, FUN = mean, na.rm = TRUE)
      labeldata <- labeldata[order(labeldata$group, labeldata$azimuth),]

      label <- match.arg(label)
      labeltext1 <- switch(label,
                           name = labeldata$party,
                           seats = as.character(labeldata$seats),
                           both = paste0(labeldata$party, " (", labeldata$seats, ")"),
                           neither = rep("", length(labeldata$party))
      )

      #colour labels by if party is in power or not
      if (!is.null(government)){
        labeldata$government <- long_df$government[match(unique(paste(long_df$party, long_df$group)),
                                                         paste(long_df$party, long_df$group))]
      } else {
        #placeholder
        labeldata$government <- 1
      }
      labeldata$colour <- ifelse(is.na(labeldata$government), "grey40", "black")
      #get the nubmer of rows by group
      labeldata$y <- rep(as.numeric(aggregate(nrows ~ group, data = polar, FUN = max)$nrows), as.numeric(table(labeldata$group)))
      #make sure that even with nudge is within max radius
      labeldata$y <- ifelse(labeldata$y > max(polar$radius) - 7 + 5, max(polar$radius) - 7 + 5, labeldata$y)
      labeldata[["labeltext"]] <- labeltext1

      #add to the plot
      p <- p + geom_text(data = labeldata, aes(x = azimuth, y = y, label = labeltext), colour = labeldata$colour, nudge_y = 7)
    }

    #add a label for the total number of seats
    if (!is.null(total)) {
      if (is.null(group)){
        p <- p + geom_text(aes(x = 0.25, y = 0.2), label = sum(seats1), size = total, color = "black")
      } else {
        #might not work properly
        grouped_totals <- data.frame(table(long_df$group))
        names(grouped_totals) <- c("group", "sum")
        grouped_totals$group <- as.character(grouped_totals$group)
        p <- p + geom_text(data = grouped_totals, aes(x = 0.25, y = 0.2, label = sum), size = total, color = "black")
      }
    }

    #add in a colour scale from the data
    #coloumn in the data for the colour for each party
    #I find it easiest to create a separate df of party/colours and merge in
    if (!is.null(colour)) {
      party_levels <- factor(unique(party), levels = unique(party[c(1:nrow(data))]), ordered = T)
      colour <- data$colour[match(party_levels, data$party)]
      col_levels <- factor(colour, levels = colour[order(levels(party_levels))], ordered = T)
      colScale <- scale_colour_manual(name = "Elected party", values = levels(col_levels))
      p <- p + colScale
    }

    # remove superfluous theme elements
    simple_theme <- theme(axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())

    p <- p + simple_theme

    return(p)
  }






