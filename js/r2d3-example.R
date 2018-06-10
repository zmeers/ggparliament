data("German_elections")

dat <- German_elections[German_elections$year == 2017, ]
names(dat) <- c('year', 'legend', 'id', 'seats', 'colour', 'government')

#' d3-parliament now makes use of the colour column:
#' Temporary note: At the moment this is not merged into main repository, but soon will be...
#'                 https://github.com/geoffreybr/d3-parliament/pull/3

r2d3(data = dat,
     script = 'js/d3plot.js',
     dependencies = 'js/d3-parliament.js',
     d3_version = '4', viewer = 'browser')

dat$colour[1] <- 'gray'
r2d3(data = dat,
     script = 'js/d3plot.js',
     dependencies = 'js/d3-parliament.js',
     d3_version = '4', viewer = 'browser')

# You can still use CSS after a litle adjustment in d3plot.js.
# If you'd like to try this, first open d3plot.js, then call r2d3:

# r2d3(data = dat,
#      script = 'js/d3plot.js',
#      dependencies = 'js/d3-parliament.js',
#      css = 'js/german2017.css',
#      d3_version = '4', viewer = 'browser')