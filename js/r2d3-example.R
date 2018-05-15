data("German_elections")

r2d3(data = German_elections[German_elections$year == 2017, ],
     script = system.file('js/d3plot.js', package = 'ggparliament'),
     dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'),
     d3_version = '4', viewer = 'browser')

# Issue 1 ####
# Problem : d3-parliament expects data in a particular format, e.g. party names specified by an "id" attribute. The script fails to read the party names properly using the example dataset. 
# See     : d3-parliament.js L116-L120 or example data (https://github.com/geoffreybr/d3-parliament/blob/master/example/french.json)
#           The problem is visible if you open developer tools in your browser and check the classes of any circle: <circle class='seat'>.
#           If the JS code is able to parse the party name, circles should be assigned to parties by class.

# Hacky solution: change the names of the data
dat <- German_elections[German_elections$year == 2017, ]
names(dat) <- c('year', 'legend', 'id', 'seats', 'colour', 'government')

r2d3(data = dat,
     script = system.file('js/d3plot.js', package = 'ggparliament'),
     dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'),
     d3_version = '4', viewer = 'browser')

# Issue 2 ####
# Problem: d3-parliament expects color information to be provided with CSS, I'm guessing due to performance considerations. This is why
#          you do not see colors, although the party names are read properly           
# See    : example/index.html L21-L41.

# Hacky solution: create CSS file, and populate based on the dataframe.
# (since this is quite some work, I added a file for specific German data)

r2d3(data = dat,
     script = system.file('js/d3plot.js', package = 'ggparliament'),
     dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'),
     css = system.file('js/german2017.css', package = 'ggparliament'),
     d3_version = '4', viewer = 'browser')

# Issue 3 ####
# Problem: d3-parliament orders the plots by the order in data           
# See    : Example below

# Hacky solution: create CSS file, and populate based on the dataframe.
# (since this is quite some work, I added a file for specific German data)

dat_ordered <- dat[order(dat$seats), ]
r2d3(data = dat_ordered,
     script = system.file('js/d3plot.js', package = 'ggparliament'),
     dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'),
     css = system.file('js/german2017.css', package = 'ggparliament'),
     d3_version = '4', viewer = 'browser')
