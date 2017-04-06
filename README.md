# Parliament plots

This package attempts to implement "parliament plots" - visual representations of the composition of legislatures that display seats color-coded by party. The input is a data frame containing one row per party, with columns representing party name/label and number of seats, respectively.

## Code Examples

Here are some examples for a "small" parliament:


```r
library("ggplot2")
library("ggparliament")

# example data (114th vs 115th US Senate)
d <- data.frame(Party = c("D", "R", "I"),
                Number = c(46, 52, 2),
                NumberPre = c(53, 45, 2))

# dot-style
cols1 <- scale_color_manual(values = c("D" = "Blue", "I" = "Gray", "R" = "Red"))
ggparliament(d, party = Party, seats1 = Number, 
             style = "dots", portion = 0.5, nrows = 6) + cols1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# arc-style
cols2 <- scale_fill_manual(values = c("D" = "Blue", "I" = "Gray", "R" = "Red"))
ggparliament(d, party = Party, seats1 = Number, style = "arc") + cols1 + cols2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
# double arc-style
ggparliament(d, party = Party, seats1 = Number, seats2 = NumberPre, 
             style = "arc") + cols1 + cols2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)



Here are some examples for a "large" parliament:


```r
# example
d <- data.frame(Party = factor(c("GUE/NGL", "S&D", "Greens/EFA", "ALDE", "EPP", "ECR", "EFD", "NA")),
                Number = c(35, 184, 55, 84, 265, 54, 32, 27),
                NumberPre = c(20, 166, 90, 40, 210, 130, 60, 20))

# dot-style
ggparliament(d, party = Party, seats1 = Number, style = "dots", nrows = 15)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
# arc-style
ggparliament(d, party = Party, seats1 = Number, style = "arc")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png)

```r
# double arc-style
ggparliament(d, party = Party, seats1 = Number, seats2 = NumberPre, style = "arc")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-3.png)

## Requirements and Installation

[![CRAN](http://www.r-pkg.org/badges/version/parliament)](https://cran.r-project.org/package=parliament)
[![Build Status](https://travis-ci.org/leeper/parliament.svg?branch=master)](https://travis-ci.org/leeper/parliament)
[![codecov.io](http://codecov.io/github/leeper/parliament/coverage.svg?branch=master)](http://codecov.io/github/leeper/parliament?branch=master)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

The development version of this package can be installed directly from GitHub using `ghit`:

```R
if (!require("ghit")) {
    install.packages("ghit")
    library("ghit")
}
install_github("leeper/ggparliament")
```
