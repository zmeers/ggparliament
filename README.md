``` r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
source("parliament_data.R")
load("election_data.rda")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
    ## ✔ tibble  1.4.2          ✔ dplyr   0.7.5     
    ## ✔ tidyr   0.8.1          ✔ stringr 1.3.1     
    ## ✔ readr   1.1.1          ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Parliament plots
================

This package attempts to implement “parliament plots” - visual
representations of the composition of legislatures that display seats
color-coded by party. The input is a data frame containing one row per
party, with columns representing party name/label and number of seats,
respectively.

Inspiration from this package comes from:
[parliamentdiagram](https://github.com/slashme/parliamentdiagram), which
is used on Wikipedia,
[parliament-svg](https://github.com/juliuste/parliament-svg), which is a
javascript clone, and [a discussion on
StackOverflow](http://stackoverflow.com/questions/42729174/creating-a-half-donut-or-parliamentary-seating-chart),
which provided some of the code for part for the “arc” representations
used in this package.

Unique parliament layouts:
==========================

Monkey Cage article :
<https://www.washingtonpost.com/news/monkey-cage/wp/2017/03/04/these-5-designs-influence-every-legislature-in-the-world-and-tell-you-how-each-governs/?utm_term=.e1e1c1c3c37b>

Opposing benches parliament
---------------------------

### United Kingdom, Canada

#### Data

``` r
ukresults <- election_data %>%
  filter(country == "UK" &
           year == 2017)
ukresults$seats <- as.numeric(ukresults$seats)
left_parties <- c("Labour",
                  "Scottish National Party",
                  "Liberal Democrats",
                  "Green",
                  "Plaid Cymru",
                  "Ind")
right_parties <- c("Conservative",
                   "Democratic Unionist")
ukresults$location <-
  ifelse(ukresults$party_long %in% left_parties, "left",
    ifelse(ukresults$party_long %in% right_parties, "right",
      NA
    )
  )

ukresults_left <- ukresults %>%
  filter(location == "left")

ukresults_right <- ukresults %>%
  filter(location == "right")
```

#### Plot (using the old version of `geom_parliament_*()`)

``` r
leftside <- ggplot()+
  geom_parliament_waffle(
  type = "opposing_benches",
  seatspp = ukresults_left$seats,
  party_names = ukresults_left$party_long,
  parlrows = 10) +
  theme_parliament() +
  theme(legend.position = "left") +
  labs(fill = "", title = "Opposing benches parliament: Westminster") +
  scale_fill_manual(
    values = ukresults_left$colour,
    labels = ukresults_left$party_long,
    limits = ukresults_left$party_long)


rightside <- ggplot() +
  geom_parliament_waffle(
  type = "opposing_benches",
  seatspp = ukresults_right$seats,
  party_names = ukresults_right$party_long,
  parlrows = 10) + 
  theme_parliament() +
  labs(fill = "") +
  scale_fill_manual(
    values = ukresults_right$colour,
    labels = ukresults_right$party_long,
    limits = ukresults_right$party_long)


uk_parliament <- combine_opposingbenches(left = leftside, right = rightside)
uk_parliament
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-2-1.png)

Semicircle parliament
---------------------

### EU, France, United States, and so on…

#### Data

``` r
us_congress <- election_data %>%
  filter(country == "USA" &
    year == "2016" &
    house == "Representatives")
us_congress1 <- parliament_data(electiondata = us_congress,
  type = "semicircle",
  totalseats = sum(us_congress$seats),
  parlrows = 10,
  party_names = us_congress$party_short,
  seatspp = us_congress$seats)
us_senate <- election_data %>%
  filter(country == "USA" &
    year == "2016" &
    house == "Senate")
us_senate <- parliament_data(
  electiondata = us_senate,
  type = "semicircle",
  totalseats = sum(us_senate$seats),
  parlrows = 4,
  party_names = us_senate$party_short,
  seatspp = us_senate$seats)
```

#### Plot

``` r
ggplot(us_congress1, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) +
  theme_void() +
  labs(colour = "", title = "United States Congress") +
  annotate("text", x=0, y=0, label=paste("Total:", sum(us_congress$seats), "Reps"), fontface="bold", size=8) +
  scale_colour_manual(values = us_congress1$colour, limits = us_congress1$party_short)
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
senate <- ggplot(us_senate, aes(x=x, y=y, colour=party_long)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) +
  theme_parliament() +
  labs(colour = "", 
       title = "United States Senate",
       subtitle = "Government encircled in black.") +
  scale_colour_manual(values = us_senate$colour, limits=us_senate$party_long)
senate 
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
germany <- election_data %>%
  filter(year==2017 & country=="Germany")
#View(germany)
germany <- parliament_data(electiondata=germany, totalseats = sum(germany$seats), parlrows=10,seatspp=germany$seats, type='semicircle')

ggplot(germany, aes(x,y,colour=party_short))+
  geom_parliament_seats()+
  #geom_highlight_government(government==1) + 
  labs(colour="Party", title="Germany 2017 Election Results") + 
  theme_parliament()+
  scale_colour_manual(values = germany$colour, limits=germany$party_short)
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-6-1.png)

Horseshoe parliament
--------------------

### Australia, New Zealand

#### Data

``` r
australia <- election_data %>%
  filter(year == 2016 &
    country == "Australia" &
    house == "Representatives")
australia <- australia[c(1, 5, 6, 7, 4, 3, 2), ]

aus <- parliament_data(electiondata = australia,
  totalseats = sum(australia$seats),
  seatspp = australia$seats,
  parlrows = 4,
  type = "horseshoe")
```

#### Plot

``` r
ggplot(aus, aes(x, y, colour=party_long)) +
  geom_parliament_seats() + 
  theme_parliament() +
  geom_highlight_government(government == 1) + 
  labs(colour = "", title = "Australia House of Representatives",
    subtitle = "Government encircled in black.") +
  annotate("text", x=0, y=0, label=paste("Total:", sum(australia$seats), "MPs"), fontface="bold", size=12) +
  scale_colour_manual(values = aus$colour, limits = aus$party_long)
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-8-1.png)

Circle parliament
-----------------

### old German Bundestag (find more modern examples).

If we can’t find a good example, is it worth including?

Classroom parliament
--------------------

### China, Russia, North Korea

#### Data

``` r
russia <- election_data %>%
  filter(country == "Russia",
    year == "2016")
russia <- parliament_data(
  electiondata = russia,
  type = "classroom",
  seatspp = russia$seats,
  party_names = russia$party_short,
  parlrows = 10)
```

#### Plot

``` r
ggplot(russia, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_parliament() +
  #geom_highlight_government(government==1) + 
  scale_colour_manual(values = c(russia$colour),
    labels = c(russia$party_short),
    limits = c(russia$party_short)) +
  labs(colour = "", title = "Russian Parliament") 
```

![](parliament_dotplots_example_files/figure-markdown_github/unnamed-chunk-10-1.png)
