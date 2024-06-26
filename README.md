

<img src = "man/figures/HexSticker.png" align = "right" width = "200"/>

# `ggparliament`: Parliament plots


![CRAN Status](https://www.r-pkg.org/badges/version/ggparliament)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggparliament)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.01313/status.svg)](https://doi.org/10.21105/joss.01313)

This package attempts to implement "parliament plots" - visual representations of the composition of legislatures that display seats colour-coded by party. The input is a data frame containing one row per party, with columns representing party name/label and number of seats, respectively.

This `R` package is a `ggplot2` extension and is now on CRAN. Please install the stable version in `R` by running:


```r
install.packages("ggparliament")
```

To install the package from source:

```r
devtools::install_github("zmeers/ggparliament")
```

Inspiration from this package comes from: [parliamentdiagram](https://github.com/slashme/parliamentdiagram), which
is used on Wikipedia, [parliament-svg](https://github.com/juliuste/parliament-svg), which is a javascript clone, and [a discussion on StackOverflow](https://stackoverflow.com/questions/42729174/creating-a-half-donut-or-parliamentary-seating-chart), which provided some of the code for part for the "arc" representations used in this package.


If you have any issues, please note the problem and inform us!


## Election data

`ggparliament` provides election data from the following countries. 


```r
election_data %>% 
  distinct(year, country, house) %>% 
  arrange(country, year)
```

```
## # A tibble: 39 x 3
##    country    year house          
##    <chr>     <dbl> <chr>          
##  1 Australia  2010 Representatives
##  2 Australia  2010 Senate         
##  3 Australia  2013 Representatives
##  4 Australia  2013 Senate         
##  5 Australia  2016 Representatives
##  6 Australia  2016 Senate         
##  7 Australia  2019 Representatives
##  8 Australia  2019 Senate         
##  9 Chile      2009 Diputados      
## 10 Chile      2009 Senadores      
## # … with 29 more rows
```

We also provide the following vignettes for further explanation:

1. Basic parliament plots
2. Labelling parties
3. Drawing the majority threshold line
4. Highlighting parties in power
5. Faceting legislatures
6. Emphasizing certain seats
7. Visualizaing overhang seats in MMP electoral systems
8. Arranging seat order in ggparliament plots.

Quick `ggparliament` examples can be viewed below.

## Semicircle parliament

### EU, France, United States, and so on...


### Plot of US House of Representatives



```r
#filter the election data for the most recent US House of Representatives
us_house <- election_data %>%
  filter(country == "USA" &
    year == 2016 &
    house == "Representatives")

us_house <- parliament_data(election_data = us_house,
  type = "semicircle",
  parl_rows = 10,
  party_seats = us_house$seats)

us_senate <- election_data %>%
  filter(country == "USA" &
    year == 2016 &
    house == "Senate")

us_senate <- parliament_data(
  election_data = us_senate,
  type = "semicircle",
  parl_rows = 4,
  party_seats = us_senate$seats)
```


```r
representatives <- ggplot(us_house, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  #highlight the party in control of the House with a black line
  geom_highlight_government(government == 1) +
  #draw majority threshold
  draw_majoritythreshold(n = 218, label = TRUE, type = 'semicircle')+
  #set theme_ggparliament
  theme_ggparliament() +
  #other aesthetics
  labs(colour = NULL, 
       title = "United States House of Representatives",
       subtitle = "Party that controls the House highlighted.") +
  scale_colour_manual(values = us_house$colour, 
                      limits = us_house$party_short) 

representatives
```

![US House of Representatives](docs/figure/unnamed-chunk-6-1.png)

### Plot of US Senate


```r
senate <- ggplot(us_senate, aes(x, y, colour = party_long)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) +
  # add bar showing proportion of seats by party in legislature
  geom_parliament_bar(colour = colour, party = party_long) + 
  theme_ggparliament(legend = FALSE) +
  labs(colour = NULL, 
       title = "United States Senate",
       subtitle = "The party that has control of the Senate is encircled in black.") +
  scale_colour_manual(values = us_senate$colour,
                      limits = us_senate$party_long)
senate 
```

![US Senate](docs/figure/unnamed-chunk-7-1.png)


### Plot of German Bundestag


```r
germany <- election_data %>%
  filter(year == 2017 & 
           country == "Germany") 

germany <- parliament_data(election_data = germany, 
                           parl_rows = 10,
                           type = 'semicircle',
                           party_seats = germany$seats)

bundestag <- ggplot(germany, aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 3) +
  labs(colour="Party") +  
  theme_ggparliament(legend = TRUE) +
  scale_colour_manual(values = germany$colour, 
                      limits = germany$party_short) 

bundestag
```

![German Bundestag](docs/figure/unnamed-chunk-8-1.png)

## Opposing Benches Parliament



### United Kingdom


```r
#data preparation
uk_17 <- election_data %>% 
  filter(country == "UK" & 
           year == "2017") %>% 
  parliament_data(election_data = .,
                  party_seats = .$seats,
                  parl_rows = 12,
                  type = "opposing_benches",
                  group = .$government)


commons <- ggplot(uk_17, aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 3) + 
  theme_ggparliament() + 
  coord_flip() + 
  labs(colour = NULL, 
       title = "UK parliament in 2017") +
  scale_colour_manual(values = uk_17$colour, 
                      limits = uk_17$party_short)

commons
```

![UK Parliament](docs/figure/unnamed-chunk-9-1.png)



## Horseshoe parliament

### Australia, New Zealand


```r
australia <- election_data %>%
  filter(country == "Australia" &
    house == "Representatives" &
    year == 2016) %>% 
  parliament_data(election_data = .,
    party_seats = .$seats,
    parl_rows = 4,
    type = "horseshoe")
```

### Plot of Australian parliament


```r
au_rep <-ggplot(australia, aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 3.5) + 
  geom_highlight_government(government == 1, colour = "pink", size = 4) + 
  draw_majoritythreshold(n = 76, 
                         label = TRUE, 
                         linesize = 0.5,
                         type = 'horseshoe') + 
  theme_ggparliament() +
  theme(legend.position = 'bottom') + 
  labs(colour = NULL,
       title = "Australian Parliament",
       subtitle = "Government circled in pink.") +
  scale_colour_manual(values = australia$colour, 
                      limits = australia$party_short) 

au_rep
```

![Australian Parliament](docs/figure/unnamed-chunk-11-1.png)





