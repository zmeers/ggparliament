## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggparliament)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
source("../R/parliament_data.R")
source("../R/geom_parliament_seats.R")
source("../R/geom_highlight_government.R")
source("../R/helper_funcs.R")
source("../R/draw_majoritythreshold.R")
source("../R/draw_partylabels.R")
source("../R/draw_majoritythreshold.R")
source("../R/draw_totalseats.R")
source("../R/theme_ggparliament.R")
load("../R/sysdata.rda")

## ------------------------------------------------------------------------
us_rep <- election_data %>%
  filter(country == "USA" &
    year == 2016 &
    house == "Representatives")
head(us_rep)

## ------------------------------------------------------------------------
us_house_semicircle <- parliament_data(election_data = us_rep,
  type = "semicircle",
  parl_rows = 10,
  party_seats = us_rep$seats)
head(us_house_semicircle)

## ---- fig.width=6, fig.height=4------------------------------------------
us <- ggplot(us_house_semicircle, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(colour = NULL, 
       title = "United States Congress") +
  scale_colour_manual(values = us_house_semicircle$colour, 
                      limits = us_house_semicircle$party_short) 

us

## ------------------------------------------------------------------------
australia <- election_data %>%
  filter(country == "Australia" &
    house == "Representatives" &
    year == 2016) 

australia_horseshoe <- parliament_data(election_data = australia,
  party_seats = australia$seats,
  parl_rows = 4,
  type = "horseshoe")

## ----fig.width=4, fig.height=4-------------------------------------------
au <- ggplot(australia_horseshoe, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(colour = NULL, 
       title = "Australian Parliament") +
  scale_colour_manual(values = australia$colour, 
                      limits = australia$party_short) + 
  theme(legend.position = 'bottom')

au

## ---- fig.height=6, fig.width = 6----------------------------------------
uk_17 <- election_data %>% 
  filter(country == "UK" & 
           year == "2017")


uk_17 <- parliament_data(election_data = uk_17,
  party_seats = uk_17$seats,
  parl_rows = 12,
  group = uk_17$government,
  type = "opposing_benches")

uk <- ggplot(uk_17, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(colour = NULL) +
  scale_colour_manual(values = uk_17$colour, 
                      limits = uk_17$party_short) +
  theme(legend.position = 'right')
uk

## ------------------------------------------------------------------------
russia_classroom <- election_data %>%
  filter(country == "Russia" &
    house == "Duma" &
    year == 2016) %>% 
  parliament_data(election_data = .,
                  party_seats = .$seats,
                  parl_rows = 11,
                  type = "classroom")

## ----fig.width=4, fig.height=4-------------------------------------------
rus <- ggplot(russia_classroom, aes(x, y, colour = party_short)) +
  geom_parliament_seats() +
  theme_ggparliament() +
  labs(
    colour = NULL,
    title = "Russian Duma") +
  scale_colour_manual(
    values = russia_classroom$colour,
    limits = russia_classroom$party_short) +
  theme(legend.position = "bottom")

rus

## ------------------------------------------------------------------------
russia_circle <- election_data %>%
  filter(country == "Russia" &
    house == "Duma" &
    year == 2016) %>% 
  parliament_data(election_data = .,
    party_seats = .$seats,
    parl_rows = 11,
    type = "circle")

## ----fig.width=4, fig.height=4-------------------------------------------
example <- ggplot(russia_circle, aes(x, y), colour = "black") +
  geom_parliament_seats() +
  theme_ggparliament() +
  labs(colour = NULL) +
  theme(legend.position = "bottom")

example

