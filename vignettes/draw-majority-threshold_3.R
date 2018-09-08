## ----setup, include=FALSE------------------------------------------------
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



## ---- fig.height = 4, fig.width=6----------------------------------------
usa_12 <- election_data %>%
  filter(country == "USA" &
           house == "Representatives" & 
           year == "2012")  %>% 
  parliament_data(election_data = .,
  party_seats = .$seats,
  parl_rows = 8,
  type = "semicircle")

ggplot(usa_12, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) + 
  draw_majoritythreshold(n = 218,
                         type = "semicircle") + 
  theme_ggparliament() +
  labs(colour = NULL, 
       title = "2012 American Congress",
       subtitle = "Party that controls the chamber is highlighted in black.") +
  scale_colour_manual(values = usa_12$colour, 
                      limits = usa_12$party_short) 


## ---- fig.height = 4, fig.width=4----------------------------------------
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

## ----fig.width=4, fig.height=4-------------------------------------------
australia <- election_data %>%
  filter(country == "Australia" &
    house == "Representatives" &
    year == 2016) 

australia1 <- parliament_data(election_data = australia,
  party_seats = australia$seats,
  parl_rows = 4,
  type = "horseshoe")

au <-ggplot(australia1, aes(x, y, colour = party_short)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) + 
  draw_majoritythreshold(n = 76,
                         type = "horseshoe") + 
  theme_ggparliament() +
  labs(colour = NULL, 
       title = "Australian Parliament",
       subtitle = "Government encircled in black.") +
  scale_colour_manual(values = australia$colour, 
                      limits = australia$party_short) + 
  theme(legend.position = 'bottom') 
au

## ---- fig.height=4, fig.width=8------------------------------------------
germany <- election_data %>%
  filter(year == 2017 & 
           country == "Germany") 

germany <- parliament_data(election_data = germany, 
                           parl_rows = 12,
                           party_seats = germany$seats, 
                           type = 'semicircle')

german_parliament <- ggplot(germany, aes(x, y, colour = party_short)) +
  geom_parliament_seats() +
  geom_highlight_government(government == 1) + 
  draw_majoritythreshold(n = 355, label = FALSE, type = "semicircle") + 
  labs(colour="Party", 
       title="Germany 2017 Election Results") + 
  theme_ggparliament() +
  scale_colour_manual(values = germany$colour, 
                      limits = germany$party_short)
german_parliament

