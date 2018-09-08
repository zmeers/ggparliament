## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


source("../R/parliament_data.R")
source("../R/geom_parliament_seats.R")
source("../R/geom_highlight_government.R")
source("../R/geom_overhang_seats.R")
source("../R/helper_funcs.R")
source("../R/draw_majoritythreshold.R")
source("../R/draw_partylabels.R")
source("../R/draw_majoritythreshold.R")
source("../R/draw_totalseats.R")
source("../R/theme_ggparliament.R")
load("../R/sysdata.rda")
library(ggparliament)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)


