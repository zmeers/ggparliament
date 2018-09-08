library(ggparliament)
library(testthat)
library(ggplot2)
library(dplyr)

test_data <- readRDS("../testdata/usa_data.rda")
result <- parliament_data(election_data = test_data, parl_rows = 8, party_seats = test_data$seats, type = "semicircle")


test_that("Check that the number of expanded observations is identical to the sum of the seats", {
  expect_equal(sum(test_data$seats), nrow(result))
})

test_that("Check that the resulting output contains 12 headers", {
  expect_equal(colnames(result), c("year","country","house","party_long","party_short","seats","government","colour","x","y","row","theta"))
})


