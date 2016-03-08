library(flipPlots)
library(testthat)
context("Scatterplot matrix")


test_that("Scatterplot matrix",
{
    x <- 1:100 + rnorm(100)
    y <- 1:100 + rnorm(100)
    expect_error(ScatterplotMatrix(x), "You need at least 2 columns to display a scatterplot matrix.")
    expect_that(ScatterplotMatrix(x, y), not(throws_error()))
})
