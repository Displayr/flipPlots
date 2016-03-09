library(flipPlots)
library(testthat)
context("Scatterplot matrix")


test_that("Scatterplot matrix",
{
    x <- 1:100 + rnorm(100)
    y <- 1:100 + rnorm(100)
    filter <- sample(c(TRUE, FALSE), 100, replace = TRUE)
    weights <- sample.int(10, 100, replace = TRUE)

    expect_error(ScatterplotMatrix(x), "You need at least 2 columns to display a scatterplot matrix.")
    expect_that(ScatterplotMatrix(x, y), not(throws_error()))
    expect_that(ScatterplotMatrix(x, y, .subset = filter), not(throws_error()))
    expect_that(ScatterplotMatrix(x, y, .weights = weights), not(throws_error()))
    expect_that(ScatterplotMatrix(x, y, .subset = filter, .weights = weights), not(throws_error()))
})
