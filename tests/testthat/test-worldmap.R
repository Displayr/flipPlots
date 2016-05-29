library(flipPlots)
library(testthat)
context("World Map")



test_that("Checking inputs",
{
    invalid.continent.names <- 1:6
    names(invalid.continent.names) <- c("Asia", "Africa", "Europe", "South America", "Oceania", "North America")
    expect_error(WorldMap(invalid.continent.names))
    rm(invalid.continent.names)
    expect_error(WorldMap(1:10))
    expect_error(WorldMap(matrix(1, 2, 2)))
    expect_error(WorldMap(matrix(1, 2, 2, dimnames = list(LETTERS[1:2], NULL))))
    expect_error(WorldMap(matrix(1, 2, 2, dimnames = list(NULL, LETTERS[1:2]))))
    expect_error(WorldMap(array(1, dim = c(2,2,2),
        dimnames = list(LETTERS[1:2], LETTERS[1:2], LETTERS[1:2]))))
})


test_that("Checking accuracy of row names",
{
    valid.continent.names <- 1:6
    names(valid.continent.names) <- c("Asia", "Africa", "Europe", "South America", "Oceania", "North America")
    expect_error(WorldMap(valid.continent.names, type = "continent"), NA)
    rm(valid.continent.names)
    invalid.continent.names <- 1:6
    names(invalid.continent.names) <- c("Asia", "Africag", "Europe", "South America", "Oceania", "North America")
    expect_error(WorldMap(invalid.continent.names, type = "continent"))
    invalid.country.names <- matrix(1:2, 2,dimnames =list(c("Australia", "New Zealands"), "A"))
    expect_error(WorldMap(invalid.country.names))
    rm(invalid.country.names)
})

test_that("Checking accuracy of country names",
{
    # Should use low resolution map - I don't know how to test this
    valid.names <- 1:6
    names(valid.names) <- c("Australia", "New Zealand", "USA", "Canada", "United Kingdom", "Japan")
    expect_error(WorldMap(valid.names, type = "name"), NA)

    # Should use high resolution map
    valid.names <- 1:7
    names(valid.names) <- c("Australia", "New Zealand", "USA", "Canada", "United Kingdom", "Japan", "Singapore")
    expect_error(WorldMap(valid.names, type = "name"),NA)
})
