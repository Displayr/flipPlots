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
    expect_that(WorldMap(valid.continent.names, type = "continent"), not(throws_error()))
    rm(valid.continent.names)
    invalid.continent.names <- 1:6
    names(invalid.continent.names) <- c("Asia", "Africag", "Europe", "South America", "Oceania", "North America")
    expect_error(WorldMap(invalid.continent.names, type = "continent"))
    invalid.country.names <- matrix(1:2, 2,dimnames =list(c("Australia", "New Zealands"), "A"))
    expect_error(WorldMap(invalid.country.names))
    rm(invalid.country.names)
})
