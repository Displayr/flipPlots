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


test_that("Australian State maps with automatic renaming",
{
    states <- 1:8
    # State and country names as they are in the source data
    names(states) <- c("New South Wales", "Victoria", "Queensland", "South Australia",
        "Tasmania", "Northern Territory", "Western Australia", "Australian Capital Territory")
    expect_that(StateMap(states, country = "Australia"), not(throws_error()))

    # Normal abbreviations of the states
    names(states) <- c("NSW", "VIC", "QLD", "SA", "TAS", "NT", "WA", "ACT")
    expect_that(StateMap(states, country = "Australia"), not(throws_error()))

    # Long name of Australia is Commonwealth of Australia
    expect_that(StateMap(states, country = "Commonwealth of Australia"), not(throws_error()))

    # That's not a real state
    names(states)[1] <- "Old North Wales"
    expect_error(StateMap(states, country = "Australia"))

    # These aren't states in Austria either
    names(states)[1] <- "NSW"
    expect_error(StateMap(states, country = "Austria"))
})


test_that("US State maps with automatic renaming",
{
    # Row names are the full state names
    us.data <- state.x77
    expect_that(StateMap(us.data, country = "United States"), not(throws_error()))

    row.names(us.data) <- state.abb
    expect_that(StateMap(us.data, country = "United States"), not(throws_error()))

    expect_that(StateMap(us.data, country = "United States of America"), not(throws_error()))
})
