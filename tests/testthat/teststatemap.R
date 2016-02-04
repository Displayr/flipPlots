library(flipPlots)
library(testthat)
context("State Map")


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

test_that("Asking for states in a country",
{
    country <- "Australia"
    aus.states <- c("Australian Capital Territory", "Jervis Bay Territory", "Macquarie Island",
        "New South Wales", "Norfolk Island", "Northern Territory", "Queensland",
        "South Australia", "Tasmania", "Victoria", "Western Australia")
    expect_equal(StatesInCountry(country), aus.states)

    expect_error(StatesInCountry("Hello World"), "Country 'Hello World' not found.")
})
