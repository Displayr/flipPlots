library(testthat)
context("State Map")


test_that("Australian State maps with automatic renaming",
{
    states <- 1:8
    # State and country names as they are in the source data
    names(states) <- c("New South Wales", "Victoria", "Queensland", "South Australia",
        "Tasmania", "Northern Territory", "Western Australia", "Australian Capital Territory")
    expect_error(StateMap(states, country = "Australia"), NA)
    # Guess country from state names
    expect_error(StateMap(states), NA)

    # Normal abbreviations of the states
    names(states) <- c("NSW", "VIC", "QLD", "SA", "TAS", "NT", "WA", "ACT")
    expect_error(StateMap(states, country = "Australia"), NA)
    expect_error(StateMap(states), NA)

    # Long name of Australia is Commonwealth of Australia
    expect_error(StateMap(states, country = "Commonwealth of Australia"), NA)

    # These aren't states in Austria
    expect_error(StateMap(states, country = "Austria"))

    # That's not a real state
    names(states)[1] <- "Old North Scotland"
    expect_error(StateMap(states, country = "Australia"))
})


test_that("US State maps with automatic renaming",
{
    # Row names are the full state names
    us.data <- state.x77
    expect_error(StateMap(us.data, country = "United States"), NA)
    expect_error(StateMap(us.data),NA)

    row.names(us.data) <- state.abb
    expect_error(StateMap(us.data, country = "United States"), NA)
    expect_error(StateMap(us.data), NA)

    expect_error(StateMap(us.data, country = "United States of America"), NA)
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
