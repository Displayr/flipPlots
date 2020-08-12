context("SplineWithSimultaneousConfIntervals")

test_that("SplineWithSimultaneousConfIntervals",
{
    expect_error(SplineWithSimultaneousConfIntervals(rep(1, 10), rnorm(10)),
        "could not be converted")
    expect_error(SplineWithSimultaneousConfIntervals(rpois(100, 5), rnorm(100),
        type = "Linear"), NA)
})
