context("SplineWithSimultaneousConfIntervals")

data("phone", package = "flipExampleData")
test_that("SplineWithSimultaneousConfIntervals",
{
    expect_error(SplineWithSimultaneousConfIntervals(rpois(100, 5), rnorm(100),
        type = "Linear"), NA)
    expect_error(SplineWithSimultaneousConfIntervals(phone$q4, phone$q24c.wdm,
        type = "Binary Logit"), NA)
    # Cannot use DichotomizeFactor
    expect_error(SplineWithSimultaneousConfIntervals(phone$q1, phone$q24c.wdm,
        type = "Binary Logit"), NA)
})
