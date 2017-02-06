context("Sankey Diagram")
test_that("Sankey diagrame",
          {

              data(phone, package = "flipExampleData")
expect_error(print(SankeyDiagram(as.list(phone[, c("q1", "q8", "q9", "q27")]))), NA)
})
