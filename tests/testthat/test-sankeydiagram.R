context("Sankey Diagram")
test_that("Sankey diagrame",
          {

              data(phone, package = "flipExampleData")
              p <- phone[, c("q1", "q8", "q9", "q27")]
              expect_error(print(SankeyDiagram(as.list(p))), NA)
              p <- data.frame(p)
              expect_error(print(SankeyDiagram(p)), NA)
              p <- p[p$q1 == 1, ]
              expect_error(print(SankeyDiagram(p)), NA)
              data(colas, package = "flipExampleData")
              p <- colas[, c("d3", "d4")]
              expect_error(print(SankeyDiagram(as.list(p))), NA)
              p <- data.frame(p)
              expect_error(print(SankeyDiagram(p)), NA)
              p1 <- p[p$q1 == "Female", ]
              expect_error(print(SankeyDiagram(p1)))
              p <- p[p$d3 == "Female", ]
              expect_error(print(SankeyDiagram(p)), NA)
})
