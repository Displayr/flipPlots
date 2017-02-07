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
              p <- colas[, c("d1", "d2")]
              p <- data.frame(p)
              expect_error(print(SankeyDiagram(p, max.categories = 1)))
              for (i in 2:20)
                expect_error(print(SankeyDiagram(p, max.categories = i)), NA)
              p$d2 <- ordered(p$d2)
              for (i in 2:20)
                expect_error(print(SankeyDiagram(p, max.categories = i)), NA)
              flipPlots:::categorizeVariable(p$d1, max.categories = 2, var.name = "age")
              SankeyDiagram(p, max.categories = 2)
})
