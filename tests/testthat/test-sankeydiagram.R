context("Sankey Diagram")
test_that("categorizeVariables",
          {

              data(colas, package = "flipExampleData")
              v <- colas$d1
              # Factor
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Factor with missing data
              v[c(1,10,23)] <- NA
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Ordered factor
              v <- ordered(colas$d1)
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Ordered  with missing data
              v[c(1,10,23)] <- NA
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Numeric
              v <- as.numeric(colas$d1)
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Numeric with missing data
              v[c(1,10,23)] <- NA
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # character
              v <- as.character(colas$d1)
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
              # Numeric with missing data
              v[c(1,10,23)] <- NA
              for (i in 2:20) {
                  new.v <- flipPlots:::categorizeVariable(v, max.categories = i, var.name = "age")
                  expect_true(length(unique(new.v)) <= i)
              }
})


test_that("Sankey diagrams",
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
              {
                  pm <- SankeyDiagram(p, max.categories = i)
                  expect_error(print(pm), NA)
              }
              flipPlots:::categorizeVariable(p$d1, max.categories = 2, var.name = "age")
              SankeyDiagram(p, max.categories = 2)
              p <- colas[, c("d3", "d1")]
              SankeyDiagram(p, max.categories = 2)
})
