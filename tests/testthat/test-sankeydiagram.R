library(stats)

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

expenses <- structure(list(Category = structure(c(4L, 2L, 1L, 2L, 1L, 3L,
2L, 2L, 3L, 5L, 2L, 2L, 1L, 1L, 5L, 3L, 3L, 3L, 1L, 3L, 1L, 2L,
1L, 1L, 5L, 2L, 4L, 3L, 5L, 1L, 3L, 5L, 5L, 5L, 1L, 3L, 1L, 1L,
3L, 5L, 1L, 5L, 5L, 1L, 3L, 1L, 5L, 3L, 2L, 3L, 1L, 1L, 5L, 2L,
3L, 3L, 4L, 3L, 5L, 3L, 2L, 5L, 5L, 3L, 5L, 5L, 1L), .Label = c("Development Assistance",
"Humanitarian Assistance", "Normative, Treaty-related and Knowledge creation activities",
"Peacekeeping Operations", "Technical Cooperation"), class = "factor"),
    Agency = structure(c(1L, 30L, 14L, 19L, 20L, 12L, 20L, 12L,
    31L, 11L, 25L, 8L, 17L, 12L, 8L, 15L, 6L, 3L, 2L, 2L, 16L,
    31L, 27L, 30L, 24L, 2L, 24L, 32L, 23L, 21L, 34L, 31L, 6L,
    12L, 13L, 10L, 5L, 31L, 16L, 16L, 24L, 4L, 33L, 9L, 26L,
    8L, 3L, 4L, 17L, 29L, 32L, 18L, 18L, 24L, 18L, 7L, 12L, 22L,
    34L, 28L, 18L, 10L, 7L, 8L, 28L, 29L, 10L), .Label = c("DPKO",
    "FAO", "IAEA", "ICAO", "IFAD", "ILO", "IMO", "IOM", "ITC",
    "ITU", "PAHO", "UN", "UNAIDS", "UNDP", "UNEP", "UNESCO",
    "UNFPA", "UN-HABITAT", "UNHCR", "UNICEF", "UNIDO", "UNITAR",
    "UNODC", "UNOPS", "UNRWA", "UNU", "UNWOMEN", "UNWTO", "UPU",
    "WFP", "WHO", "WIPO", "WMO", "WTO"), class = "factor")), .Names = c("Category",
"Agency"), row.names = c(NA, 67L), class = "data.frame", weights = c(8876176000,
5018199183, 4659525828, 3846924119, 2899073124, 2852121000, 2528181910,
1925690000, 1769674421, 1363470773, 1316762306, 936364827, 844803603,
709197000, 571246061, 561342000, 475664289, 463845188, 460742988,
426273315, 398210228, 340307917, 339801000, 337209860, 326506000,
314801837, 274471000, 273903334, 241906000, 235511425, 229931293,
208477657, 199272711, 188584000, 181750055, 170742371, 169727000,
152602283, 132736743, 132736743, 116733000, 108720761, 98226341,
91197000, 89986553, 86851769, 85799163, 83625879, 77714189, 74487892,
73133740, 61980404, 60104070, 52163000, 51875314, 47273818, 37442000,
23854000, 19303415, 18198766, 12473530, 11099580, 10547639, 7844760,
4972566, 2915644, 1762927))

test_that("Sankey diagrams: weights and filter",
          {
              data(phone, package = "flipExampleData")
              p <- phone[, c("q1", "q8", "q9", "q27")]
              subset <- rep(TRUE, nrow(p))
              subset[phone$q20h3 == "No"] <- FALSE
              expect_error(print(SankeyDiagram(p, subset = subset)), NA)
              weights <- sample(3, nrow(p), replace = TRUE)
              expect_error(print(SankeyDiagram(p, weights = weights)), NA)
              expect_error(print(SankeyDiagram(p, subset = subset, weights = weights)), NA)

              data(colas, package = "flipExampleData")
              p <- colas[, c("d1", "d2")]
              subset <- rep(TRUE, nrow(p))
              subset[colas$Q5_5_7 == "No"] <- FALSE
              expect_error(print(SankeyDiagram(p, subset = subset)), NA)
              weights <- rnorm(nrow(p))
              weights[weights < 0] <- 0
              expect_error(print(SankeyDiagram(p, weights = weights)), NA)
              expect_error(print(SankeyDiagram(p, subset = subset, weights = weights)), NA)

              # Large weights
              expect_error(SankeyDiagram(expenses, weights = attr(expenses, "weights")), NA)
})


