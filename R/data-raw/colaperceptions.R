#' Read CSV File as Table.
#' \code{ReadCsvTable} reads a CSV file and turns the first column into row names.table in where the first row and column contain labels.
#' @param file A path and file name.
#' @param as.is Prevents factors from automatically being created (see \code{\link[utils]{read.csv}})
#' @return A \code{\link[base]{data.frame}} containing a representation of the data in the file.
#' @examples
#' dataset <- system.file("extdata", "Cola_perceptions.csv", package="flip")
#' ReadCsvTable(dataset)
#
#' ReadCsvTable("http://surveyanalysis.org/images/c/ca/Cola_perceptions.csv")

ReadCsvTable <- function(file, as.is = TRUE, ...){
  # Reads a table in where the first row and column contain labels
  #
  # Args:
  #   file: file name and path
  # Returns: matrix or data.frame
  #
  x = read.csv(file, as.is = as.is, ...)
  row.names(x) = x[,1]
  x = x[,-1]
  x
}



dataset <- system.file("extdata", "Cola_perceptions.csv", package="flip")
#colaPerceptions = ReadCsvTable("http://surveyanalysis.org/images/c/ca/Cola_perceptions.csv")
colaPerceptions = ReadCsvTable(dataset)
colaPerceptions = as.matrix(colaPerceptions)
dimnames(colaPerceptions) = list("Brands" = dimnames(colaPerceptions)[[1]], "Attributes"
                                 = dimnames(colaPerceptions)[[2]])
devtools::use_data(colaPerceptions, internal = FALSE, overwrite = TRUE)
