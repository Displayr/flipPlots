#' Scatterplot matrix
#'
#' Wrapper around the function \code{\link[psych]{pairs.panels}} from the
#' \code{psych} package. Ensures that the variable names come through nicely
#' from Displayr.
#'
#' @param ... The variables to plot. Can be data frames or vectors, but should
#'   all be the same length as they will be bound together.
#' @param .subset Used to subset the data.
#' @param .weights Weights.
#' @param .missing How to handle missing values.
#' @export
ScatterplotMatrix <- function(..., .subset = NULL, .weights = NULL,
    .missing = c("Exclude cases with missing data",
        "Error if missing data found",
        "Imputation (replace missing values with estimates)"))
{
    .missing <- match.arg(.missing)

    arg.val.names <- as.character(match.call()[-1])
    arg.names <- names(as.list(match.call()[-1]))

    to.drop <- arg.names %in% c(".subset", ".weights", ".missing")
    if (any(to.drop))
        arg.val.names <- arg.val.names[!to.drop]

    dots <- list(...)
    named.args <- names(dots) != ""

    if (any(named.args))
        arg.val.names[named.args] <- names(dots)[named.args]

    names(dots) <- gsub("^`(.+)`$", "\\1", arg.val.names)
    all.names <- NULL
    for (counter in seq(along = dots))
    {
        question.name <- names(dots)[counter]
        variables <- dots[[counter]]

        if (!is.data.frame(variables))
            all.names <- c(all.names, question.name)
        else
            all.names <- c(all.names, paste(question.name, names(variables), sep = "\n"))
    }

    x <- data.frame(dots)

    if (!is.null(.subset))
        x$.subset <- .subset

    if (!is.null(.weights))
        x <- flipMultivariates::AdjustDataToReflectWeights(x, .weights)

    if (!is.null(.subset))
    {
        x <- x[x$.subset, ]
        x$.subset <- NULL
    }

    # Put this after the subsetting in case that removes the missing values
    if (.missing == "Exclude cases with missing data")
        x <- na.exclude(x)
    else if (.missing == "Error if missing data found")
        x <- tryCatch(na.fail(x), error = function(e) stop("The data contains missing values."))
    else if (.missing == "Imputation (replace missing values with estimates)")
        x <- flipMultivariates::SingleImputation(x)

    if (ncol(x) < 2)
        stop("You need at least 2 columns to display a scatterplot matrix.")

    psych::pairs.panels(x, labels = all.names)
}
