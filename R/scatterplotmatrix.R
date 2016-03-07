#' Scatterplot matrix
#'
#' Wrapper around the function \code{\link[psych]{pairs.panels}} from the
#' \code{psych} package. Ensures that the variable names come through nicely
#' from Displayr.
#'
#' @param ... The variables to plot. Can be data frames or vectors, but should
#'   all be the same length as they will be bound together.
#' @export
ScatterplotMatrix <- function(...)
{
    arg.names <- as.character(match.call()[-1])
    dots <- list(...)
    names(dots) <- gsub("^`(.+)`$", "\\1", arg.names)

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

    if (ncol(x) < 2)
        stop("You need at least 2 columns to display a scatterplot matrix.")

    psych::pairs.panels(x, labels = all.names)
}
