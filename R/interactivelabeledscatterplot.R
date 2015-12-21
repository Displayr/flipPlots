#' \code{createTooltips}
#' @description Creates tooltips for \code{\link{CorrespondenceAnalysis}}.
#' @param x The data that is being analzyed.
#' @param row.labels A vector of the row labels.
createCaTooltips <- function(x, row.labels, column.labels)
{
    .createTooltips = function(x, row.labels, column.labels)
    {
        n.rows = length(row.labels)
        x.neat = matrix(prettyNum(x, digits = 2), n.rows)
        column.labels.repeated = matrix(column.labels,
            n.rows, length(column.labels), byrow = TRUE)
        cells = matrix(paste(x.neat, column.labels.repeated),n.rows)
        rows = apply(cells, 1, function(x) {paste(x, collapse = "<br>")})
        paste("<strong>", row.labels, "</strong><br>", rows, sep = "")
    }
    c(.createTooltips(x, row.labels, column.labels),
        .createTooltips(t(x), column.labels, row.labels))
}

#' \code{LabeledScatterPlot} Scatterplot with Labeled Points.
#' @param coords The xy coordinates of the points.
#' @param group A factor indicating group membership for each point.
#' @param row.labels A vector of labels which will, if supplied, over-ride the rownames of coodinates.
#' @param column.labels A vector of labels which will, if supplied, over-ride the colnames of coodinates.
#' @param group.name Title for the legend (which only appears if group is not null).
#' @param fixed.aspect if true, forces the x and y dimensions to be on the same scale.
#' @export
InteractiveLabeledScatterPlot <- function(coords,  group = NULL, row.labels = NULL, column.labels = NULL,
                                      group.name = "",
                                      space.substitute = "\n",
                                      fixed.aspect = TRUE,
                                      colors = q.colors)
{
    # Extracting the labels
    if (is.null(row.labels))
        row.labels <- rownames(coords)
    n <- length(row.labels)
    if (is.null(column.labels))
        column.labels <- colnames(coords)
    if(is.null(column.labels))
        column.labels <- c("Dimension 1", "Dimension 2")
    scatterD3::scatterD3(x = coords[,1], y = coords[,2],
          lab = row.labels,
          col_var = group,
          xlab = column.labels[1], ylab = column.labels[2], col_lab = group.name,
          tooltip_text = createCaTooltips(coords, row.labels), fixed = fixed.aspect)
}


