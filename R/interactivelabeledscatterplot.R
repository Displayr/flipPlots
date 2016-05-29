#' \code{CreateInteractiveScatterplotTooltips}
#' @description Creates tooltips for \code{\link{InteractiveLabeledScatterPlot}}.
#' @param x The data that is being analzyed, where each row corresponds to a tooltip and
#' and the cells in that row appear in the tooltip.
#' @export
CreateInteractiveScatterplotTooltips <- function(x)
{
    row.labels <- rownames(x)
    column.labels <- colnames(x)
    n.rows <- length(row.labels)
    x.neat <- matrix(prettyNum(x, digits = 2), n.rows)
    column.labels.repeated <- matrix(column.labels,
        n.rows, length(column.labels), byrow = TRUE)
    cells <- matrix(paste(x.neat, column.labels.repeated),n.rows)
    rows <- apply(cells, 1, function(x) {paste(x, collapse = "<br>")})
    paste("<strong>", row.labels, "</strong><br>", rows, sep = "")
}

#' \code{InteractiveLabeledScatterPlot} Scatterplot with Labeled Points.
#' @param coords The xy coordinates of the points.
#' @param group A factor indicating group membership for each point.
#' @param group.name The title to appear above the legend, which indicates group membership.
#' @param tooltip.text The text to appear on tooltips.
#' @param row.labels A vector of labels which will, if supplied, over-ride the rownames of coodinates.
#' @param column.labels A vector of labels which will, if supplied, over-ride the colnames of coodinates.
#' @param fixed.aspect If true, forces the x and y dimensions to be on the same scale.
#' @param colors Colors that are cycled through where there is only one series, or, used to demarkate series where there are multiple series.
#' @param auto.color Automatically colors the points (if FALSE, the first color is used).
#' @param legend.width Legend area width, in pixels.
#' @importFrom scatterD3 scatterD3
#' @export
InteractiveLabeledScatterPlot <- function(coords,  group = NULL, row.labels = NULL, column.labels = NULL,
                                      group.name = "",
                                      tooltip.text = NULL,
                                      fixed.aspect = FALSE,
                                      colors =  QColors,
                                      auto.color = TRUE,
                                      legend.width = 150)
{
    # Extracting the labels
    if (is.null(row.labels))
        row.labels <- rownames(coords)
    n <- length(row.labels)
    if (is.null(column.labels))
        column.labels <- colnames(coords)
    if(is.null(column.labels))
        column.labels <- c("Dimension 1", "Dimension 2")
    if (!is.null(group))
    {
        unique.groups <- unique(group)
        n.groups <- length(unique.groups)
        if(n.groups > length(colors))
            colors <- rep(colors, n.groups)
    }
    else
    {
            legend.width <- 0
            group <- if (auto.color) 1:n else rep(1, n)
    }
    .axisLimits <- function(lim) lim + c(-1, 1) * diff(lim) * 0.05
    x.lim <- .axisLimits(range(coords[ ,1]))
    y.lim <- .axisLimits(range(coords[ ,2]))
    scatterD3::scatterD3(x = coords[,1], y = coords[,2],
          lab = row.labels,
          col_var = group, colors = strtrim(colors, 7),
          xlab = column.labels[1], ylab = column.labels[2], col_lab = group.name,
          legend_width = legend.width,
          tooltip_text = tooltip.text, fixed = fixed.aspect,
          xlim = x.lim, ylim = y.lim)
}

