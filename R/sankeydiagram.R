#' Sankey Diagram
#'
#' Creates a sankey diagram of the relationship between different variables.
#' @param data A \code{\link{data.frame}} of variables.
#' @param max.categories When the number of unique values
#' of numeric data exceeds this value, the variable is quantized.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process.
#' @param weights An optional vector of sampling weights.
#' @param font.size Font size of node labels.
#' @param font.family Font family of node labels.
#' @param colors Colors of the nodes, supplied as a vector of hex colors.
#' @param node.width Width of the width.
#' @param link.color One of \code{"None", "Source", "Target"}. This specifies how whether
#'     the links are shown in grey (None), the same color as the source node, or the same
#'     color as the target node.
#' @importFrom networkD3 sankeyNetwork JS
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @return A sankey diagram (HTMLwidget).
#' @details Text variables are grouped as having text or not having text.
#' To see patterns with text variables, they should first be turned into
#' factors.
#' @export
SankeyDiagram <- function(data, max.categories = 8, subset = NULL, weights = NULL,
                          font.size = 12, font.family = "Times New Roman", colors = NULL,
                          link.color = c("None", "Source", "Target")[1], node.width = 30)
{
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    if (nrow(data) < 2)
        stop(paste0(nrow(data), "observations: more data is required to create a Sankey diagram."))

    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")

    # Take subset and resample to generate a weighted sample, if necessary.
    if (is.null(subset))
        subset.data <- data
    else {
        subset.data <- data[subset, , drop = FALSE]
        weights <- weights[subset]
    }

    weight.subset.data <- if (is.null(weights))
        subset.data
    else
        suppressWarnings(AdjustDataToReflectWeights(subset.data, weights))

    variables <- categorizeData(weight.subset.data, max.categories)
    links <- computeLinks(variables)
    nodes <- nodeDictionary(variables)

    # Setting colors
    if (is.null(colors))
        color.str <- "d3.scale.category20()"
    else
    {
        if (length(colors) < nrow(nodes))
            colors <- paste0(colors, rep("", nrow(nodes)))
        color.str <- paste0('d3.scale.ordinal().range(["',
                     paste(c(colors[-(1:2)], colors[1:2]), collapse = '","'), '"])')
    }
    grps <- 0:(nrow(nodes)-1)
    if (link.color == "None")       # nodes at each level to be the same
        nodes$group <- factor(rep(1:ncol(variables), sapply(variables, function(x){length(unique(x))})))
    else
        nodes$group <- factor(grps) # all nodes different colors
    if (link.color == "Source")
        links$group <- as.factor(links$source)
    else
        links$group <- as.factor(links$target)
    sankeyNetwork(Links = links, LinkGroup = if (link.color == "None") NULL else 'group',
                Nodes = nodes, NodeID = 'name', NodeGroup = 'group', nodeWidth = node.width,
                Source = "source", Target = "target", Value = "value",
                fontSize = font.size, fontFamily = font.family, colourScale = JS(color.str))
}

#' computeLinks
#'
#' Computes the links between the nodes, so that can be expressed as a network.
#' @param data A \code{\link{data.frame}} or \code{\link{list}} of variables.
#' @importFrom stats xtabs
computeLinks <- function(data) {
    links <- NULL
    counter <- 0
    n <- length(data)
    for (i in 1:(n - 1)){
        x <- data[[i]]
        y <- data[[i + 1]]
        x.names <- levels(x)
        n.x <- length(x.names)
        n.y <- nlevels(y)
        x.y <- xtabs(~ x + y)
        for (i.x in 1:n.x) {
            row.node <- counter + i.x - 1
            for (i.y in 1:n.y) {
                value <- x.y[i.x, i.y]
                if (value > 0) {
                    column.node <- counter + n.x + i.y - 1
                    links <- rbind(links,c(row.node,column.node, x.y[i.x, i.y]))
                }
            }
        }
        counter <- counter + n.x
    }
    links <- as.data.frame(links)
    names(links) <- c("source", "target", "value")
links}

#' nodeDictionary
#'
#' Creates a dictionary of the nodes.
#' @param list.of.factors Factors representing variables in the data file.
nodeDictionary <- function(list.of.factors)
{
    nodes <- NULL
    for (vr in list.of.factors)
    {
        for (r in levels(vr)) {
            nodes <- c(nodes, r)
         }
    }
    data.frame(name = nodes)
}

#' categorizeData
#'
#' Creates a dictionary of the nodes.
#' Quantizes numeric variables.
#' @param data A \code{\link{data.frame}} or \code{\link{list}} of variables.
#' @param max.categories When the number of unique values
#' of numeric data exceeds this value, the variable is quantized.
categorizeData <- function(data, max.categories)
{
    var.names <- names(data)
    n <- length(var.names)
    nodes <- NULL
    for (i in 1:n)
    {
        vr <- categorizeVariable(data[[i]], max.categories, var.names[i])
        data[[i]] <- vr
        for (r in levels(vr))
            nodes <- c(nodes, r)
    }
data}

#' categorizeVariable
#'
#' Quantizes a numeric variable.
#' @param x A variable.
#' @param max.categories With numeric
#' variables, \code{\link{cut}} is used, unless \code{max.categories} has a value of
#' 2 and there are missing values, in which case the data is turned into a factor
#' with levels of "Data" and "NA"). With factors, the two smallest
#' categories are merged (based on unweighted data). With ordered factors, the
#' smallest category is merged with the smallest adjacent category.
#' Missing values are not merged. With character variables, the data is merged into missing
#' versus non-missing data.
#' @param var.name The name of the variable.
#' of numeric data exceeds this value, the variable is quantized.
#' @importFrom flipTransformations Factor
categorizeVariable <- function(x, max.categories, var.name)
{
    if (max.categories < 2)
        stop("'max.categories must be more than 1.")
    uniques <- unique(x)
    n.unique <- length(uniques)
    n <- length(x)
    if (n.unique > max.categories)
    {
        if(is.factor(x) | (ordered <- is.ordered(x)))
        {
            x <- Factor(x)
            n.to.merge <- n.unique - max.categories
            for (i in 1:n.to.merge)
            {
                counts <- table(x)
                if (!ordered)
                    counts <- sort(counts)
                if (!ordered | length(counts) == 2)
                    merge <- 1:2
                else
                {
                    merge <- match(min(counts), counts)[1]
                    if (merge == 1)
                        merge <- 2
                    else if (merge == length(counts) & ordered)
                        merge <- merge - 1
                    merge <- c(merge, if(counts[merge - 1] < counts[merge + 1]) merge - 1 else merge + 1)
                    merge <- sort(merge)
                }
                merge <- match(names(counts)[merge], levels(x))
                levels(x)[merge] <- paste(levels(x)[merge], collapse = ",")
            }
            valid <- x
        } else if (is.numeric(x)) {
            n.cuts <- max.categories - if(any(is.na(uniques))) 1 else 0
            if (n.cuts == 1)
                valid <- factor(as.integer(!is.na(x)), 1:2, c("Data", "NA"))
            else
                valid <- cut(x, n.cuts)
        } else {
            valid <- rep( "Text", n)
            valid[x == ""] <- "BLANK"
        }
    } else {
       valid <- Factor(x)
    }
    valid <- addNA(valid, ifany = TRUE)
    levels(valid) <- paste(var.name, levels(valid), sep = ": ")
    valid
}

