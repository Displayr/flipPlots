#' Sankey Diagram
#'
#' Creates a sankey diagram of the relationship between different variables.
#' @param data A \code{\link{data.frame}} of variables.
#' @param max.categories When the number of unique values
#' of numeric data exceeds this value, the variable is quantized.
#' @importFrom networkD3 sankeyNetwork
#' @return A sankey diagram (HTMLwidget).
#' @details Text variables are grouped as having text or not having text.
#' To see patterns with text variables, they should first be turned into
#' factors.
#' @export
SankeyDiagram <- function(data, max.categories = 8)
{
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    if (nrow(data) < 2)
        stop(paste0(nrow(data), "observations: more data is required to create a Sankey diagram."))
    variables <- categorizeData(data, max.categories)
    links <- computeLinks(variables)
    nodes <- nodeDictionary(variables)
    sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
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
data.frame(name = nodes)}

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
#' @param max.categories When the number of unique values. With numeric
#' variables, \code{\link{cut}} is used. With factors, the two smallest
#' categories are merged (based on unweighted data). With ordered factors, the
#' smallest category is merged with the category immediately prior (unless the
#' smallest is the first category, in which case it is merged with the category above it).
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
        if(is.factor(x) | is.ordered(x))
        {
            n.to.merge <- n.unique - max.categories
            for (i in 1:n.to.merge)
            {
                counts <- table(x)
                if (!is.ordered(x))
                    counts <- sort(counts)
                smallest.cat <- match(min(counts), counts)[1]
                if (smallest.cat == 1)
                    smallest.cat <- 2
                merge <- c(-1:0) + smallest.cat
                merge <- match(names(counts)[merge], levels(x))
                levels(x)[merge] <- paste(levels(x)[merge], collapse = ",")
            }
            valid <- x
        } else if (is.numeric(x)) {
            valid <- cut(x, max.categories - if(any(is.na(uniques))) 1 else 0)
        } else {
            valid <- rep( "Text", n)
            valid[x == ""] <- "BLANK"
        }
    } else {
       valid <- Factor(x)
    }
    valid <- addNA(valid)
    levels(valid) <- paste(var.name, levels(valid), sep = ": ")
    valid
}
