#' Sankey Diagram
#'
#' Creates a sankey diagram of the relationship between different variables.
#' @param data A \code{\link{data.frame}} or \code{\link{list}} of variables.
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
        data[[i]] <- vr <- categorizeVariable(data[[i]], max.categories, var.names[i])
        for (r in levels(vr))
            nodes <- c(nodes, r)
    }
data}

#' categorizeVariable
#'
#' Quantizes a numeric variable.
#' @param x A variable.
#' @param max.categories When the number of unique values
#' @param var.name The name of the variable.
#' of numeric data exceeds this value, the variable is quantized.
categorizeVariable <- function(x, max.categories, var.name)
{
    n.unique <- length(unique(x))
    n <- length(x)
    if (n.unique > max.categories)
    {
        if(is.factor(x) | is.ordered(x))
        {
            valid <- rep( "Categories", n)
            valid[is.na(x)] <- "Missing"
        } else if (is.numeric(x)) {
            n.cat <- max.categories - ifelse(sum(is.na(x)) == 0, 0, 1)
            valid <- as.character(cut(x, n.cat))
        } else {
            valid <- rep( "Text", n)
            valid[x == ""] <- "BLANK"
        }
    } else {
       valid <- as.character(x)
    }
    valid <- paste(var.name, valid,sep = "\n")
    as.factor(valid)
}
