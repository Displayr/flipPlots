library(rgdal)


getCoordinates <- function()
{
    return(rgdal::readOGR("https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson", "OGRGeoJSON"))
}

#' \code{GeographicRegionNames} Returns the list of unique geographic names that can be used when creating a WorldMap.
#'
#' @param type... The name of the geographic region type. E.g.,\code{name}, \code{continent},
#' @details
#' Uses \code{\link{Zelig:zelimethods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' @export
GeographicRegionNames <- function(type)
{
    unique(getCoordinates()[[type]])
}
# Example: GeographicRegionNames("continent")

#' \code{GeographicRegionTypes} The geographic region types that are available for refering in a map. E.g., \code{name}, \code{continent},
#'
#' @export
GeographicRegionTypes <- function()
{
    names(GeographicRegionNames())
}

#' \code{Linear Regression} Linear Regression.
#'
#' reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @details
#' Uses \code{\link{Zelig:zelimethods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' @export
WorldMap = function(table, treat.zeros.differently = TRUE,
                     remove.last.column = TRUE,
                     remove.last.row = TRUE,
                    type = "name",
                    colors = c("#CCF3FF","#23B0DB"),
                    legend.title = "",
                    only.show.regions.in.table = FALSE,
                    remove.antarctica = TRUE,
                    add.detail = FALSE){
     # Correcting comment name errors.
    if (type == "name")
    {
        correct.names <- c("United States", "United Kingdom")
        incorrect.names <- c("United States of America", "United Kingdom of Great Britain and Northern Ireland")
        rows.to.change <- match(incorrect.names, rownames(table))
        if(!is.na(rows.to.change[1]))
            rownames(table)[rows.to.change] <- correct.names
    }
    # Neatening the data.
    if(is.vector(table) || length(dim(table)) == 1)
    {
        table.name = deparse(substitute(table))
        table = as.matrix(table)
        if(is.null(dimnames(table)))
            stop(paste(table.name, "has no names."))
        dimnames(table)[[2]] = table.name
    } else
    {
        if (remove.last.column)
            table = table[, -ncol(table), drop = FALSE]
     }
     if (remove.last.row)
        table = table[-nrow(table), , drop = FALSE]
    table.names = rownames(table)
    # Getting geographic boundarie
    coords <- getCoordinates()
    coords[[type]] = as.character(coords[[type]])
    if (remove.antarctica)
         coords = coords[!coords$continent %in% "Antarctica",]

#     if (type = "name")
#     {
#     if (remove.antarctica) {
#         antarctica = c("Fr. S. Antarctic Lands", "Antarctica")
#         coords.names = coords$name
#         coords = coords[!coords.names %in% antarctica,]
#     }
    coords.names = coords[[type]]
    if (only.show.regions.in.table)
        coords = coords[coords.names %in% table.names,]
    coords.names = coords[[type]]
    # Checking to see if input data is OK.
    incorrect.names = table.names %in% coords.names
    print(incorrect.names)
    if (sum(incorrect.names) == 0)
        stop(paste("Incorrect country names:", paste(table.names[!incorrect.names],collapse=",")))
    # Splicing data onto coordinate data.frame.
    country.lookup = match(coords.names,table.names)
    categories = colnames(table)
    n.categories = length(categories)
    for (i in 1:n.categories) {
        new.var = table[country.lookup, i]
        if(treat.zeros.differently) {
            zeroes = new.var == 0
            if (sum(zeroes, na.rm = TRUE) > 0)
                new.var[zeroes] = NA
        } else
            new.var[is.na(new.var)] = 0
        coords$table = new.var
        names(coords)[ncol(coords)] = paste("table", i, sep = "")
    }
    coords$table.max = apply(table, 1, max)[country.lookup]
    require(leaflet)
    # Creating the map
    map = leaflet(coords)
    opacity = 1
    if (add.detail) {
        opacity = .2
        map = addTiles(map)
    }
    pal <- colorNumeric(palette = colors,domain = c(0, max(table)),
            na.color =ifelse(treat.zeros.differently,"white", "#808080"))
    map = addLegend(map, "bottomright", pal = pal, values = ~table.max,
                    title = legend.title,
                    labFormat = labelFormat(prefix = ""),
                    opacity = opacity, na.label = "0")
    if (n.categories == 1) {
        map = addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                          fillOpacity = opacity, color = ~pal(x1))
    } else {
        for (i in 1:n.categories) {
            cl = as.formula(paste("~pal(table", i, ")", sep = ""))
            map = addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                               fillOpacity = opacity,
                              color = cl, group = categories[i])
        }
        map = addLayersControl(map, baseGroups = categories,
            options = layersControlOptions(collapsed = FALSE))
    }

map}


z = 1:7
names(z) = c("Asia", "Africa", "Europe", "South America", "Oceania", "North America")
WorldMap(as.matrix(z), type = "continent")


#world.map <- worldMap(QInputs(formTableOrR))
