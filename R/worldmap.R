globalVariables("map.coordinates")
#' \code{GeographicRegionRowNames} Names of geographic regions.
#' Returns the list of unique geographic names that
#' can be used when creating a WorldMap.
#' @param type The name of the geographic region type. See \code{\link{GeographicRegionTypes}}
#'
#' @export
GeographicRegionRowNames <- function(type)
{
    data("map.coordinates", envir=environment())
    unique(map.coordinates[[type]])
}

#' \code{GeographicRegionTypes} Types of Geographic Regions
#' The geographic region types that are available for refering
#' in a map. E.g., \code{name}, \code{continent},
#' @export
GeographicRegionTypes <- function()
{
    data("map.coordinates", envir=environment())
    names(map.coordinates)
}
# # Reading the coordinates.
# getCoordinates <- function()
# {
#     return(rgdal::readOGR("https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson", "OGRGeoJSON"))
# }

#' \code{WorldMap} World Map
#'
#' Creates a map with a table as an input, using shading to represent the values of
#' countries on the map.
#' @param table An matrix, two-dimensional array, table or vector, containing the data
#' to be plotted. The \code{\link{rownames}} (or  \code{\link{rownames}} in the case
#' of a vector) should contain the names of the geographic entities to be plotted (see
#' \code{type}).
#' @param type The type of geographic information to be plotted. By default, this is
#' \code{name}, which refers to the name of the country. To see a list of the available
#' types, use \code{GeographicRegionTypes()}. To see a complete list of names within a type,
#' use \code{GeographicRegionRowNames("type")}.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any geographical entities
#' without data with as having a 0 value.
#' @param only.show.regions.in.table When TRUE, only geographic entities that are included in
#' the table are shown on the table.
#' @param add.detail Display names of geographical entities on the map. When TRUE, it also changes
#' the appearance of the map, making the map wrap around. The only way to prevent this is to
#' resize the map.
#' @param remove.last.column Deletes the last column of the table prior to creating the map
#' , unless the table is a vector or only has one column.
#' @param remove.last.row Deletes the bottom row  of the table prior to creating the map
#' , unless the table is a vector or only has one column.
#' @param colors A vector of two colors, which are used as endpoints in interpolating colors.
#' @param color.NA The color used to represent missing values. Not used when \code{treat.NA.as.0},
#' is set to missing.
#' @param legend.title The text to appear above the legend.
#' @param remove.antarctica Automatically removes Antarctica from the ma. Defaults to TRUE.
#' @details
#' This function is based on the \code{leaflet} package. See
#' \url{https://rstudio.github.io/leaflet/} for an overview of this package and
#' how to use it without using \code{WorldMap}.
#' @export
WorldMap = function(table,
                    type = "name",
                    treat.NA.as.0 = FALSE,
                    only.show.regions.in.table = FALSE,
                    add.detail = FALSE,
                    remove.last.column = FALSE,
                    remove.last.row = FALSE,
                    colors = c("#CCF3FF","#23B0DB"),
                    color.NA = "#808080",
                    legend.title = "",
                    remove.antarctica = TRUE){
     # Correcting rowname errors for country names.
    requireNamespace("rgdal")
    if (type == "name")
    {
        correct.names <- c("United States", "United Kingdom")
        incorrect.names <- c("United States of America", "United Kingdom of Great Britain and Northern Ireland")
        rows.to.change <- match(incorrect.names, rownames(table))
        if(!is.na(rows.to.change[1]))
            rownames(table)[rows.to.change] <- correct.names
    }
    # Neatening the data.
    table.name <- deparse(substitute(table))
    if(is.vector(table) || length(dim(table)) == 1)
    {
        if(is.null(names(table)))
            stop(paste(table.name, "has no names."))
        table <- as.matrix(table)
    }
    if(length(dim(table)) != 2)
        stop(paste("Tables must contain one or more columns of data, and may not have three or more dimensions."))
    if (ncol(table) == 1 && is.null(dimnames(table)[[2]]))
        dimnames(table)[[2]] = table.name
    if(is.null(colnames(table)))
        stop(paste(table.name, "has no column names"))
    if(is.null(rownames(table)))
        stop(paste(table.name, "has no row names. The row names are required to match known geographic entitites."))
    if (remove.last.column & ncol(table) > 1)
        table <- table[, -ncol(table), drop = FALSE]
    if (remove.last.row)
        table <- table[-nrow(table), , drop = FALSE]
    table.names <- rownames(table)
    if (treat.NA.as.0)
        table[is.na(table)] <- 0
    # Getting geographic boundaries
    data("map.coordinates", envir=environment())
    coords <- map.coordinates
    coords[[type]] <- as.character(coords[[type]])
    if (remove.antarctica)
         coords <- coords[!coords$continent %in% "Antarctica",]
    coords.names <- coords[[type]]
    if (only.show.regions.in.table)
        coords <- coords[coords.names %in% table.names,]
    coords.names <- coords[[type]]
    # Checking to see if input data is OK.
    if (treat.NA.as.0)
    {
        table <- table[apply(table, 1, max, na.rm = TRUE) > 0, , drop = FALSE]
        table.names <- rownames(table)
    }
    incorrect.names <- !table.names %in% coords.names
    if (sum(incorrect.names) != 0)
        stop(paste("Incorrect rowname:", paste(table.names[incorrect.names],collapse=",")))
    # Splicing data onto coordinate data.frame.
    country.lookup <- match(coords.names,table.names)
    categories <- colnames(table)
    n.categories <- length(categories)
    for (i in 1:n.categories)
    {
        new.var <- table[country.lookup, i]
        if(treat.NA.as.0)
            new.var[is.na(new.var)] <- 0
        coords$table <- new.var
        names(coords)[ncol(coords)] <- paste("table", i, sep = "")
    }
    # Creating a variable for use in scaling the legend.
    min.value <- min(table, na.rm = TRUE)
    if (treat.NA.as.0 & nrow(table) < nrow(map.coordinates))
        min.value <- min(0, min.value)
    coords$table.max <- apply(table, 1, max)[country.lookup]
    if(treat.NA.as.0)
        coords$table.max[is.na(coords$table.max)] <- 0
    min.in.table.max <- min(coords$table.max , na.rm = TRUE)
    if (min.value < min.in.table.max) #Replacing the minimum with the global minimum.
        coords$table.max[match(min.in.table.max, coords$table.max)] <- min.value
    # Creating the map
    map = leaflet::leaflet(coords)
    opacity = 1
    if (add.detail) {
        opacity = .2
        map = leaflet::addTiles(map)
    }
    max.range <- max(coords$table.max, na.rm = TRUE)
    .pal <- leaflet::colorNumeric(palette = colors,domain = c(min.value, max.range),
            na.color = color.NA)
    map <- leaflet::addLegend(map, "bottomright", pal = .pal, values = ~table.max,
                    title = legend.title,
                    labFormat = leaflet::labelFormat(prefix = ""),
                    opacity = opacity,
                    na.label = ifelse(treat.NA.as.0, "0", "NA"))
    if (n.categories == 1) {
        map = leaflet::addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                          fillOpacity = opacity, color = ~.pal(table1))
    } else {
        for (i in 1:n.categories) {
            cl = as.formula(paste("~.pal(table", i, ")", sep = ""))
            map = leaflet::addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                               fillOpacity = opacity,
                              color = cl, group = categories[i])
        }
        map = leaflet::addLayersControl(map, baseGroups = categories,
            options = leaflet::layersControlOptions(collapsed = FALSE))
    }
map}

#
# #
# valid.continent.names <- matrix(0:2, 3, 1, dimnames = list(c("Fake", "Europe", "South America"), LETTERS[1]))
# WorldMap(valid.continent.names, type = "continent", treat.NA.as.0 = TRUE)
# #
#
# valid.continent.names <- matrix(1:3, 3, 2, dimnames = list(c("Asia", "Europe", "South America"), LETTERS[1:2]))
# WorldMap(valid.continent.names, type = "continent")
# WorldMap(valid.continent.names, type = "continent", treat.NA.as.0 = TRUE)
# WorldMap(valid.continent.names, type = "continent", treat.NA.as.0 = TRUE, remove.last.column = TRUE)
# WorldMap(valid.continent.names[,1], type = "continent", treat.NA.as.0 = TRUE)
# WorldMap(valid.continent.names, type = "continent", remove.last.column = TRUE)
# WorldMap(valid.continent.names, type = "continent", remove.last.row = TRUE)
# WorldMap(valid.continent.names, type = "continent", colors = c("#AAAAAA", "#FFFFFF"))
# WorldMap(valid.continent.names, type = "continent",
#          colors = c("#AAAAAA", "#FFFFFF"), treat.NA.as.0 = TRUE, color.NA = "#23B0DB")
# WorldMap(valid.continent.names, type = "continent",
#          colors = c("#AAAAAA", "#FFFFFF"), treat.NA.as.0 = FALSE, color.NA = "#23B0DB")
# WorldMap(valid.continent.names, type = "continent",
#          colors = c("#AAAAAA", "#FFFFFF"), treat.NA.as.0 = FALSE, color.NA = "#23B0DB",
#          legend.title = "% agree")
# WorldMap(valid.continent.names, type = "continent",
#          colors = c("#AAAAAA", "#FFFFFF"), treat.NA.as.0 = FALSE, color.NA = "#23B0DB",
#          legend.title = "% agree", only.show.regions.in.table = TRUE)
# WorldMap(valid.continent.names, type = "continent",
#          treat.NA.as.0 = TRUE,
#          legend.title = "% agree", only.show.regions.in.table = TRUE)
# valid.continent.names[1] <- NA
# WorldMap(valid.continent.names, type = "continent",
#          legend.title = "% agree", only.show.regions.in.table = TRUE)
# WorldMap(valid.continent.names, type = "continent", remove.antarctica = FALSE)
# WorldMap(valid.continent.names, type = "continent", remove.antarctica = FALSE)
# WorldMap(valid.continent.names, type = "continent", add.detail = TRUE, remove.antarctica = FALSE)
# valid.country.names <- matrix(1:2, 2,dimnames =list(c("Australia", "New Zealand"), "A"))
# WorldMap(valid.country.names)

valid.country.names <- 1:2
names(valid.country.names) <- c("Australia", "New Zealand")
WorldMap(valid.country.names)
