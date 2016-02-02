globalVariables(c("map.coordinates", "admin1.coordinates", "country.regions"))
#' \code{GeographicRegionRowNames} Names of geographic regions.
#'
#' Returns the list of unique geographic names that can be used when creating a
#' WorldMap.
#'
#' @param type The name of the geographic region type. See
#'   \code{\link{GeographicRegionTypes}}
#'
#' @examples
#' GeographicRegionRowNames("name")
#' GeographicRegionRowNames("continent")
#'
#' @export
GeographicRegionRowNames <- function(type)
{
    requireNamespace("sp")
    data("map.coordinates", package = packageName(), envir = environment())
    # Make sure the dataset gets loaded
    invisible(map.coordinates)

    type.names <- map.coordinates[[type]]

    if (is.factor(type.names))
        levels(type.names)
    else
        unique(type.names)
}

#' \code{GeographicRegionTypes} Types of Geographic Regions
#'
#' The geographic region types that are available for referring in a map. E.g.,
#' \code{name}, \code{continent}
#'
#' @examples
#' GeographicRegionTypes()
#'
#' @export
GeographicRegionTypes <- function()
{
    requireNamespace("sp")
    data("map.coordinates", package = packageName(), envir = environment())
    names(map.coordinates)
}
# # Reading the coordinates.
# getCoordinates <- function()
# {
#     return(rgdal::readOGR("https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson", "OGRGeoJSON"))
# }


#' Get the states in a country
#'
#' When using \code{\link{StateMap}} you need to match the state names exactly.
#' You can use this function to look up the correct names of the states for the
#' country that you are interested in.
#'
#' @param country The country to look at
#' @export
#' @seealso \code{\link{GeographicRegionRowNames}}
StatesInCountry <- function(country)
{
    requireNamespace("sp")
    data("admin1.coordinates", package = packageName(), envir = environment())

    if (!(country %in% levels(admin1.coordinates$admin)))
        stop("Country '", country, "' not found.")

    levels(droplevels(subset(admin1.coordinates, admin == country)$name))
}


#' \code{WorldMap} World Map
#'
#' Creates a map with a table as an input, using shading to represent the values of
#' countries on the map.
#'
#' @inheritParams BaseMap
#' @param remove.antarctica Automatically removes Antarctica from the map. Defaults to TRUE.
#'
#' @describeIn BaseMap A map of the countries of the world.
#' @export
WorldMap <- function(...,
    remove.antarctica = TRUE,
    remove.regions = NULL)
{
    requireNamespace("sp")
    # Getting geographic boundaries
    data("map.coordinates", package = packageName(), envir = environment())
    coords <- map.coordinates

    if (remove.antarctica)
    {
        coords <- coords[!(coords$continent %in% "Antarctica"), ]
        remove.regions <- c(remove.regions, "Antarctica")
    }

    BaseMap(..., coords = coords, remove.regions = remove.regions, name.map = admin0.name.map.by.name)
}

#' State Map
#'
#' Creates a map with a table as an input, using shading to represent the values of
#' states in countries on the map.
#'
#' @inheritParams BaseMap
#' @param country The country the states come from.
#' @param ... Other parameters to pass to \code{BaseMap}.
#'
#' @describeIn BaseMap A map of states within countries.
#' @export
StateMap <- function(table, country, ...)
{
    requireNamespace("sp")
    # Getting geographic boundaries
    data("admin1.coordinates", package = packageName(), envir = environment())

    # If the country is not an exact match, search wider for it
    if (!(country %in% names(admin0.name.map.by.admin)))
    {
        for (admin in names(admin0.name.map.by.admin))
        {
            alt <- admin0.name.map.by.admin[[admin]]
            if (country %in% alt)
            {
                country <- admin
                break
            }
        }
        rm(admin)
    }

    if (!(country %in% admin1.coordinates[["admin"]]))
        stop("Country '", country, "' was not found.")

    coords <- subset(admin1.coordinates, admin == country)

    name.map <- admin1.name.map[[country]]

    BaseMap(table = table, coords = coords, ..., name.map = name.map)
}

#' Base mapping function
#'
#' Creates a map with a table as an input, using shading to represent the values
#' of regions on the map.
#'
#' @param table A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted (see \code{type}).
#' @param coords The coordinates to be mapped.
#' @param type The type of geographic information to be plotted. By default,
#'   this is \code{name}, which refers to the name of the country. To see a list
#'   of the available types, use \code{GeographicRegionTypes()}. To see a
#'   complete list of names within a type, use
#'   \code{GeographicRegionRowNames("type")}.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any
#'   geographical entities without data with as having a 0 value.
#' @param only.show.regions.in.table When TRUE, only geographic entities that
#'   are included in the table are shown on the table.
#' @param add.detail Display names of geographical entities on the map. When
#'   TRUE, it also changes the appearance of the map, making the map wrap
#'   around. The only way to prevent this is to resize the map.
#' @param remove.last.column Deletes the last column of the table prior to
#'   creating the map, unless the table is a vector or only has one column.
#' @param remove.last.row Deletes the bottom row of the table prior to creating
#'   the map, unless the table is a vector or only has one row.
#' @param colors A vector of two colors, which are used as endpoints in
#'   interpolating colors.
#' @param color.NA The color used to represent missing values. Not used when
#'   \code{treat.NA.as.0}, is set to missing.
#' @param legend.title The text to appear above the legend.
#' @param remove.regions The regions to remove, even if they are in the table.
#' @param unmatched.regions.is.error If there are regions in \code{table} that
#'   are not found in \code{coords}, if this is \code{TRUE} it will cause an
#'   error, otherwise just print a message.
#' @param name.map A mapping between incorrect and correct names, useful for
#'   automatically fixing names that a commonly misspecified. Should be a list
#'   where the keys are the correct names and the values are vectors of
#'   incorrect names that should be changed.
#'
#' @details This function is based on the \code{leaflet} package. See
#'   \url{https://rstudio.github.io/leaflet/} for an overview of this package
#'   and how to use it without using these functions.
#'
#' @export
BaseMap <- function(table,
    coords,
    type = "name",
    treat.NA.as.0 = FALSE,
    only.show.regions.in.table = FALSE,
    add.detail = FALSE,
    remove.last.column = FALSE,
    remove.last.row = FALSE,
    colors = c("#CCF3FF", "#23B0DB"),
    color.NA = "#808080",
    legend.title = "",
    remove.regions = NULL,
    unmatched.regions.is.error = TRUE,
    name.map = NULL)
{
    # Correcting rowname errors for country names.
    # Neatening the data.
    table.name <- deparse(substitute(table))
    if (is.vector(table) || length(dim(table)) == 1)
    {
        if(is.null(names(table)))
            stop(paste(table.name, "has no names."))

        table <- as.matrix(table)
    }

    if (length(dim(table)) != 2)
        stop(paste("Tables must contain one or more columns of data, and may not have three or more dimensions."))

    if (ncol(table) == 1 && is.null(dimnames(table)[[2]]))
        dimnames(table)[[2]] = table.name

    if (is.null(colnames(table)))
        stop(paste(table.name, "has no column names"))

    if (is.null(rownames(table)))
        stop(paste(table.name, "has no row names. The row names are required to match known geographic entitites."))

    if (remove.last.column && ncol(table) > 1)
        table <- table[, -ncol(table), drop = FALSE]

    if (remove.last.row)
        table <- table[-nrow(table), , drop = FALSE]

    if (treat.NA.as.0)
        table[is.na(table)] <- 0

    # Tidying some names.
    if (type == "name" && !is.null(name.map))
    {
        for (correct in names(name.map))
        {
            incorrect <- name.map[[correct]]
            matches <- match(incorrect, rownames(table))

            if (!all(is.na(matches)))
                rownames(table)[matches[!is.na(matches)]] <- correct
        }
    }

    coords[[type]] <- as.character(coords[[type]])

    if (!is.null(remove.regions))
    {
        remove.regions <- stringr::str_trim(unlist(strsplit(remove.regions, ",", fixed = TRUE)))
        if (type == "name" && !is.null(name.map))
        {
            for (region in names(name.map))
            {
                alt <- name.map[[region]]
                matches <- match(alt, remove.regions)

                if (!all(is.na(matches)))
                    remove.regions[matches[!is.na(matches)]] <- region
            }
        }

        coords <- coords[!(coords[[type]] %in% remove.regions), ]
        table <- table[!(rownames(table) %in% remove.regions), , drop = FALSE]
    }

    if (only.show.regions.in.table)
        coords <- coords[coords[[type]] %in% rownames(table), ]

    # Checking to see if input data is OK.
    if (treat.NA.as.0)
    {
        table <- table[apply(table, 1, max, na.rm = TRUE) > 0, , drop = FALSE]
    }

    table.names <- rownames(table)
    coords.names <- coords[[type]]
    incorrect.names <- !table.names %in% coords.names

    if (any(incorrect.names))
    {
        msg <- paste("Unmatched region names:", paste(table.names[incorrect.names], collapse = ", "))
        if (unmatched.regions.is.error)
            stop(msg)
        else
            message(msg)
    }

    # Splicing data onto coordinate data.frame.
    country.lookup <- match(coords.names, table.names)
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
    if (treat.NA.as.0 && nrow(table) < nrow(coords))
        min.value <- min(0, min.value)

    coords$table.max <- apply(table, 1, max)[country.lookup]
    if (treat.NA.as.0)
        coords$table.max[is.na(coords$table.max)] <- 0

    min.in.table.max <- min(coords$table.max , na.rm = TRUE)
    if (min.value < min.in.table.max) #Replacing the minimum with the global minimum.
        coords$table.max[match(min.in.table.max, coords$table.max)] <- min.value

    # Creating the map
    map <- leaflet::leaflet(coords)
    opacity <- 1
    if (add.detail)
    {
        opacity <- 0.2
        map <- leaflet::addTiles(map)
    }
    max.range <- max(coords$table.max, na.rm = TRUE)
    .pal <- leaflet::colorNumeric(palette = colors, domain = c(min.value, max.range),
            na.color = color.NA)
    map <- leaflet::addLegend(map, "bottomright", pal = .pal, values = ~table.max,
                    title = legend.title,
                    labFormat = leaflet::labelFormat(prefix = ""),
                    opacity = opacity,
                    na.label = ifelse(treat.NA.as.0, "0", "NA"))

    if (n.categories == 1)
    {
        map <- leaflet::addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
            fillOpacity = opacity, color = ~.pal(table1))
    }
    else
    {
        for (i in 1:n.categories)
        {
            cl <- as.formula(paste("~.pal(table", i, ")", sep = ""))
            map <- leaflet::addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                fillOpacity = opacity, color = cl, group = categories[i])
        }
        map <- leaflet::addLayersControl(map, baseGroups = categories,
            options = leaflet::layersControlOptions(collapsed = FALSE))
    }

    map
}

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
# valid.country.names <- 1:2
# names(valid.country.names) <- c("Australia", "New Zealand")
# WorldMap(as.array(valid.country.names))
#
# valid.country.names <- array(1:2, dim = 2, dimnames = list(c("Australia", "New Zealand")))
# WorldMap(valid.country.names)
# a

#' World map based on \code{choroplethr}
#'
#' Similar to \code{\link{WorldMap}} but does the plotting using
#' \code{choroplethr} rather than \code{leaflet}.
#'
#' @inheritParams BaseMap
#' @param ... Other parameters to pass to \code{Choropleth}.
#' @describeIn Choropleth A choropleth of the countries of the world.
#' @export
WorldChoropleth <- function(..., remove.regions = "antarctica")
{
    Choropleth(..., region = "world", remove.regions = remove.regions)
}

#' Base function for plotting a map
#'
#' @param df A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted. If it is a matrix with more than 1 column, the first will be
#'   used as the values on the map.
#' @param region The region of the world to map. This can either be
#'   \dQuote{world} or the name of a country.
#' @inheritParams BaseMap
#' @export
Choropleth <- function(df,
    region,
    treat.NA.as.0 = FALSE,
    only.show.regions.in.table = FALSE,
    remove.last.row = FALSE,
    legend.title = "",
    remove.regions = NULL)
{
    # Correcting rowname errors for country names.
    # Neatening the data.
    table.name <- deparse(substitute(df))
    if (is.vector(df) || length(dim(df)) == 1)
    {
        if(is.null(names(df)))
            stop(paste(table.name, "has no names."))

        df <- as.matrix(df)
    }

    if (length(dim(df)) != 2)
        stop(paste("Tables must contain one or more columns of data, and may not have three or more dimensions."))

    df <- data.frame(df)

    # If we don't alreay have a "value" column, use the first column.
    if (!("value" %in% names(df)))
        names(df)[1] <- "value"

    if (is.null(rownames(df)))
        stop(paste(table.name, "has no row names. The row names are required to match known geographic entitites."))

    df$region <- tolower(rownames(df))

    # Could use RemoveRowsAndOrColumns() from flipU
    if (remove.last.row)
        df <- df[-nrow(df), , drop = FALSE]

    # Tidying some names
    df$region[df$region == "united states"] <- "united states of america"
    df$region[df$region == "united kingdom of great britain and northern ireland"] <- "united kingdom"

    if (region == "world")
    {
        data("country.regions", package = "choroplethrMaps", envir = environment())
        all.regions <- country.regions$region
    }
    else
    {
        all.regions <- choroplethrAdmin1::get_admin1_regions(region)$region
    }

    zoom <- NULL
    if (only.show.regions.in.table)
        zoom <- df$region
    else if (!is.null(remove.regions))
        zoom <- all.regions[!(all.regions %in% remove.regions)]

    # Checking to see if input data is OK.
    if (treat.NA.as.0)
    {
        df <- merge(df, data.frame(region = all.regions), by = "region", all = TRUE)
        df$value[is.na(df$value)] <- 0
        df <- df[apply(df, 1, max, na.rm = TRUE) > 0, , drop = FALSE]
    }

    if (region == "world")
    {
        map <- choroplethr::country_choropleth(df, legend = legend.title, num_colors = 1, zoom = zoom)
    }
# Cannot use this because it does not render correctly in plotly
#     else if (region == "united states of america")
#     {
#         require(choroplethrMaps)
#         map <- choroplethr::state_choropleth(df, legend = legend.title, num_colors = 1, zoom = zoom)
#     }
    else
    {
        requireNamespace("choroplethrAdmin1")
        map <- choroplethr::admin1_choropleth(region, df, legend = legend.title, num_colors = 1,
            zoom = zoom)
    }

    map
}
