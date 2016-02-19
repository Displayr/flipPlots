base.url <- "https://raw.githubusercontent.com/datasets/geo-boundaries-world-110m/master/countries.geojson"
download.file(base.url, f <- tempfile())
country.coordinates <- rgdal::readOGR(f, "OGRGeoJSON")

country.coordinates.df <- data.frame(country.coordinates)
column.class <- sapply(country.coordinates.df, class)
column.class <- column.class[column.class == "factor"]
rm(country.coordinates.df)

for (column in names(column.class))
    Encoding(levels(country.coordinates[[column]])) <- "UTF-8"

devtools::use_data(country.coordinates, internal = FALSE, overwrite = TRUE)


# Code source: http://stackoverflow.com/questions/29118059/display-spatialpolygonsdataframe-on-leaflet-map-with-r
download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/50m/cultural',
                        'ne_50m_admin_0_countries.zip'), f <- tempfile())
unzip(f, exdir=tempdir())
map.coordinates.50 <- rgdal::readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')

map.coordinates.df <- data.frame(map.coordinates.50)
column.class <- sapply(map.coordinates.df, class)
column.class <- column.class[column.class == "factor"]
rm(map.coordinates.df)

for (column in names(column.class))
    # Encoding(levels(map.coordinates.50[[column]])) <- "UTF-8"
    levels(map.coordinates.50[[column]]) <- enc2utf8(levels(map.coordinates.50[[column]]))

devtools::use_data(map.coordinates.50, internal = FALSE, overwrite = TRUE)


#http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip
download.file(file.path('http://www.naturalearthdata.com/http/',
    'www.naturalearthdata.com/download/110m/cultural',
    'ne_110m_admin_0_countries.zip'), f <- tempfile())
unzip(f, exdir=tempdir())
map.coordinates.110 <- rgdal::readOGR(tempdir(), 'ne_110m_admin_0_countries', encoding='UTF-8')

map.coordinates.df <- data.frame(map.coordinates.110)
column.class <- sapply(map.coordinates.df, class)
column.class <- column.class[column.class == "factor"]
rm(map.coordinates.df)

for (column in names(column.class))
    # Encoding(levels(map.coordinates.110[[column]])) <- "UTF-8"
    levels(map.coordinates.110[[column]]) <- enc2utf8(levels(map.coordinates.110[[column]]))

devtools::use_data(map.coordinates.110, internal = FALSE, overwrite = TRUE)


f <- tempfile()
download.file(file.path('http://www.naturalearthdata.com/http/',
    'www.naturalearthdata.com/download/10m/cultural',
    'ne_10m_admin_1_states_provinces.zip'), f)
d <- tempdir()
unzip(f, exdir = d)
admin1.coordinates <- rgdal::readOGR(d, 'ne_10m_admin_1_states_provinces')

admin1.coordinates.df <- data.frame(admin1.coordinates)
column.class <- sapply(admin1.coordinates.df, class)
column.class <- column.class[column.class == "factor"]
rm(admin1.coordinates.df)

for (column in names(column.class))
    Encoding(levels(admin1.coordinates[[column]])) <- "UTF-8"

devtools::use_data(admin1.coordinates, internal = FALSE, overwrite = TRUE)
