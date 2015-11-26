library(rgdal)
#https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson
#country.coordinates <- readOGR("c://delete//countries.geojson.txt", "OGRGeoJSON")
# Code source: http://stackoverflow.com/questions/29118059/display-spatialpolygonsdataframe-on-leaflet-map-with-r
download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/50m/cultural',
                        'ne_50m_admin_0_countries.zip'),
f <- tempfile())
unzip(f, exdir=tempdir())
library(rgdal)
map.coordinates <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')
devtools::use_data(map.coordinates, internal = FALSE, overwrite = TRUE)
