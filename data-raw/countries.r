library(rgdal)
#https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson
coordinates <- readOGR("c://delete//countries.geojson.txt", "OGRGeoJSON")
devtools::use_data(coordinates, internal = FALSE, overwrite = TRUE)
