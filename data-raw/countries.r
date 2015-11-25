library(rgdal)
#https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson
country.coordinates <- readOGR("c://delete//countries.geojson.txt", "OGRGeoJSON")
devtools::use_data(country.coordinates, internal = FALSE, overwrite = TRUE)
