
library(geojsonR)
library(data.table)
library(jsonlite)

# Current method works but is slow
# Go backwards through url_js and stop at a given date?

url_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/9efe7653009b448f8d177c1da0cc068f_0.geojson")[[1]]

df = data.frame(matrix(NA, nrow = length(url_js), ncol = 22))

for (i in 1:1000) {
  coordinates = url_js[[i]]$geometry$coordinates
  names(coordinates) = c("X", "Y")
  properties = unlist(url_js[[i]]$properties)
  
  if (i == 1) {
    colnames(df) = c("X", "Y", names(properties))
  }
  
  df[i, names(coordinates)] = coordinates
  df[i, names(properties)] = properties
}

