
library(geojsonR)
library(data.table)
library(rjson)

url_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/9efe7653009b448f8d177c1da0cc068f_0.geojson")

str(url_js)



test = rbindlist(url_js[[1]], fill=TRUE)


dfs <- lapply(url_js[[1]], data.frame, stringsAsFactors = FALSE)

test = unlist(url_js[[1]])
  
test2 = do.call("rbind", test)


result <- fromJSON(unlist(url_js[[1]]))
