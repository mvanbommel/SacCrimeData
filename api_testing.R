
library("esri2sf")

# https://rstudio-pubs-static.s3.amazonaws.com/301644_61297506548c4b3cb9e7cc8cbc8578ce.html
# https://github.com/yonghah/esri2sf
# https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/general_offenses_year3/FeatureServer/0/validateSQL

url = "https://services5.arcgis.com/54falWtcpty3V47Z/ArcGIS/rest/services/cad_calls_year3/FeatureServer/0"
where = "Occurence_Date < date'2019-01-03' AND (Description = 'SHOTS FIRED - LESS THAN 15 AGO' OR Call_Type = '971')"
df = esri2sf(url, where = where)








library(geojsonR)
library(jsonlite)
library(rjson)

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



filtered_url_js = FROM_GeoJson(url_file_string = "https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=Occurence_Date%3Cdate%272018-01-07%27&outFields=*&outSR=4326&f=json")

test = fromJSON(file = "https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=Occurence_Date%3Cdate%272018-01-07%27&outFields=*&outSR=4326&f=json")



https://developers.arcgis.com/rest/services-reference/query-feature-service-layer-.htm
http://doc.arcgis.com/en/arcgis-online/reference/sql-agol.htm
http://data.cityofsacramento.org/datasets/sacramento-dispatch-data-from-current-year/geoservice
https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=Occurence_Date%3Cdate%272018-01-03%27&outFields=*&resultOffset=1000&outSR=4326&f=json
https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1%3D1&outFields=*&returnCountOnly=true&outSR=4326&f=json


#
https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=Occurence_Date%3Cdate%272018-01-03