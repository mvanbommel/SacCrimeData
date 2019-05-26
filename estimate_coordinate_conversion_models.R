# Estimate linear models to convert X_Coordinates to longitude and Y_Coordinates
# to latitude
#
# Note: requires sourcing 'esri2sf.R' in https://github.com/mvanbommel/esri2sf

`%>%` = dplyr::`%>%`

url = "https://services5.arcgis.com/54falWtcpty3V47Z/ArcGIS/rest/services/cad_calls_year3/FeatureServer/0"

# Get 1000 observations
# If error returned, run command again
dispatch_data = try(esri2sf(url, limit = 1000) %>%
                      as.data.frame())

# Extract longitude and latitude from geom
coordinates = do.call(rbind, st_geometry(dispatch_data$geoms)) %>% 
  as.data.frame() %>% 
  setNames(c("longitude","latitude"))

dispatch_data = cbind(dispatch_data, coordinates)

# Missing Latitude and Longitude to NA
dispatch_data$X_Coordinate[dispatch_data$longitude < -142] = NA
dispatch_data$Y_Coordinate[dispatch_data$latitude < 32] = NA
dispatch_data$longitude[dispatch_data$longitude < -142] = NA
dispatch_data$latitude[dispatch_data$latitude < 32] = NA

# Estimate models
longitude_model = lm(X_Coordinate ~ longitude, data = dispatch_data)
latitude_model = lm(Y_Coordinate ~ latitude, data = dispatch_data)

save(latitude_model, longitude_model, file = 'coordinate_conversion_models.RData')

longitude_predictions = predict(longitude_model, newdata = dispatch_data)
latitude_predictions = predict(latitude_model, newdata = dispatch_data)

# RMSE
sqrt(mean((dispatch_data$longitude - longitude_predictions)^2, na.rm = TRUE))
sqrt(mean((dispatch_data$latitude - latitude_predictions)^2, na.rm = TRUE))

# Examine Differences
plot(dispatch_data$X_Coordinate, dispatch_data$Y_Coordinate, pch = 19)
points(longitude_predictions, latitude_predictions, col = 'red')
