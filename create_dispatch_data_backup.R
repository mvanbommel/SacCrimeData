epoch_to_calendar = function(epoch) {
  as.Date(as.POSIXct(epoch / 1000, 
                     origin = '1970-01-01',
                     tz = 'America/Los_Angeles'),
          format = '%y-%m-%d')
}

url = "https://services5.arcgis.com/54falWtcpty3V47Z/ArcGIS/rest/services/cad_calls_year3/FeatureServer/0"
where = "Occurence_Date >= date'2019-01-01' AND Occurence_Date <= date'2019-02-07' "
limit = 1000
offset = 0
 
dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
                      as.data.frame())
offset = offset + 1000

while (epoch_to_calendar(dispatch_data$Occurence_Date[nrow(dispatch_data)]) < as.Date('2019-02-01')) {
  print(epoch_to_calendar(dispatch_data$Occurence_Date[nrow(dispatch_data)]))
  
  new_dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
                            as.data.frame())
  
  if ('try-error' %in% class(dispatch_data)) {
    # Sometimes a HTTP2 errors occurs, if so try again
    new_dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
                              as.data.frame())
  }
  
  offset = offset + 1000
  
  dispatch_data = rbind(dispatch_data, new_dispatch_data)
}

rownames(dispatch_data) = 1:nrow(dispatch_data)

save(dispatch_data, file = 'january_2019_dispatch_data.RData')
write.csv(dispatch_data, file = 'january_2019_dispatch_data.csv', row.names = FALSE)
