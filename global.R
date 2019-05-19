library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)
library(leaflet, warn.conflicts = FALSE, quietly = TRUE)
library(leaflet.extras, warn.conflicts = FALSE, quietly = TRUE)
library(sp, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(shinyWidgets, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly = TRUE)


# To do:
# -improve map
#   - fix shape drawing filter
#   - map appearance https://uasnap.shinyapps.io/ex_leaflet/


number_total_observations = jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=*&returnDistinctValues=true&returnCountOnly=true&outSR=4326&f=json")$count
number_total_call_type_descriptions = jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=Description&returnDistinctValues=true&returnCountOnly=true&outSR=4326&f=json")$count


clean_dispatch_data = function(dispatch_data) {
  if (nrow(dispatch_data) == 0) {
    return(dispatch_data)
  } else {
  
    # Get Lattitude and Longituge from geoms
    coordinates = do.call(rbind, st_geometry(dispatch_data$geoms)) %>% 
      as.data.frame() %>% 
      setNames(c("longitude","latitude"))
    
    # Select and rename columns
    dispatch_data = cbind(dispatch_data, coordinates) %>%
      select('location' = Location,
             'call_type_code' = Call_Type,
             'call_type_description' = Description,
             'reporting_officer_id' = Reporting_Officer,
             'unit_id' = Unit_ID,
             'police_district' = Police_District,
             'police_beat' = Beat,
             'day_of_week' = Day_of_Week,
             'occurence_date' = Occurence_Date,
             'report_created' = Report_Created)
    
    dispatch_data = cbind(dispatch_data, coordinates)
    
    # Missing Latitude and Longitude to NA
    dispatch_data$longitude[dispatch_data$longitude == -142.954019480548] = NA
    dispatch_data$latitude[dispatch_data$latitude == 31.0959930602462] = NA
    
    # Convert epoch date to calendar date
    dispatch_data$occurence_date = as.Date(as.POSIXct(dispatch_data$occurence_date / 1000, 
                                                      origin = '1970-01-01',
                                                      tz = 'America/Los_Angeles'),
                                           format = '%y-%m-%d')
  
    return(dispatch_data)
  }
}

