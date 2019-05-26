#remotes::install_github(repo = "mvanbommel/esri2sf")

library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)
library(leaflet, warn.conflicts = FALSE, quietly = TRUE)
library(leaflet.extras, warn.conflicts = FALSE, quietly = TRUE)
library(sp, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(shinyWidgets, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly = TRUE)
library(esri2sf, warn.conflicts = FALSE, quietly = TRUE)


# Total Observations ----
number_total_observations = jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=*&returnDistinctValues=true&returnCountOnly=true&outSR=4326&f=json")$count

# Descriptions ----
all_descriptions = jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=Description&returnGeometry=false&returnDistinctValues=true&outSR=4326&f=json")$features$attributes$Description
number_total_call_type_descriptions = length(all_descriptions)

description_time_groups = c('IN PROGRESS', 
                            'LESS THAN 5 AGO', 
                            'LESS THAN 15 AGO')
description_crime_groups =  c('ASSAULT',
                              'BURGLARY',
                              'CARJACKING',
                              'DOMESTIC VIOLENCE',
                              'HIT & RUN',
                              'RAPE',
                              'ROBBERY',
                              'SHOOTING',
                              'STOLEN VEHICLE',
                              'THEFT',
                              'VANDALISM',
                              'VEHICLE ADDICENT',
                              'OTHER')
description_groups = c(description_time_groups, description_crime_groups)

any_crime_group_index = unique(unlist(lapply(description_crime_groups, 
                                             grep, 
                                             all_descriptions)))
other_crime_descriptions = all_descriptions[-any_crime_group_index]


# Coordinate Conversion Models ----
load('coordinate_conversion_models.RData')

# Functions ----
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
    dispatch_data$X_Coordinate[dispatch_data$longitude < -142] = NA
    dispatch_data$Y_Coordinate[dispatch_data$latitude < 32] = NA
    dispatch_data$longitude[dispatch_data$longitude < -142] = NA
    dispatch_data$latitude[dispatch_data$latitude < 32] = NA
    
    # Convert epoch date to calendar date
    dispatch_data$occurence_date = as.Date(as.POSIXct(dispatch_data$occurence_date / 1000, 
                                                      origin = '1970-01-01',
                                                      tz = 'America/Los_Angeles'),
                                           format = '%y-%m-%d')
  
    return(dispatch_data)
  }
}

