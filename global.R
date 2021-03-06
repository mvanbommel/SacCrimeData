#remotes::install_github(repo = "mvanbommel/esri2sf")

library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinyWidgets, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(shinyjs, warn.conflicts = FALSE, quietly = TRUE)

library(R.utils, warn.conflicts = FALSE, quietly = TRUE)

library(leaflet, warn.conflicts = FALSE, quietly = TRUE)
library(leaflet.extras, warn.conflicts = FALSE, quietly = TRUE)
library(sp, warn.conflicts = FALSE, quietly = TRUE)
library(sf, warn.conflicts = FALSE, quietly = TRUE)
library(esri2sf, warn.conflicts = FALSE, quietly = TRUE)

library(sqldf, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)

library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly = TRUE)

# Functions ----
epoch_to_calendar_date = function(epoch) {
  as.Date(as.POSIXct(epoch / 1000, 
                     origin = '1970-01-01',
                     tz = 'America/Los_Angeles'),
          format = '%y-%m-%d')
}

clean_dispatch_data = function(dispatch_data) {
  if (nrow(dispatch_data) == 0) {
    return(dispatch_data)
  } else {
    
    # Get Lattitude and Longituge from geoms
    coordinates = do.call(rbind, st_geometry(dispatch_data$geoms)) %>% 
      as.data.frame() %>% 
      setNames(c("longitude", "latitude"))
    
    dispatch_data = cbind(dispatch_data, coordinates)
    
    # Missing Latitude and Longitude to NA
    dispatch_data$longitude[dispatch_data$longitude < -142] = NA
    dispatch_data$latitude[dispatch_data$latitude < 32] = NA
    
    # Convert epoch date to calendar date
    dispatch_data$Occurence_Date = epoch_to_calendar_date(epoch = dispatch_data$Occurence_Date)
    
    return(dispatch_data)
  }
}

rename_dispatch_data = function(dispatch_data) {
  if (nrow(dispatch_data) == 0) {
    return(dispatch_data)
  } else {
    # Select and rename columns
    dispatch_data = dispatch_data %>%
      select('location' = Location,
             'call_type_code' = Call_Type,
             'call_type_description' = Description,
             'reporting_officer_id' = Reporting_Officer,
             'unit_id' = Unit_ID,
             'police_district' = Police_District,
             'police_beat' = Beat,
             'day_of_week' = Day_of_Week,
             'occurence_date' = Occurence_Date,
             'report_created' = Report_Created,
             'longitude' = longitude,
             'latitude' = latitude
      )
  }
  return(dispatch_data)
}

api_is_live = TRUE

# Total Observations ----
# If fromJSON() takes too long, return error (likely an issue with API)
number_total_observations = try(withTimeout(jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=*&returnDistinctValues=true&returnCountOnly=true&outSR=4326&f=json")$count,
                                            timeout = 5,
                                            onTimeout = 'error'))

if ('try-error' %in% class(number_total_observations)) {
  # API is down
  api_is_live = FALSE
}

# Load Backup Data ----
load('january_2019_dispatch_data.RData')
backup_dispatch_data = clean_dispatch_data(dispatch_data) %>%
  select(-geoms)

# Dates need to be characters for sqldf()
backup_dispatch_data$Occurence_Date = as.character(backup_dispatch_data$Occurence_Date)

number_total_observations = nrow(backup_dispatch_data)

rm(dispatch_data)

# Dates ----
if (api_is_live) {
  most_recent_date = epoch_to_calendar_date(jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=Occurence_Date&orderByFields=Occurence_Date%20DESC&returnGeometry=false&resultRecordCount=1&outSR=4326&f=json")$features$attributes$Occurence_Date)
} else {
  most_recent_date = max(backup_dispatch_data$Occurence_Date)
}

# Descriptions ----
if (api_is_live) {
  all_descriptions = jsonlite::fromJSON("https://services5.arcgis.com/54falWtcpty3V47Z/arcgis/rest/services/cad_calls_year3/FeatureServer/0/query?where=1=1&outFields=Description&returnGeometry=false&returnDistinctValues=true&outSR=4326&f=json")$features$attributes$Description
} else {
  all_descriptions = unique(backup_dispatch_data$Description)
}
number_total_call_type_descriptions = length(all_descriptions)

description_groups =  c('ASSAULT',
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
                        'VEHICLE ACCIDENT',
                        'OTHER')

any_crime_group_index = unique(unlist(lapply(description_groups, 
                                             grep, 
                                             all_descriptions)))
other_crime_descriptions = all_descriptions[-any_crime_group_index]


# Coordinate Conversion Models ----
load('coordinate_conversion_models.RData')


