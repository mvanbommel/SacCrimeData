library(DT)
library(leaflet)
library(dplyr)


# To do:
# - warning message if selected number of points is too large
# - dynamix max number of points


# Functions ----
missing_data_index = function(column_name, dispatch_data) {
  if (grepl('date', column_name)) {
    type = 'date'
  } else if (grepl('time', column_name)) {
    type = 'time'
  } else {
    type = column_name
  }
  if (type %in% c('date', 'time')) {
    column = dispatch_data[, column_name]
    
    # Remove white space and extract the first 10 characters
    # - missing times will be blank
    # - missing dates will be 1899-01-01 
    which((gsub("^\\s+|\\s+$", "", substr(as.character(column), 1, 10)) %in% c('', '1899-01-01')))
    
  } else if (type == 'longitude') {
    which(dispatch_data[, column_name] == -142.954)
  } else if (type == 'latitude') {
    which(dispatch_data[, column_name] == 31.096)
  } else {
    stop(paste0('Column: ', column_name, 'not applicable for missing_data_index()'))
  }
  
}

convert_to_date_time <- function(date_time_name, dispatch_data) {
  dispatch_data[, paste0(date_time_name, '_date_time')] = as.POSIXct(paste0(substr(dispatch_data[, paste0(date_time_name, '_date')], 1, 10), ' ', dispatch_data[, paste0(date_time_name, '_time')]))
  dispatch_data = dispatch_data[, -which(colnames(dispatch_data) %in% c(paste0(date_time_name, '_date'), paste0(date_time_name, '_time')))]
  dispatch_data
}


# Read Data ----
dispatch_data = read.csv('Sacramento_Dispatch_Data_From_Current_Year.csv')
colnames(dispatch_data)[1] = 'X'

dispatch_data = dispatch_data %>%
  mutate(Report_Created = ifelse(Report_Created == Y, TRUE, FALSE),
         Day_of_Week = recode(Day_of_Week, 'Sun' = 'Sunday',
                                              'Mon' = 'Monday',
                                              'Tue' = 'Tuesday',
                                              'Wed' = 'Wednesday',
                                              'Thu' = 'Thursday', 
                                              'Fri' = 'Friday',
                                              'Sat' = 'Saturday')) %>%
  select('location' = Location,
         'call_type_code' = Call_Type,
         'call_type_description' = Description,
         'reporting_officer_id' = Reporting_Officer,
         'unit_id' = Unit_ID,
         'police_district' = Police_District,
         'police_beat' = Beat,
         'day_of_week' = Day_of_Week,
         'occurence_date' = Occurence_Date,
         'occurence_time' = Occurence_Time,
         'received_date' = Received_Date,
         'received_time' = Received_Time,
         'dispatch_date' = Dispatch_Date,
         'dispatch_time' = Dispatch_Time,
         'enroute_date' = Enroute_Date,
         'enroute_time' = Enroute_Time,
         'at_scene_date' = At_Scene_Date,
         'at_scene_time' = At_Scene_Time,
         'clear_date' = Clear_Date,
         'clear_time' = Clear_Time,
         'longitude' = X,
         'latitude' = Y,
         'report_created' = Report_Created)

# Remove rows with locations outside of Sacramento
dispatch_data = dispatch_data[-which(dispatch_data$latitude < 38 | dispatch_data$latitude > 39 | dispatch_data$longitude < -122 | dispatch_data$longitude > -121), ]


# Remove rows with missing time or date information, or missing latitude/longitude
time_points = c('occurence', 'received', 'dispatch', 'enroute', 'at_scene', 'clear')

missing_time_index = lapply(paste0(time_points, '_time'), missing_data_index, dispatch_data)
missing_date_index = lapply(paste0(time_points, '_date'), missing_data_index, dispatch_data)

any_time_missing = Reduce(union, missing_time_index)
any_date_missing = Reduce(union, missing_date_index)

missing_latitude_index = missing_data_index('latitude', dispatch_data)
missing_longitude_index = missing_data_index('longitude', dispatch_data)

any_data_missing = Reduce(union, list(any_time_missing, any_date_missing, missing_latitude_index, missing_longitude_index))

dispatch_data = dispatch_data[any_data_missing, ]

# Convert separate date and time columns to combined date-time columns
for (name in time_points) {
  dispatch_data = convert_to_date_time(name, dispatch_data)
}

# Add decimal integer data for filtering
dispatch_data[, 'occurence_time'] = as.numeric(format(dispatch_data$occurence_date_time, "%H")) + (as.numeric(format(dispatch_data$occurence_date_time, "%M")) / 60)
 

# hist(as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time)[which(as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time) < 10000 & as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time) > 0)] / 60)
# 

# Look at times by beat or description


# Ideas:
# response times
# times of day (downloaded data in PST, API data in UTC)
# day of week
# locations



###########
# Mapping
###########
# m <- leaflet() %>% setView(lng = -121.49, lat = 38.57, zoom = 11)
# m %>% addTiles()
# 
# dispatch_data = dispatch_data[1:100, ]
# 
# leaflet(data = dispatch_data) %>% addTiles() %>%
#   addMarkers(~dispatch_data$longitude, ~dispatch_data$latitude, popup = ~as.character(dispatch_data$Description), label = ~as.character(dispatch_data$Location))





# length(unique(dispatch_data$Call_Type))
# unique(dispatch_data$Description)
#        
# sort(table(dispatch_data$Description))
# 
# hist(as.numeric(dispatch_data$At_Scene_Time) - as.numeric(dispatch_data$Received_Time))
# 
# as.numeric(dispatch_data$At_Scene_Time[6]) - as.numeric(dispatch_data$Received_Time[6])
