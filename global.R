library(DT)

dispatch_data = read.csv('Sacramento_Dispatch_Data_From_Current_Year.csv')

available_data_index = function(column_name, dispatch_data, type='Time') {
  column = dispatch_data[, column_name]
  which(!(gsub("^\\s+|\\s+$", "", substr(as.character(column), 1, 10)) %in% c('', '1899-01-01')))
}

time_points = c('Occurence', 'Received', 'Dispatch', 'Enroute', 'At_Scene', 'Clear')

available_time_index = lapply(paste0(time_points, '_Time'), available_data_index, dispatch_data, type='Time')
available_date_index = lapply(paste0(time_points, '_Date'), available_data_index, dispatch_data, type='Date')

all_times_available = Reduce(intersect, available_time_index)
all_dates_available = Reduce(intersect, available_date_index)

all_date_times_available = Reduce(intersect, list(all_time_available, all_dates_available))

dispatch_data = dispatch_data[all_date_times_available, ]

convert_to_date_time <- function(date_time_name, dispatch_data) {
  dispatch_data[, paste0(date_time_name, '_Date_Time')] = as.POSIXct(paste0(substr(dispatch_data[, paste0(date_time_name, '_Date')], 1, 10), ' ', dispatch_data[, paste0(date_time_name, '_Time')]))
  dispatch_data = dispatch_data[, -which(colnames(dispatch_data) %in% c(paste0(date_time_name, '_Date'), paste0(date_time_name, '_Time')))]
  dispatch_data
}

for (name in time_points) {
  dispatch_data = convert_to_date_time(name, dispatch_data)
}


hist(as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time)[which(as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time) < 10000 & as.numeric(dispatch_data$At_Scene_Date_Time - dispatch_data$Received_Date_Time) > 0)] / 60)


# Look at times by beat or description







length(unique(dispatch_data$Call_Type))
unique(dispatch_data$Description)
       
sort(table(dispatch_data$Description))

hist(as.numeric(dispatch_data$At_Scene_Time) - as.numeric(dispatch_data$Received_Time))

as.numeric(dispatch_data$At_Scene_Time[6]) - as.numeric(dispatch_data$Received_Time[6])
