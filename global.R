library(DT)

dispatch_data = read.csv('Sacramento_Dispatch_Data_From_Current_Year.csv')

received_time = as.POSIXct(paste0(substr(dispatch_data$Received_Date[1], 1, 10), ' ', dispatch_data$Received_Time[1]))
at_scene_time = as.POSIXct(paste0(substr(dispatch_data$At_Scene_Date[1], 1, 10), ' ', dispatch_data$At_Scene_Time[1]))


available_data_index = function(column, type='time') {
  if (type == 'time') {
    which(gsub("^\\s+|\\s+$", "", as.character(column)) != '1899-01-01')
  } else if (type == 'date') {
    which(!(gsub("^\\s+|\\s+$", "", substr(as.character(column), 1, 10)) %in% c('', '1899-01-01')))
  }
}





length(unique(dispatch_data$Call_Type))
unique(dispatch_data$Description)
       
sort(table(dispatch_data$Description))

hist(as.numeric(dispatch_data$At_Scene_Time) - as.numeric(dispatch_data$Received_Time))

as.numeric(dispatch_data$At_Scene_Time[6]) - as.numeric(dispatch_data$Received_Time[6])
