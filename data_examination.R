# Load Data ----
dispatch_data = read.csv('Sacramento_Dispatch_Data_From_Current_Year.csv', stringsAsFactors = FALSE)
colnames(dispatch_data)[1] = 'X'


# Examine Descriptions ----
sort(table(unlist(strsplit(unique(dispatch_data$Description), "\\s+|\\-|\\/"))), decreasing = TRUE)

sort(unique(dispatch_data$Description))


description_categories = c('ALARM', 
                           'ARSON', 
                           'ASSAULT', 
                           'BAIT', 
                           'BATTERY', 
                           'BRANDISHING', 
                           'BURGLARY', 
                           'CAR CLOUT', 
                           'CARJACKING', 
                           'CHILD ABUSE/NEGLECT', 
                           'DEAD BODY', 
                           'DIRECTED PATROL', 
                           'DISTURBANCE', 
                           'DOMESTIC VIOLENCE', 
                           'DRUNK', 
                           'FELONY',
                           'HIT & RUN', 
                           'HOMICIDE', 
                           'INCOMPLETE CALL', 
                           'INJURIES', 
                           'KIDNAPPING', 
                           'LOCAL GOVERNMENT', 
                           'MEDICAL AID', 
                           'MISDEMEANOR', 
                           'NARCOTIC', 
                           'PRISONER IN CUSTODY', 
                           'RAPE', 
                           'ROBBERY', 
                           'SEX CRIMES', 
                           'SHOOTING', 
                           'STOLEN PROPERTY', 
                           'STOLEN VEHICLE', 
                           'SUSPICIOUS', 
                           'THEFT', 
                           'TRAFFIC', 
                           'VANDALISM', 
                           'VEHICLE ACCIDENT')
description_times = c('CSI', 
                      'IN PROGRESS', 
                      'LESS THAN 5 AGO', 
                      'LESS THAN 15 AGO', 
                      'REPORT')





count_occurrences = function(string, vector) {
  length(grep(string, vector))
}


category_counts = lapply(description_categories, count_occurrences, vector=unique(dispatch_data$Description))
cbind(description_categories, unlist(category_counts))
