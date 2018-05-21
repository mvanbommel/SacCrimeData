server <- function(input, output, session) {
  
  # UI Inputs ----
  # Input for selecting the number of points to display on the map
  output$points_on_map = renderUI({
    numericInput("points_on_map", h3("Points Displayed"),
                 min=0, max=100, value = 25)
  })
  
  output$call_type_description = renderUI({
    
    choice_list = sort(all_descriptions[unique(unlist(lapply(input$description_groups, 
                                                             grep, 
                                                             all_descriptions)))])
    
    if ("OTHER" %in% input$description_groups) {
      choice_list = sort(append(choice_list, other_crime_descriptions))
    }
    
    pickerInput("call_type_description", h3("Call Type Description"), 
              choices = choice_list,
              selected = choice_list,
              options = list(`selected-text-format` = "count > 1",
                             `actions-box` = TRUE,
                             `live-search` = TRUE), 
              multiple = TRUE)
  })
  
  
  # Table ----
  
  # Table in the table pane
  output$dispatch_table = DT::renderDataTable({
    filtered_dispatch_data()
  })
  
  
  # Map ----
  
  # * Filtering the Data ----
  # ** Inputs ----
  filtered_dispatch_data = reactive({
    req(input$occurence_time_range)
    req(input$occurence_date_range)
    req(input$day_of_week)
    req(input$call_type_description)
    
    # Always filter occurence date and call type description
    filtered_dispatch_data = dispatch_data %>%
      filter(occurence_date > input$occurence_date_range[1] & 
               occurence_date < input$occurence_date_range[2]) %>%
      filter(call_type_description %in% ifelse(length(input$call_type_description) == 0 | 
                                                 input$call_type_description == 'ALL', 
                                               unique(dispatch_data$call_type_description), 
                                               input$call_type_description))
    
    
    # Only filter occurence times and day of the week if the user changes the default
    if (!all.equal(input$occurence_time_range, c(0, 24))) {
      filtered_dispatch_data = filtered_dispatch_data %>%
        filter(occurence_time > input$occurence_time_range[1] & 
                 occurence_time < input$occurence_time_range[2])
    }
    if (!all.equal(input$day_of_week, c('Sunday', 
                                        'Monday', 
                                        'Tuesday', 
                                        'Wednesday', 
                                        'Thursday', 
                                        'Friday', 
                                        'Saturday'))) {
      filtered_dispatch_data = filtered_dispatch_data %>%
        filter(day_of_week %in% input$day_of_week)
    }
    
    filtered_dispatch_data
  })
  
  # Compute the number of observations in the filtered data
  number_filtered_observations = reactive({
    nrow(filtered_dispatch_data())
  })
  
  # ** Map ----
  map_filtered_dispatch_data = reactive({
    missing_latitude_index = which(is.na(filtered_dispatch_data()$latitude))
    missing_longitude_index = which(is.na(filtered_dispatch_data()$longitude))
    
    any_data_missing = Reduce(union, list(missing_latitude_index, 
                                          missing_longitude_index))
    
    if (length(any_data_missing) > 0) {
      map_filtered_dispatch_data = filtered_dispatch_data()[-any_data_missing, ]
    } else {
      map_filtered_dispatch_data = filtered_dispatch_data()
    }
    
    map_filtered_dispatch_data
  })
  
  # Compute the number of observations in the map filtered data
  number_map_filtered_observations = reactive({
    nrow(map_filtered_dispatch_data())
  })
  
  # ** Times ----
  time_filtered_dispatch_data = reactive({
    req(input$time_range)

    time_choices = c("Occurence", "Received", "Dispatch", 
                     "Enroute", "At Scene", "Clear")
    time_values = c("occurence", "received", "dispatch",
                    "enroute", "at_scene", "clear")
    
    start_time_column = paste0(time_values[which(time_choices == input$time_range[1])], 
                               "_date_time")
    end_time_column = paste0(time_values[which(time_choices == input$time_range[2])], 
                             "_date_time")
    
    missing_start_time_index = which(is.na(filtered_dispatch_data()[, start_time_column]))
    missing_end_time_index = which(is.na(filtered_dispatch_data()[, end_time_column]))
    
    any_data_missing = Reduce(union, list(missing_start_time_index, 
                                          missing_end_time_index))
    
    if (length(any_data_missing) > 0) {
      time_filtered_dispatch_data = filtered_dispatch_data()[-any_data_missing, ]
    } else {
      time_filtered_dispatch_data = filtered_dispatch_data()
    }
    
    time_filtered_dispatch_data
  })
  
  # Compute the number of observations in the time filtered data
  number_time_filtered_observations = reactive({
    nrow(time_filtered_dispatch_data())
  })
  
  
  # Determine how many points to display on the map
  points_on_map = reactive({
    req(input$points_on_map, number_map_filtered_observations())
    
    # If the user selects more points than are available, display the maximum 
    # number of points available
    if (input$points_on_map > number_map_filtered_observations()) {
      points_on_map = number_map_filtered_observations()
    } else {
      points_on_map = input$points_on_map
    }
  })
  
  # Create the message stating how many points are shown on the map and how many 
  # are available
  output$points_displayed_message = renderUI({
    req(number_filtered_observations())
    HTML(paste0("Showing ", 
                 points_on_map(), 
                 " of ", 
                 number_map_filtered_observations(), 
                 " entries with location data. <br/>",
                 number_filtered_observations() - number_map_filtered_observations(),
                 " additional entries are missing location data. <br/>",
                 number_total_observations - number_filtered_observations(),
                 " additional observations do not meet the selected requirements."))
  })
  
  # Generate a random order to be used to select a random subset of the 
  # available points
  random_order = reactive({
    refresh = input$new_points
    
    random_order = sample(1:number_filtered_observations(), 
                          size=number_filtered_observations(), 
                          replace=FALSE)
  })
  
  # * Map Output ----
  output$dispatch_map = renderLeaflet({
    # Deafult to a blank map   
    map = leaflet() %>%
            addTiles() %>%
            setView(-121.5, 38.55, zoom=10)
    
    if (!is.null(input$call_type_description)) {
      req(input$points_on_map, number_map_filtered_observations())
      if (number_map_filtered_observations() != 0) {
        # If a description is selected and there are more than 0 filtered 
        # observations:
        
        # Generate the subset of points to display and place them on the map 
        dispatch_subset = map_filtered_dispatch_data()[random_order()[1:points_on_map()], ]
        map = leaflet(data = dispatch_subset) %>% addTiles() %>%
                # Include the call type description as a pop-up and the location 
                # as a label
                addMarkers(~dispatch_subset$longitude, 
                           ~dispatch_subset$latitude, 
                           popup = ~as.character(dispatch_subset$call_type_description), 
                           label = ~as.character(dispatch_subset$location))
      }
    }
  
    map
  })

  
  # * Time Distribution Output ----
  # Initialize values for the saved time distributions
  reactive_values = reactiveValues(saved_time_distributions = list(), 
                                   saved_time_distribution_number = 1)
  
  # Get the distribution for the currently selected input values
  new_time_distribution = reactive({
    req(filtered_dispatch_data())
    
    time_choices = c("Occurence", "Received", "Dispatch", 
                     "Enroute", "At Scene", "Clear")
    time_values = c("occurence", "received", "dispatch",
                    "enroute", "at_scene", "clear")
    
    start_time_column = paste0(time_values[which(time_choices == input$time_range[1])], 
                               "_date_time")
    end_time_column = paste0(time_values[which(time_choices == input$time_range[2])], 
                             "_date_time")
    
    times = as.numeric(difftime(time_filtered_dispatch_data()[, end_time_column], 
                                time_filtered_dispatch_data()[, start_time_column],
                                units = "mins"))
    
    # Use the distribution name input as the label in the legend
    distribution = data.frame(time = times, line = input$new_time_distribution_name)
    
    distribution
  })
  
  # Save the current distributions (adds to any previously saved distributions)
  observeEvent(input$save_new_time_distribution, {
    reactive_values$saved_time_distributions[[reactive_values$saved_time_distribution_number]] = new_time_distribution()
    reactive_values$saved_time_distribution_number = reactive_values$saved_time_distribution_number + 1
  })
  
  # Reset the saved distributions
  observeEvent(input$reset_time_distribution_plot, {
    reactive_values$saved_time_distributions = list()
    reactive_values$saved_time_distribution_number = 1
  })
  
  output$time_distribution = renderPlot({
    req(filtered_dispatch_data())
   
    saved_time_distributions = reactive_values$saved_time_distributions
    
    if (length(saved_time_distributions) == 0) {
      # If no distributions are saved:
      if(input$plot_current_distribution) {
        # Show distribution from current inputs
        saved_time_distributions = list(new_time_distribution())
      } else {
        # Shot no plot
        saved_time_distributions = list()
      }
    } else {
      if(input$plot_current_distribution) {
        # Also show distribution from current inputs
        saved_time_distributions[[reactive_values$saved_time_distribution_number]] = new_time_distribution()
      } 
    }
   
    plot = ggplot()
    if (length(saved_time_distributions) > 0) {
      for (i in 1:length(saved_time_distributions)) {
        data = saved_time_distributions[[i]]
        
        # Filter data based on the selected time range
        plotted_data = data[which(data$time > input$time_distribution_plot_minimum_x & 
                                  data$time < input$time_distribution_plot_maximum_x), ] 
        
        plot = plot + 
          geom_density(data = plotted_data, aes(x=time, fill=line), alpha=.3)
      }
    }

    plot
  })
  
}

