server = function(input, output, session) {
  
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
  
  
  # Reactive Values ----
  values = reactiveValues(results_offset = 0)
  
  observeEvent(input$new_points, {
    values$results_offset = values$results_offset + input$points_on_map
  })
  
  reset_results_offset_1 = observeEvent(input$occurence_date_range, {
    values$results_offset = 0
  })
  reset_results_offset_1 = observeEvent(input$day_of_week, {
    values$results_offset = 0
  })
  reset_results_offset_1 = observeEvent(input$call_type_description, {
    values$results_offset = 0
  })
  
  # Data ----
  dispatch_data_query_filter = reactive({
    req(input$occurence_date_range)
    req(input$day_of_week)
    req(input$call_type_description)
    
    query = paste0("Occurence_Date > date'", input$occurence_date_range[1], 
                   "' AND Occurence_Date < date'", input$occurence_date_range[2], "' ")
    
    if (!(length(input$day_of_week) %in% c(0, 7))) {
      day_of_week_filter = paste0(" AND (Day_of_Week = '", 
                                  paste0(input$day_of_week, 
                                         collapse = "' OR Day_of_Week = '"),
                                  "') ")
      query = paste0(query, day_of_week_filter)
    }
    
    if (!(length(input$call_type_description) %in% c(0, number_total_call_type_descriptions))) {
      call_type_description_filter = paste0(" AND (Description = '",
                                            paste(input$call_type_description,
                                                  collapse = "' OR Description = '"),
                                            "') ")
      
      query = paste0(query, call_type_description_filter)
    }
    
    query
    
  })
  
  filtered_dispatch_data = reactive({
    req(input$points_on_map)
    req(input$occurence_date_range)
    req(input$day_of_week)
    req(input$call_type_description)
    
    refresh = input$new_points

    url = "https://services5.arcgis.com/54falWtcpty3V47Z/ArcGIS/rest/services/cad_calls_year3/FeatureServer/0"
    where = dispatch_data_query_filter()
    limit = input$points_on_map
    offset = values$results_offset
    
    dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
      as.data.frame())
    
    if ('try-error' %in% class(dispatch_data)) {
      # Sometimes a HTTP2 errors occurs, if so try again
      dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
                            as.data.frame())
    }
    
    clean_dispatch_data(dispatch_data)
  })
  
  # Table ----
  # Table in the table pane
  output$dispatch_table = DT::renderDataTable({
    filtered_dispatch_data()
  })
  
  
  # Map ----
  # * Variables ----
  # List to store selected map points
  map_reactive_values = reactiveValues(selected_points = list())
  
  # Compute the number of observations in the filtered data
  number_filtered_observations = reactive({
    nrow(filtered_dispatch_data())
  })
  
  # ** Map ----
  map_filtered_dispatch_data = reactive({
    if (nrow(filtered_dispatch_data()) == 0) {
      filtered_dispatch_data()
    } else {
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
    }
  })
  
  # Compute the number of observations in the map filtered data
  number_map_filtered_observations = reactive({
    nrow(map_filtered_dispatch_data())
  })
  
  # ** Shapes ----
  # Filter in drawn shapes, if a drawn shape exists
  shape_filtered_dispatch_data = reactive({
    shape_filtered_dispatch_data = map_filtered_dispatch_data()
    if (length(input$dispatch_map_draw_new_feature) > 0) {
     
      filtered_dispatch_data_coordinates = SpatialPointsDataFrame(shape_filtered_dispatch_data[,c('longitude', 
                                                                                                  'latitude')], 
                                                                  shape_filtered_dispatch_data[, c('latitude', 
                                                                                                   'longitude', 
                                                                                                   'id', 
                                                                                                   'selected_id')])
      map_reactive_values$selected_points = findLocations(shape = input$dispatch_map_draw_new_feature,
                                                          location_coordinates = filtered_dispatch_data_coordinates,
                                                          location_id_colname = "id")
      
      # look up points by ids found
      shape_filtered_dispatch_data = subset(shape_filtered_dispatch_data, id %in% map_reactive_values$selected_points)
    }
  
    shape_filtered_dispatch_data
  })
  
  # Compute the number of observations in the shape filtered data
  number_shape_filtered_observations = reactive({
    nrow(shape_filtered_dispatch_data())
  })
  
  
  # Determine how many points to display on the map
  points_on_map = reactive({
    req(input$points_on_map, number_shape_filtered_observations())
    
    # If the user selects more points than are available, display the maximum 
    # number of points available
    if (input$points_on_map > number_shape_filtered_observations()) {
      points_on_map = number_shape_filtered_observations()
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
                 number_total_observations, 
                 " total entries."))
  })

  
  # * Map Output ----
  # ** Default Map ----
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
        dispatch_subset = shape_filtered_dispatch_data()

        map = leaflet(data = dispatch_subset) %>% 
          addTiles() %>%
          # Include the call type description as a pop-up and the location 
          # as a label
          addCircles(~dispatch_subset$longitude, 
                     ~dispatch_subset$latitude, 
                     popup = ~paste0('Occurence Date:<br>', dispatch_subset$occurence_date, '<br>',
                                     '<br> Call Type:<br>', dispatch_subset$call_type_description), 
                     label = ~as.character(dispatch_subset$location),
                     stroke = TRUE,
                     layerId = as.character(dispatch_subset$id),
                     highlightOptions = highlightOptions(color = "mediumseagreen",
                                                         bringToFront = TRUE)) %>%
          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                              color = 'white',
                                                                              weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                                  color = 'white',
                                                                                  weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                              color = 'white',
                                                                              weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
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
    distribution = data.frame(time = times, line = paste0("Line ", line_number$number))
    
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
    data = NULL
    if (length(saved_time_distributions) > 0) {
      # Update the line_number reactive value for the new_time_distribution_name input
      line_number$number = length(saved_time_distributions) 
      
      for (i in 1:length(saved_time_distributions)) {
        data = rbind(data, saved_time_distributions[[i]])
      }
      
      # Filter data based on the selected time range
      plotted_data = data[which(data$time >= input$time_distribution_plot_minimum_x & 
                                data$time <= input$time_distribution_plot_maximum_x), ] 
      
      if (input$time_distribution_plot_type == 'Frequency') {
        plot = plot +
          geom_density(data = plotted_data, aes(x=time, y=..count.., fill=line), alpha=0.3) + 
          labs(x = "Time (Minutes)", y = 'Frequency', fill = 'Legend') +
          theme_bw()
      } else {
        plot = plot +
          geom_density(data = plotted_data, aes(x=time, fill=line), alpha=0.3) + 
          labs(x = "Time (Minutes)", y = 'Density', fill = 'Legend') +
          theme_bw()
      }
    
      if (length(saved_time_distributions) > 0) {
        plot = plot + 
          scale_fill_manual(values = time_distribution_colors[1:length(saved_time_distributions)])
      }
    }

    plot
  })
  
}

