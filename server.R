server = function(input, output, session) {
  
  # UI Inputs ----
  # Input for selecting the number of points to display on the map
  output$points_on_map = renderUI({
    numericInput("points_on_map", 
                 label = h3("Points Displayed"),
                 min = 0, 
                 max = 1000, 
                 value = 25)
  })
  
  output$call_type_description = renderUI({
    choice_list = sort(all_descriptions[unique(unlist(lapply(input$description_groups, 
                                                             grep, 
                                                             all_descriptions)))])
    
    if ("OTHER" %in% input$description_groups) {
      choice_list = sort(append(choice_list, other_crime_descriptions))
    }
    
    # Filter out duplicate descriptions
    choice_list = unique(choice_list)
    
    pickerInput("call_type_description", 
                label = h3("Call Type Description"), 
                choices = choice_list,
                selected = choice_list,
                multiple = TRUE,
                options = list(`selected-text-format` = "count > 1",
                               `actions-box` = TRUE,
                               `live-search` = TRUE,
                               `size` = 15), 
                choicesOpt = list(
                  content = gsub(pattern = '-', replace = '<br>', x = choice_list)
                ))
  })
  
  
  # Reactive Values ----
  # * Results Offset ----
  values = reactiveValues(results_offset = 0)
  
  observeEvent(input$new_points, {
    values$results_offset = values$results_offset + input$points_on_map
  })
  observeEvent(input$occurence_date_range, {
    values$results_offset = 0
  })
  observeEvent(input$day_of_week, {
    values$results_offset = 0
  })
  observeEvent(input$call_type_description, {
    values$results_offset = 0
  })
  
  # Data ----
  # * Create Query ----
  dispatch_data_query_filter = reactive({
    req(input$occurence_date_range)
    req(input$day_of_week)
    req(input$call_type_description)
    
    query = paste0("Occurence_Date >= date'", input$occurence_date_range[1], 
                   "' AND Occurence_Date <= date'", input$occurence_date_range[2], "' ")
    
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
    
    if (length(input$dispatch_map_draw_new_feature) > 0) {

      boundaries = as.data.frame(matrix(unlist(input$dispatch_map_draw_new_feature$geometry$coordinates),
                                        ncol = 2, 
                                        byrow = TRUE),
                                 stringsAsFactors = FALSE)
      colnames(boundaries) = c('longitude', 'latitude')
      
      min_longitude = min(boundaries$longitude)
      max_longitude = max(boundaries$longitude)
      min_latitude = min(boundaries$latitude)
      max_latitude = max(boundaries$latitude)
      
      # Convert longitude and latitude to X and Y for query
      min_X_Coordinate = predict(longitude_model, newdata = data.frame(longitude = min_longitude))
      max_X_Coordinate = predict(longitude_model, newdata = data.frame(longitude = max_longitude))
      min_Y_Coordinate = predict(latitude_model, newdata = data.frame(latitude = min_latitude))
      max_Y_Coordinate = predict(latitude_model, newdata = data.frame(latitude = max_latitude))
      
      shape_filter = paste0(" AND X_Coordinate >= ", min_X_Coordinate,
                            " AND X_Coordinate <= ", max_X_Coordinate,
                            " AND Y_Coordinate >= ", min_Y_Coordinate,
                            " AND Y_Coordinate <= ", max_Y_Coordinate,
                            " ")
      
      query = paste0(query, shape_filter)
      
    }
    
    return(query)
  })
  
  # * Pull Data ----
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

    if (api_is_live) {
      dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
        as.data.frame())
      
      if ('try-error' %in% class(dispatch_data)) {
        # Sometimes a HTTP2 errors occurs, if so try again
        dispatch_data = try(esri2sf(url, where = where, limit = limit, offset = offset) %>%
                              as.data.frame())
      }
      
      if ('try-error' %in% class(dispatch_data)) {
        api_is_live = FALSE
      }
    }

    if (api_is_live == FALSE) {
      sqldf_query = paste0("SELECT 
                            * 
                            FROM backup_dispatch_data 
                            WHERE 1 = 1
                            AND ", where, "
                            LIMIT ", limit, "
                            OFFSET ", offset)
      # Remove the 'date' casts from the query
      sqldf_query = gsub(pattern = 'date', 
                         replacement = '',
                         x = sqldf_query)
      dispatch_data = sqldf(sqldf_query)
    } else {
      dispatch_data = clean_dispatch_data(dispatch_data)
    }

    return(rename_dispatch_data(dispatch_data))
  })
  
  # Table ----
  # Table in the table pane
  output$dispatch_table = DT::renderDataTable({
    table_data = filtered_dispatch_data() %>%
      select('Occurence Date' = occurence_date,
             'Day' = day_of_week,
             'Call Type' = call_type_description,
             'Report Created' = report_created,
             'Address' = location,
             'Longitude' = longitude,
             'Latitude' = latitude,
             'Officer ID' = reporting_officer_id,
             'Unit ID' = unit_id,
             'Police District' = police_district,
             'Police Beat' = police_beat)
  })
  
  
  # Map ----
  # Compute the number of observations in the filtered data
  number_filtered_observations = reactive({
    nrow(filtered_dispatch_data())
  })
  
  # * Filter Data ----
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
    missing_location_point_count = input$points_on_map - points_on_map()
    HTML(paste0("Showing ", 
                 points_on_map(), 
                 " of ", 
                 number_total_observations, 
                 " total entries.",
                case_when(
                  missing_location_point_count == 0 ~ "",
                  missing_location_point_count == 1 ~ "<br>(1 point missing location data).",
                  missing_location_point_count > 1 ~ paste0("<br>(",
                                                            missing_location_point_count, 
                                                            " points missing location data).")
                  )
    ))
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
        dispatch_subset = map_filtered_dispatch_data()

        map = leaflet(data = dispatch_subset) %>% 
          addTiles() %>%
          # Include the call type description as a pop-up and the location 
          # as a label
          addCircleMarkers(~dispatch_subset$longitude, 
                           ~dispatch_subset$latitude, 
                           radius = 4,
                           color = '#337ab7',
                           popup = ~paste0('Address:<br>', dispatch_subset$location, '<br>',
                                           '<br>Occurence Date:<br>', dispatch_subset$occurence_date, '<br>',
                                           '<br>Call Type:<br>', dispatch_subset$call_type_description), 
                           label = ~as.character(dispatch_subset$location),
                           stroke = TRUE) %>%
          addDrawToolbar(
            targetGroup = 'Selected',
            polylineOptions = FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                                  color = 'white',
                                                                                  weight = 3)),
            polygonOptions = FALSE,
            circleOptions = FALSE,
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
      }
    }
  
    return(map)
  })
  
}

