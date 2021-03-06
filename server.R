server = function(input, output, session) {
  
  # UI Inputs ----
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
  
  output$api_status_button = renderUI({
    if (values$api_is_live) {
      actionButton("api_button", 
                   label = "API",
                   icon = icon("check-circle"), 
                   style = "color: #fff; 
                            background-color: #218838;
                            border-color: #1e7e34;")
    } else {
      actionButton("api_button", 
                   label = "API",
                   icon = icon("times-circle"), 
                   style = "color: #fff; 
                            background-color: #dc3545;
                            border-color: #dc3545;")
    }
  })
  
  # Start with Marker Groups Check Box enabled
  shiny::observe({
    shinyjs::enable("marker_groups_check_box")
  })
  
  # Disable Marker Groups Check Box when Markers Check Box is FALSE (enable when TRUE)
  shiny::observe({
    if (input$markers_check_box == FALSE) {
      shinyjs::disable("marker_groups_check_box")
    } else {
      shinyjs::enable("marker_groups_check_box")
    }
  })
  
  # Pop Ups ----
  observeEvent(input$api_button, {
    if (values$api_is_live) {
      sendSweetAlert(
        session = session,
        title = "API Status",
        text = "Sac Open Data API is up and running. 
        Data is up to date.",
        type = "success"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "API Status",
        text = "Sac Open Data API is down. 
        Backup data for January 2019 has been loaded into the app.",
        type = "error"
      )
    }
  })
  
  observeEvent(input$help, {
    sendSweetAlert(
      session = session,
      title = "Help",
      text = "This app displays Sacramento Police Dispatch data for the current year from data.cityofsacramento.org. 

      The most recent day of data available is displayed on load.      

      Use the sidebar to filter the results displayed on the map. The sidebar can be opened and closed using the button in the header. 

      You can also filter the data by area using the rectangle button in the top right (below the zoom buttons).

      Click on a point for more information.",
      type = "info"
    )
  })
  
  # Warnings ----
  observe({
    if (values$filtered_observation_rows > 999) {
      showNotification(paste("Warning:\nOnly the first 1000 points meeting filter criteria displayed. Use New Points button to view the next 1000."), 
                       type = 'error', 
                       duration = 5)
    }
  })
  
  observe({
    if (values$filtered_observation_rows == 0) {
      showNotification(paste("Warning:\nNo observations meet the filter criteria."), 
                       type = 'error', 
                       duration = 5)
    }
  })
  
  # Reactive Values ----
  values = reactiveValues(api_is_live = api_is_live,
                          filtered_observation_rows = 1,
                          center_longitude = -121.5,
                          center_latitude = 38.55, 
                          zoom = 11)
  
  # * Save Map Center / Zoom ----
  observeEvent({ 
    input$occurence_date_range
    input$day_of_week
    input$description_groups
    input$call_type_description
    input$markers_check_box
    input$marker_groups_check_box
    input$heatmap_check_box
    input$dispatch_map_draw_new_feature
    input$clear_rectangle
  }, { 
    if (!is.null(input$dispatch_map_zoom)) {
      values$center_latitude = input$dispatch_map_center$lat
      values$center_longitude = input$dispatch_map_center$lng
      values$zoom = input$dispatch_map_zoom
    }
  })
  
  
  # Data ----
  # * Create Query ----
  dispatch_data_query_filter = reactive({
    req(input$occurence_date_range)
    req(input$call_type_description)
    
    query = paste0("Occurence_Date >= date'", input$occurence_date_range[1], 
                   "' AND Occurence_Date <= date'", input$occurence_date_range[2], "' ")

    if (length(input$day_of_week) == 0) {
      day_of_week_filter = " AND (Day_of_Week = 'NULL')"
      query = paste0(query, day_of_week_filter)
    }
    else if (length(input$day_of_week)  != 7) {
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
    req(input$occurence_date_range)
    req(input$call_type_description)
    req(dispatch_data_query_filter())

    url = "https://services5.arcgis.com/54falWtcpty3V47Z/ArcGIS/rest/services/cad_calls_year3/FeatureServer/0"
    where = dispatch_data_query_filter()
    limit = 1000

    if (values$api_is_live) {
      dispatch_data = try(esri2sf(url, where = where, limit = limit) %>%
        as.data.frame())
      if ('try-error' %in% class(dispatch_data)) {
        # Sometimes a HTTP2 errors occurs, if so try again
        dispatch_data = try(esri2sf(url, where = where, limit = limit) %>%
                              as.data.frame())
      }
      
      if ('try-error' %in% class(dispatch_data)) {
        values$api_is_live = FALSE
      }
    }

    if (values$api_is_live == FALSE) {
      sqldf_query = paste0("SELECT 
                            * 
                            FROM backup_dispatch_data 
                            WHERE 1 = 1
                            AND ", where, "
                            LIMIT 1000
                           ")
      # Remove the 'date' casts from the query
      sqldf_query = gsub(pattern = 'date', 
                         replacement = '',
                         x = sqldf_query)
      dispatch_data = sqldf(sqldf_query)
      
      values$filtered_observation_rows = nrow(dispatch_data)
    } else {
      values$filtered_observation_rows = nrow(dispatch_data)
      dispatch_data = clean_dispatch_data(dispatch_data)
    }

    return(rename_dispatch_data(dispatch_data))
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
  
  # * Map Output ----
  output$dispatch_map = renderLeaflet({
    # Deafult to a blank map   
    map = leaflet() %>%
            addTiles() %>%
            setView(lat = values$center_latitude, 
                    lng = values$center_longitude, 
                    zoom = values$zoom)
    
    if (!is.null(input$call_type_description) & 
        (input$markers_check_box | input$heatmap_check_box)) {
      req(number_map_filtered_observations())
      
      if (number_map_filtered_observations() != 0) {
        # If a description is selected and there are more than 0 filtered 
        # observations:
        
        # Generate the subset of points to display and place them on the map 
        dispatch_subset = map_filtered_dispatch_data()
        
        map = leaflet(data = dispatch_subset) %>% 
          addTiles() %>%
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
        
        if (input$markers_check_box) {
          # Include the call type description as a pop-up and the location 
          # as a label
          if (input$marker_groups_check_box) {
            map = map %>% 
              addMarkers(~dispatch_subset$longitude, 
                         ~dispatch_subset$latitude, 
                         popup = ~paste0('Address:<br>', dispatch_subset$location, '<br>',
                                         '<br>Occurence Date:<br>', dispatch_subset$occurence_date, '<br>',
                                         '<br>Call Type:<br>', dispatch_subset$call_type_description), 
                         label = ~as.character(dispatch_subset$location),
                         clusterOptions = markerClusterOptions())
          } else {
            map = map %>% 
              addMarkers(~dispatch_subset$longitude, 
                         ~dispatch_subset$latitude, 
                         popup = ~paste0('Address:<br>', dispatch_subset$location, '<br>',
                                         '<br>Occurence Date:<br>', dispatch_subset$occurence_date, '<br>',
                                         '<br>Call Type:<br>', dispatch_subset$call_type_description), 
                         label = ~as.character(dispatch_subset$location,
                         group = 'selected_rectangle'))
          }
        }
        
        if (input$heatmap_check_box) {
          map = map %>% 
            addHeatmap(~dispatch_subset$longitude,
                       ~dispatch_subset$latitude,
                       radius = 10) 
        }
        
        # ** Add Rectangle ----
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

          map = map %>%
            addRectangles(
              data = map_filtered_dispatch_data(),
              lng1 = min_longitude, lat1 = min_latitude,
              lng2 = max_longitude, lat2 = max_latitude,
              fillColor = "transparent",
              layerId = 'selected_rectangle')
        }
        
        map = map %>%
          setView(lat = values$center_latitude, 
                  lng = values$center_longitude, 
                  zoom = values$zoom)
      }
    }
  
    return(map)
  })
  
  # * Clear Rectangle ----
  observeEvent(input$clear_rectangle, {
    if (input$clear_rectangle == 'TRUE') {
      # Set inputs (passed as messages) to NULL using the resetInput javascript function
      session$sendCustomMessage(type = "resetInput", message = "dispatch_map_draw_new_feature")
      session$sendCustomMessage(type = "resetInput", message = "clear_rectangle")
    }
  })
  
}

