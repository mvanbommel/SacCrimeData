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
  
  # Filter the dispatch data based on the selected inputs
  filtered_dispatch_data = reactive({
    req(input$occurence_time_range)
    req(input$occurence_date_range)
    req(input$day_of_week)
    req(input$call_type_description)
    
    filtered_disptach_data = dispatch_data %>%
      filter(occurence_time > input$occurence_time_range[1] & 
             occurence_time < input$occurence_time_range[2]) %>%
      filter(occurence_date > input$occurence_date_range[1] & 
             occurence_date < input$occurence_date_range[2]) %>%
      filter(day_of_week %in% input$day_of_week) %>%
      filter(call_type_description %in% ifelse(length(input$call_type_description) == 0 | 
                                                 input$call_type_description == 'ALL', 
                                               unique(dispatch_data$call_type_description), 
                                               input$call_type_description))
  })
  
  # Compute the number of observations in the filtered data
  number_filtered_observations = reactive({
    nrow(filtered_dispatch_data())
  })

  # Determine how many points to display on the map
  points_on_map = reactive({
    req(input$points_on_map, number_filtered_observations())
    
    # If the user selects more points than are available, display the maximum 
    # number of points available
    if (input$points_on_map > number_filtered_observations()) {
      points_on_map = number_filtered_observations()
    } else {
      points_on_map = input$points_on_map
    }
  })
  
  # Create the message stating how many points are shown on the map and how many 
  # are available
  output$points_displayed_message = renderText({
    req(number_filtered_observations())
    paste0("Showing ", 
           points_on_map(), 
           " of ", 
           number_filtered_observations(), 
           " entries.")
  })
  
  # Generate a random order to be used to select a random subset of the 
  # available points
  random_order = reactive({
    refresh = input$new_points
    
    random_order = sample(1:number_filtered_observations(), 
                          size=number_filtered_observations(), 
                          replace=FALSE)
  })
  
  # Create the map
  output$dispatch_map = renderLeaflet({
    # Deafult to a blank map   
    map = leaflet() %>%
            addTiles() %>%
            setView(-121.5, 38.55, zoom=10)
    
    if (!is.null(input$call_type_description)) {
      req(input$points_on_map, number_filtered_observations())
      if (number_filtered_observations() != 0) {
        # If a description is selected and there are more than 0 filtered 
        # observations:
        
        # Generate the subset of points to display and place them on the map 
        dispatch_subset = filtered_dispatch_data()[random_order()[1:points_on_map()], ]
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

}

