server <- function(input, output, session) {
  
  # Input for selecting the number of points to display on the map
  output$points_on_map = renderUI({
    numericInput("points_on_map", h3("Points Displayed"),
                 min=0, max=100, value = 25)
  })
  
  # Table in the table pane
  output$dispatch_table = DT::renderDataTable({
    filtered_dispatch_data()
  })
  
  # Filter the dispatch data based on the selected inputs
  filtered_dispatch_data = reactive({
    filtered_disptach_data = dispatch_data %>%
      filter(occurence_time > input$occurence_time_range[1] & occurence_time < input$occurence_time_range[2])
  })
  
  # Compute the number of observations in the filtered data
  number_filtered_observations = reactive({
    nrow(filtered_dispatch_data())
  })

  # Determine how many points to display on the map
  points_on_map = reactive({
    req(input$points_on_map, number_filtered_observations())
    
    # If the user selects more points than are available, display the maximum number of points available
    if (input$points_on_map > number_filtered_observations()) {
      points_on_map = number_filtered_observations()
    } else {
      points_on_map = input$points_on_map
    }
  })
  
  # Create the message stating how many points are shown on the map and how many are available
  output$points_displayed_message = renderText({
    req(number_filtered_observations())
    paste0("Showing ", points_on_map(), " of ", number_filtered_observations(), " entries.")
  })
  
  # Generate a random order to be used to select a random subset of the available points
  random_order = reactive({
    refresh = input$new_points
    
    random_order = sample(1:number_filtered_observations(), size=number_filtered_observations(), replace=FALSE)
  })
  
  # Create the map
  output$dispatch_map = renderLeaflet({
    req(input$points_on_map, number_filtered_observations())
    
    # If the selects to display 0 points or there are no points available, show an empty map
    if (input$points_on_map == 0 | number_filtered_observations() == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(-121.5, 38.55, zoom=10)
    } else {
      # Generate the subset of points to display and place them on the map 
      dispatch_subset = filtered_dispatch_data()[random_order()[1:points_on_map()], ]
      leaflet(data = dispatch_subset) %>% addTiles() %>%
        # Include the call type description as a pop-up and the location as a label
        addMarkers(~dispatch_subset$longitude, ~dispatch_subset$latitude, popup = ~as.character(dispatch_subset$call_type_description), label = ~as.character(dispatch_subset$location))
    }
  })

}

