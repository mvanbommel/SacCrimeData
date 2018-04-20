server <- function(input, output, session) {
  
  output$points_on_map = renderUI({
    numericInput("points_on_map", h3("Points Displayed"),
                 min=0, max=100, value = 25)
  })
  
  output$dispatch_table = DT::renderDataTable({
    dispatch_data
  })
  

  points_on_map = reactive(input$points_on_map)
  
  random_order = reactive({
    refresh = input$new_points
    
    random_order = sample(1:nrow(dispatch_data), size=nrow(dispatch_data), replace=FALSE)
  })
  
  output$dispatch_map = renderLeaflet({
    print(input$points_on_map)
    req(input$points_on_map)
    
    if (input$points_on_map == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(-121.5, 38.55, zoom=10)
    } else {
      if (input$points_on_map > 100) {
        points_on_map = 100
      } else {
        points_on_map = input$points_on_map
      }
      dispatch_subset = dispatch_data[random_order()[1:points_on_map], ]
      leaflet(data = dispatch_subset) %>% addTiles() %>%
        addMarkers(~dispatch_subset$longitude, ~dispatch_subset$latitude, popup = ~as.character(dispatch_subset$call_type_description), label = ~as.character(dispatch_subset$location))
    }
  })

}

