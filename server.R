server <- function(input, output, session) {
  
  output$dispatch_table = DT::renderDataTable({
    dispatch_data
  })
  
  points_on_map = reactive(input$points_on_map)
  
  dispatch_subset <- reactive({
    refresh = input$new_points
    
    subset_index = sample(1:nrow(dispatch_data), size=points_on_map(), replace=FALSE)
    print(subset_index)
    dispatch_subset = dispatch_data[subset_index, ]
    dispatch_subset
  })
  
  output$dispatch_map <- renderLeaflet({
    dispatch_subset = dispatch_subset()
    leaflet(data = dispatch_subset) %>% addTiles() %>%
      addMarkers(~dispatch_subset$longitude, ~dispatch_subset$lattitude, popup = ~as.character(dispatch_subset$Description), label = ~as.character(dispatch_subset$Location))
    
  })

}

