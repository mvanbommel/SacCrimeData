server <- function(input, output, session) {
  
  output$dispatch_table = DT::renderDataTable({
    dispatch_data
  })
  
  dispatch_subset <- eventReactive(input$new_points, {
    subset_index = sample(1:nrow(dispatch_data), size=25, replace=FALSE)
    print(subset_index)
    dispatch_subset = dispatch_data[subset_index, ]
    dispatch_subset
  }, ignoreNULL = FALSE)
  
  output$dispatch_map <- renderLeaflet({
    dispatch_subset = dispatch_subset()
    leaflet(data = dispatch_subset) %>% addTiles() %>%
      addMarkers(~dispatch_subset$longitude, ~dispatch_subset$lattitude, popup = ~as.character(dispatch_subset$Description), label = ~as.character(dispatch_subset$Location))
    
  })

}

