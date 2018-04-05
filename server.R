server <- function(input, output, session) {
  
  output$dispatch_data = DT::renderDataTable({
    dispatch_data
  })

}

