ui <- fluidPage(
  
  # App title ----
  titlePanel("Sacramento Dispatch Data"),
  
  DT::dataTableOutput("dispatch_data")
  
)