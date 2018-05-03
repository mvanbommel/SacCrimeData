library(shiny)

shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    
    # Map Tab ----
    tabPanel("Map", id="map_tab",
      fluidRow(
        leafletOutput("dispatch_map"),
        textOutput("points_displayed_message"),
        actionButton("new_points", "New points")
      ),
      fluidRow(
        uiOutput("points_on_map"),
        
        sliderInput("occurence_time_range", h3("Occurence Time"),
                    min=0, max=24, value=c(0, 24),
                    post=':00'),
        
        # NULL end defaults to today's date
        dateRangeInput("occurence_date_range", h3("Occurence Date"),
                       start=min(dispatch_data$occurence_date), end=NULL),
        
        checkboxGroupInput("day_of_week", h3("Day of the Week"),
                           choiceNames = list('S', 'M', 'T', 'W', 'T', 'F', 'S'),
                           choiceValues = list('Sunday', 
                                               'Monday', 
                                               'Tuesday', 
                                               'Wednesday',
                                               'Thursday', 
                                               'Friday', 
                                               'Saturday'),
                           selected = list('Sunday', 
                                           'Monday', 
                                           'Tuesday', 
                                           'Wednesday',
                                           'Thursday', 
                                           'Friday', 
                                           'Saturday'),
                           inline = TRUE),
        
        selectInput("call_type_description", h3("Call Type Description"), 
                    choices = c('ALL', sort(unique(dispatch_data$call_type_description))),
                    selected = 'ALL',
                    multiple = TRUE,
                    selectize = TRUE)

      )
    ),
    
    # Table Tab ----
    tabPanel("Table", id="table_tab",
      fluidRow(
       DT::dataTableOutput("dispatch_table")
      )
    )
  )
)