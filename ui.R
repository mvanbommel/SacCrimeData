shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    
    # Map Tab ----
    tabPanel("Map", id="map_tab",
      fluidRow(
        leafletOutput("dispatch_map"),
        htmlOutput("points_displayed_message"),
        actionButton("new_points", "New points")
      ),
      
      fluidRow(
        uiOutput("points_on_map"),
        
        # NULL end defaults to today's date
        dateRangeInput("occurence_date_range", h3("Occurence Date"),
                       start="2019-01-01", end=NULL),
        
        checkboxGroupInput("day_of_week", h3("Day of the Week"),
                           choiceNames = list('S', 'M', 'T', 'W', 'T', 'F', 'S'),
                           choiceValues = list('Sun',
                                               'Mon',
                                               'Tue',
                                               'Wed',
                                               'Thu',
                                               'Fri',
                                               'Sat'),
                           selected = list('Sun',
                                           'Mon',
                                           'Tue',
                                           'Wed',
                                           'Thu',
                                           'Fri',
                                           'Sat'),
                           inline = TRUE),
        
        pickerInput("description_groups", h3("Call Type Description Groups"), 
                    choices = list(Times = description_time_groups,
                                   Crimes = description_crime_groups), 
                    selected = description_groups,
                    options = list(`selected-text-format` = "count > 1",
                                   `actions-box` = TRUE,
                                   `live-search` = TRUE), 
                    multiple = TRUE),
        
        uiOutput("call_type_description")

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