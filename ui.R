shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    
    # Map Tab ----
    tabPanel("Map", id="map_tab",
      div(class = "outer",
         
         tags$head(
           # Include custom CSS
           includeCSS("styles.css")
         ),
         
         leafletOutput("dispatch_map", width = "100%", height = "100%"),
         
         absolutePanel(id = "controls", 
                       fixed = TRUE,
                       draggable = TRUE, 
                       top = 70, 
                       left = 10,
                       right = "auto", 
                       bottom = "auto",
                       width = 350, 
                       height = "auto",
                       align = "center",
                       
                       uiOutput("points_on_map"),
                       
                       # NULL end defaults to today's date
                       dateRangeInput("occurence_date_range", 
                                      label = h3("Occurence Date"),
                                      start = "2019-01-01", 
                                      end = Sys.Date(),
                                      min = "2019-01-01",
                                      max = Sys.Date()),
                       
                       checkboxGroupInput("day_of_week", 
                                          label = h3("Day of the Week"),
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
                       
                       pickerInput("description_groups", 
                                   label = h3("Call Type Groups"), 
                                   choices = list(Times = description_time_groups,
                                                  Crimes = description_crime_groups), 
                                   selected = description_groups,
                                   options = list(`selected-text-format` = "count > 1",
                                                  `actions-box` = TRUE,
                                                  `live-search` = TRUE), 
                                   multiple = TRUE),
                       
                       uiOutput("call_type_description"),
                       
                       actionButton("new_points", 
                                    label = "New Points",
                                    icon = icon("map-marker"), 
                                    style = "color: #fff; 
                                            background-color: #337ab7;
                                            border-color: #2e6da4;")
         ),
         
         tags$div(id = "points_displayed_message",
                  htmlOutput("points_displayed_message")
         ),
         
         tags$div(id = "api_status_button",
                  uiOutput("api_status_button")
         ),
         
         tags$div(id = "help_button",         
                  actionButton("help",
                               label = "",
                               icon = icon("question"),
                               style = "color: #337ab7; 
                                        background-color: #fff;
                                        border-color: #2e6da4;"))
      )
    ),
    
    # Table Tab ----
    tabPanel("Details", id="table_tab",
      fluidRow(
       DT::dataTableOutput("dispatch_table")
      )
    )
  )
)