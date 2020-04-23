sidebar = dashboardSidebar(
  width = '325',

  # NULL end defaults to today's date
  dateRangeInput("occurence_date_range", 
                 label = h3("Occurrence Date"),
                 start = most_recent_date - 1,
                 end = most_recent_date,
                 min = paste0(format(most_recent_date, "%Y"), "-01-01"),
                 max = most_recent_date),
  
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
              choices = description_groups, 
              selected = description_groups,
              options = list(`selected-text-format` = "count > 1",
                             `actions-box` = TRUE,
                             `live-search` = TRUE), 
              multiple = TRUE),
  
  uiOutput("call_type_description"),
  
  HTML("<br>"),
  
  HTML("<h4 style='padding-left: 15px; margin-bottom: 0px;'>Display:</h4>"),
  
  prettyCheckbox(
    inputId = "markers_check_box",
    label = "Markers",
    value = TRUE,
    icon = icon("check-square-o"), 
    status = "primary",
    outline = TRUE
  ),
  
  prettyCheckbox(
    inputId = "marker_groups_check_box",
    label = "Marker Groups",
    value = TRUE,
    icon = icon("check-square-o"), 
    status = "primary",
    outline = TRUE
  ),
  
  prettyCheckbox(
    inputId = "heatmap_check_box",
    label = "Heatmap",
    value = TRUE,
    icon = icon("check-square-o"), 
    status = "primary",
    outline = TRUE
  )
  
)


body = dashboardBody(
  useShinyjs(),
  leafletOutput("dispatch_map"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  tags$script(src = "scripts.js"),
  tags$div(id = "points_displayed_message",
           htmlOutput("points_displayed_message")
  ),
  
  tags$div(id = "api_status_button",
           uiOutput("api_status_button")
  ),
  
  tags$div(id = "github_button",
           actionButton("github_button",
                        label = "",
                        icon = icon("github", "fa-1.5x"),
                        onclick ="window.open('https://github.com/mvanbommel/SacCrimeData', '_blank')")),
  
  tags$div(id = "help_button",
           actionButton("help",
                        label = "",
                        icon = icon("question")))
)

dashboardPage(
  dashboardHeader(title = "Sacramento Dispatch Data"),
  sidebar,
  body
)