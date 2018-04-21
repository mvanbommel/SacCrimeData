library(shiny)

shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    tabPanel("Map", id="map_tab",
      fluidRow(
        leafletOutput("dispatch_map"),
        textOutput("points_displayed_message"),
        actionButton("new_points", "New points")
      ),
      fluidRow(
        uiOutput("points_on_map"),
        sliderInput("occurence_time_range", h3("Occurence Time"),
                    min=0, max=24, value=c(9, 17),
                    post=':00')
      )
    ),
    tabPanel("Table", id="table_tab",
      fluidRow(
       DT::dataTableOutput("dispatch_table")
      )
    )
  )
)