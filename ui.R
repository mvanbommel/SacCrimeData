library(shiny)

shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    tabPanel("Map", id="map_tab",
      fluidRow(
        leafletOutput("dispatch_map"),
        p(),
        actionButton("new_points", "New points")
      ),
      fluidRow(
        uiOutput("points_on_map"),
        sliderInput("time_range", h3("Time"),
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