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
        sliderInput("points_on_map", h3("Points Displayed"),
                    min = 0, max = 100, value = 25),
        sliderInput("time_range", h3("Time"))
      )
    ),
    tabPanel("Table", id="table_tab",
      fluidRow(
       DT::dataTableOutput("dispatch_table")
      )
    )
  )
)