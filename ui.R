library(shiny)

shinyUI(
  navbarPage("Sacramento Dispatch Data", id = "dispatch_data",
    tabPanel("Map", id="map_tab",
      fluidRow(
        leafletOutput("dispatch_map"),
        p(),
        actionButton("new_points", "New points")
      )
    ),
    tabPanel("Table", id="table_tab",
      fluidRow(
       DT::dataTableOutput("dispatch_table")
      )
    )
  )
)