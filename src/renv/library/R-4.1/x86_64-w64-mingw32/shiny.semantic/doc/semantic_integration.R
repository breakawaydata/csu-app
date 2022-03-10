## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(shiny)
library(shiny.semantic)

## ----echo=T, include = T, eval = FALSE----------------------------------------
#  library(shiny)
#  library(shiny.semantic)
#  library(plotly)
#  
#  ui <- semanticPage(
#    segment(
#      class = "basic",
#      a(class="ui green ribbon label", "Plotly demo"),
#      plotlyOutput("plot")
#  
#    )
#  )
#  
#  server <- function(input, output, session) {
#    output$plot <- renderPlotly({
#      plot_ly(economics, x = ~date, color = I("black")) %>%
#        add_lines(y = ~uempmed) %>%
#        add_lines(y = ~psavert, color = I("red"))
#    })
#  }
#  
#  shinyApp(ui = ui, server = server)
#  

## ----echo=T, include = T, eval = FALSE----------------------------------------
#  library(shiny)
#  library(shiny.semantic)
#  library(leaflet)
#  
#  ui <- semanticPage(
#    segment(
#      class = "basic",
#      a(class="ui blue ribbon label", "Leaflet demo"),
#      leafletOutput("map")
#  
#    )
#  )
#  
#  server <- function(input, output, session) {
#    output$map <- renderLeaflet({
#      m <- leaflet() %>% addTiles()
#      m <- m %>% setView(21.00, 52.21, zoom = 12)
#      m
#    })
#  }
#  
#  shinyApp(ui = ui, server = server)

## ----echo=T, include = T, eval = FALSE----------------------------------------
#   library(shiny)
#   library(shiny.semantic)
#  
#   ui <- semanticPage(
#     h2("Pretty tables in Shiny Semantic"),
#     semantic_DTOutput("table")
#   )
#   server <- function(input, output, session) {
#     output$table <- DT::renderDataTable(
#       semantic_DT(mtcars)
#     )
#   }
#   shinyApp(ui, server)

