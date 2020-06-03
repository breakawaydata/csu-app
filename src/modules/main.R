import("htmltools")
import("shiny")
import("shiny.semantic")
import("magrittr")

export("ui", "init_server")

expose("utils/utils.R")
consts <- modules::use(consts)

ui <- function(id, data) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("active")),
    tagList(
      1:nrow(data) %>% purrr::map(
        ~ tags$div(
          id = data[.x, ]$player_id, class = "player-card", 
          style = "width: 100px; height: 100px; border: 1px solid black; float: left; margin: 5px;", 
          tags$div(tags$strong("Name:"), data[.x, ]$first),
          tags$div(tags$strong("Surname:"), data[.x, ]$last),
          tags$div(tags$strong("Number:"), data[.x, ]$number)
        )
      )
    )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns

  output$active <- renderUI({
    paste(session$userData$stat(), "+", session$userData$level())
  })
}
