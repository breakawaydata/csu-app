import("htmltools")
import("shiny")
import("shiny.semantic")
import("magrittr")

export("ui", "init_server")

expose("utils/utils.R")
consts <- modules::use(consts)

ui <- function(id, data, positions) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("active")),
    div(id = "all-container", class = "body-container",
      1:nrow(data) %>% purrr::map(
        ~ tags$div(
          id = data[.x, ]$player_id, class = "player-card", 
          style = "width: 100px; height: 100px; border: 1px solid black; float: left; margin: 5px;", 
          `data-summary` = data[.x, ]$summary,
          `data-explosive` = data[.x, ]$explosive,
          `data-reach` = data[.x, ]$reach,
          `data-balance` = data[.x, ]$balance,
          `data-capacity` = data[.x, ]$capacity,
          tags$div(tags$strong("Name:"), data[.x, ]$first),
          tags$div(tags$strong("Surname:"), data[.x, ]$last),
          tags$div(tags$strong("Number:"), data[.x, ]$number),
          tags$div(class = "stat", round(data[.x, ]$summary))
        )
      )
    ),
    div(id = "position-container", class = "body-container", style = "display: none",
        1:nrow(positions) %>% purrr::map(
          ~ tags$div(
            id = positions[.x, ]$position, class = "position-card", 
            style = "width: 100px; height: 100px; border: 1px solid black; float: left; margin: 5px;", 
            `data-summary` = positions[.x, ]$summary,
            `data-explosive` = positions[.x, ]$explosive,
            `data-reach` = positions[.x, ]$reach,
            `data-balance` = positions[.x, ]$balance,
            `data-capacity` = positions[.x, ]$capacity,
            tags$div(tags$strong("Position:"), positions[.x, ]$positions),
            tags$div(class = "stat", round(positions[.x, ]$summary))
          )
        )
    ),
    div(id = "player-container", class = "body-container", style = "display: none", "Player")
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
