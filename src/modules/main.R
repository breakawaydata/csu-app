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
        ~ player_card(data[.x, ])
      )
    ),
    div(id = "position-container", class = "body-container", style = "display: none",
        1:nrow(positions) %>% purrr::map(
          ~ position_card(positions[.x, ])
        )
    ),
    div(id = "player-container", class = "body-container", style = "display: none",
        uiOutput(ns("player"))
    )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns
  
  session$userData$player <- reactiveVal()
  
  observeEvent(input$player, {
    session$userData$player(input$player)
  }, ignoreInit = TRUE)
  
  output$player <- renderUI({
    session$userData$player()
  })

  output$active <- renderUI({
    paste(session$userData$stat(), "+", session$userData$level())
  })
}

player_card <- function(player) {
  tags$div(
    id = player$player_id,
    class = "player-card", 
    `data-summary` = player$summary,
    `data-explosive` = player$explosive,
    `data-reach` = player$reach,
    `data-balance` = player$balance,
    `data-capacity` = player$capacity,
    tags$div(class = "player-image",
             style = glue::glue("background-image: url({player$picture});")),
    tags$div(class = "first-name", player$first),
    tags$div(class = "last-name", player$last),
    tags$div(class = "player-number", paste0("#", player$number)),
    tags$div(class = "stat", player$summary)
  )
}

position_card <- function(position) {
  tags$div(
    id = position$position,
    class = "position-card", 
    `data-summary` = position$summary,
    `data-explosive` = position$explosive,
    `data-reach` = position$reach,
    `data-balance` = position$balance,
    `data-capacity` = position$capacity,
    tags$div(class = "position-name", p(position$position)),
    tags$div(class = "position-desc", position$positions),
    tags$div(class = "stat", position$summary)
  )
} 
