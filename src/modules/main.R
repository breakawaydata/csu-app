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
    div(id = consts$dom$body_container_all_id, class = consts$dom$body_container_class,
      1:nrow(data) %>% purrr::map(
        ~ player_card(data[.x, ])
      )
    ),
    div(id = consts$dom$body_container_position_id, class = consts$dom$body_container_class, style = "display: none",
        1:nrow(positions) %>% purrr::map(
          ~ position_card(positions[.x, ])
        )
    ),
    div(id = consts$dom$body_container_player_id, class = consts$dom$body_container_class, style = "display: none",
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
    req(!is.null(session$userData$player()))
    tags$div(
      class = "player-content",
      style = glue::glue("background-image: url('assets/{session$userData$stat()}.png'); height: 100vh;")
    )
  })
}

player_card <- function(player) {
  tags$div(
    id = player$player_id,
    class = consts$dom$player_card_class, 
    `data-summary` = player$summary,
    `data-explosive` = player$explosive,
    `data-reach` = player$reach,
    `data-balance` = player$balance,
    `data-capacity` = player$capacity,
    `data-picture` = player$picture,
    `data-position` = player$positions,
    `data-name` = paste(player$first, player$last),
    tags$div(class = "stat", player$summary),
    tags$div(class = "player-image",
             style = glue::glue("background-image: url({player$picture});")),
    tags$div(class = "first-name", player$first),
    tags$div(class = "last-name", player$last),
    tags$div(class = "player-number", paste0("#", player$number))
  )
}

position_card <- function(position) {
  tags$div(
    id = position$position,
    class = consts$dom$position_card_class, 
    `data-summary` = position$summary,
    `data-explosive` = position$explosive,
    `data-reach` = position$reach,
    `data-balance` = position$balance,
    `data-capacity` = position$capacity,
    `data-position` = position$positions,
    tags$div(class = "stat", position$summary),
    tags$div(class = "position-name", p(position$position)),
    tags$div(class = "position-desc", position$positions)
  )
} 
