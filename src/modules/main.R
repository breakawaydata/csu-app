import("htmltools")
import("shiny")
import("shiny.semantic")
import("magrittr")

export("ui", "init_server")

consts <- modules::use(consts)

ui <- function(id, data, positions) {
  ns <- NS(id)
  tagList(
    div(id = consts$dom$body_container_all_id, class = consts$dom$body_container_class,
      seq_len(nrow(data)) %>% purrr::map(
        ~ player_card(data[.x, ])
      )
    ),
    div(id = consts$dom$body_container_position_id, class = consts$dom$body_container_class, style = "display: none",
        seq_len(nrow(positions)) %>% purrr::map(
          ~ position_card(positions[.x, ])
        )
    ),
    div(id = consts$dom$body_container_player_id, class = consts$dom$body_container_class, style = "display: none",
      uiOutput(ns("player"))
    )
  )
}

init_server <- function(id, pages) {
  callModule(server, id, pages)
}

server <- function(input, output, session, pages) {
  ns <- session$ns

  session$userData$player <- reactiveVal()

  observeEvent(input$player, {
    session$userData$player(input$player)
  }, ignoreInit = TRUE)

  output$player <- renderUI({
    req(!is.null(session$userData$player()))

    content <- tags$div(
      class = "player-content",
      style = glue::glue("background-image: url('assets/{session$userData$stat()}.png'); height: 100vh;")
    )
    
    if (session$userData$stat() == "summary") {
      pages$summary$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$summary$ui
      )
    }

    if (session$userData$stat() == "explosive") {
      pages$explosion$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$explosion$ui
      )
    }

    if (session$userData$stat() == "reach") {
      pages$reach$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$reach$ui
      )
    }
    
    if (session$userData$stat() == "capacity") {
      pages$capacity$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$capacity$ui
      )
    }

    if (session$userData$stat() == "balance") {
      pages$balance$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$balance$ui
      )
    }
    
    if (session$userData$page() == "game") {
    content <- tags$div(
      class = "player-content",
      style = glue::glue("background-image: url('assets/live.gif'); height: 100vh;")
     )
    }

    content
  })
  outputOptions(output, "player", suspendWhenHidden = FALSE)
}

player_card <- function(player) {
  tags$div(
    id = player$player_id,
    class = consts$dom$player_card_class,
    `data-summary` = player$total_score,
    `data-explosive` = player$explosion_score,
    `data-reach` = player$reach_score,
    `data-balance` = player$balance_score,
    `data-capacity` = player$balance_score,
    `data-picture` = player$picture,
    `data-position` = player$positions,
    `data-name` = paste(player$first, player$last),
    `data-sorting-name` = paste0(player$last, player$first),
    tags$div(class = "stat", player$total_score),
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
    `data-explosive` = position$explosion,
    `data-reach` = position$reach,
    `data-balance` = position$balance,
    `data-capacity` = position$balance,
    `data-position` = position$positions,
    tags$div(class = "stat", position$summary),
    tags$div(class = "position-name", p(position$position)),
    tags$div(class = "position-desc", position$positions)
  )
}
