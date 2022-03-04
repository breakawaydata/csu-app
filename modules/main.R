import("htmltools")
import("shiny")
import("shiny.semantic")
import("magrittr")

export("ui", "init_server")

consts <- modules::use(consts)

ui <- function(id, data, positions) {
  ns <- NS(id)
  tagList(
    div(id = "toggle_main_buttons_wrapper",
      div(id = "toggle_main_buttons", class = "ui breadcrumb",
          tags$a("All", id = "toggle_main_buttons_all", class = "section"),
          tags$a("Team", id = "toggle_main_buttons_team", class = "section")
      )
    ),
    div(id = "main_team_wrapper",
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
    ),
    div(id = "main_all_wrapper", #style = ifelse(consts$default$main_toggle_all)
      br(), DT::DTOutput(ns("table"))
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
  
  output$table <- DT::renderDT({
    pages[[session$userData$stat()]]$data$dataset
  })

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

    if (session$userData$stat() == "balance") {
      pages$balance$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$balance$ui
      )
    }
    
    if (session$userData$stat() == "schedule") {
      pages$schedule$active_player$id <- input$player
      content <- tags$div(
        class = "player-content",
        pages$schedule$ui
      )
    }

    content
  })
  outputOptions(output, "player", suspendWhenHidden = FALSE)
  outputOptions(output, "table", suspendWhenHidden = FALSE)
}

player_card <- function(player) {
  tags$div(
    id = player$player_id,
    class = consts$dom$player_card_class,
    `data-summary` = player$summary,
    `data-explosive` = player$explosion,
    `data-reach` = player$reach,
    `data-balance` = player$balance,
    `data-schedule` = player$number,
    `data-picture` = player$picture,
    `data-position` = player$positions,
    `data-name` = paste(player$first, player$last),
    `data-sorting-name` = paste0(player$last, player$first),
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
    `data-explosive` = position$explosion,
    `data-reach` = position$reach,
    `data-balance` = position$balance,
    `data-schedule` = position$summary,
    `data-position` = position$positions,
    tags$div(class = "stat", position$summary),
    tags$div(class = "position-name", p(position$position)),
    tags$div(class = "position-desc", position$positions)
  )
}
