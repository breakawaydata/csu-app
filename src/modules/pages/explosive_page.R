import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")

export("explosivePage")

stat_value <- function(value, class = "") {
  div(
    class = glue::glue("stat_value {class}"),
    value
  )
}

chart_widget <- function(id,
                         title = "Bars",
                         icon = "explosive",
                         color = "#286BAF",
                         background = "#9ED3F6",
                         active_color = "#286BAF",
                         active_background = "#B94A12",
                         bar_number = 1
                        ) {
  gridPanel(
    areas = c(
      "chart_title",
      "chart_icon",
      "stats_progress_widget"
    ),
    rows = "50px 50px 1fr",
    columns = "repeat(1, minmax(300px, 1fr))",

    id = id,
    class = "stats_progress_widget",

    div( class = "chart_title", title),
    div( class = "chart_icon", tags$img( class = "icon", src = glue::glue("icons/{icon}.png"))),
    div(
      class = "chart_wrapper",
      tags$style(glue::glue('#{id} .progress {{ background: {background} }}')),
      tags$style(glue::glue('#{id} .bar {{ background: {color} }}')),
      div(class = "chart_container",
        div(
          class = "steps",
          lapply(1:25, function(index) { div(class = "bar_step") })
        ),
        tagList(
          lapply(1:bar_number, function(index){
            div(
              id = paste0(id, "_", index),
              class = "ui indicating progress",
              `data-value` = 1,
              `data-total` = 100,
              div(class = "bar")
            )
          })
        )
      )
    )
  )
}

update_chart_bar <- function(id, bar, value) {
  tags$script(glue::glue('
    $("#{id}_{bar}").progress({{value: {value}}});
  '))
}

ui <- function(id) {
  ns <- NS(id)

  div(
    gridPanel(
      class = "explosive_page_wrapper",
      areas = c("strength_chart body_chart power_chart"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",

      uiOutput(ns("strenght_bars")) %>%
        tagAppendAttributes(class = "strength_chart"),

      uiOutput(ns("body_chart")) %>%
        tagAppendAttributes(class = "body_chart"),

      uiOutput(ns("power_bars")) %>%
        tagAppendAttributes(class = "power_chart")
    )
  )
}

server <- function(input, output, session, data, active_player) {
  ns <- session$ns

  chart_options <- list(
    color = "#286BAF",
    background = "#9ED3F6",
    active_color = "#F66733",
    active_background = "#9ED3F6"
  )

  strenght_bars <- use("modules/components/stat_chart.R")$statChart(
    "strength_chart", "Strength",
    "summary", 3, chart_options
  )

  power_bars <- use("modules/components/stat_chart.R")$statChart(
    "power_chart", "Power",
    "explosive", 3, chart_options
  )

  body_chart <- use("modules/components/body_chart.R")$bodyChart(
    "body_chart",
    list(
      labels = list(
        left = list(
          top = "Upper Strength",
          middle = "Core Strength",
          bottom = "Lower Strength"
        ),
        right = list(
          top = "Upper Power",
          middle = "Core Power",
          bottom = "Lower Power"
        )
      )
    )
  )

  strenght_bars$server()
  power_bars$server()
  body_chart$server()

  output$strenght_bars <- renderUI({ strenght_bars$ui(ns("strength_chart")) })
  output$power_bars <- renderUI({ power_bars$ui(ns("power_chart")) })
  output$body_chart <- renderUI({ body_chart$ui(ns("body_chart")) })

  observeEvent(strenght_bars$state$active, {
    if (length(strenght_bars$state$active) > 0)
      power_bars$state$active <- c()
  })
  observeEvent(power_bars$state$active, {
    if (length(power_bars$state$active) > 0)
      strenght_bars$state$active <- c()
  })

  observeEvent(input$human, {
    print(input$human)
    print(active_player$id)
  })

  observeEvent(active_player$id, {
    active_player$assessements <- data$dataset[which(data$dataset == active_player$id), ]
  })

  observeEvent(active_player$assessements, {
    active_assessement <- active_player$assessements[1, ]

    strenght_bars$state$values <- list(
      total = active_assessement$strength_score,
      bars = c(
        active_assessement$strength_upper,
        active_assessement$strength_core,
        active_assessement$strength_lower
      )
    )

    power_bars$state$values <- list(
      total = active_assessement$power_score,
      bars = c(
        active_assessement$power_upper,
        active_assessement$power_core,
        active_assessement$power_lower
      )
    )

    body_chart$state$values <- list(
      total = active_assessement$power_score,
      left = list(
        top = active_assessement$strength_upper,
        middle = active_assessement$strength_core,
        bottom = active_assessement$strength_lower
      ),
      right = list(
        top = active_assessement$power_upper,
        middle = active_assessement$power_core,
        bottom = active_assessement$power_lower
      )
    )
  })

  power_mapping <- c("right_arm", "bottom_right_torso", "right_leg")
  strength_mapping <- c("left_arm", "bottom_left_torso", "left_leg")

  observeEvent(power_bars$state$active, {
    strenght_bars$state$active <- c()
    body_chart$state$active <- c(power_mapping[power_bars$state$active])
  })
  observeEvent(strenght_bars$state$active, {
    power_bars$state$active <- c()
    body_chart$state$active <- c(strength_mapping[strenght_bars$state$active])
  })
  observeEvent(body_chart$state$active, {
    if (!is.na(which(power_mapping == body_chart$state$active, arr.ind = TRUE)[1])) {
      strenght_bars$state$active <- c()
      power_bars$state$active <- c(which(power_mapping == body_chart$state$active, arr.ind = TRUE)[1])
    }
    if (!is.na(which(strength_mapping == body_chart$state$active, arr.ind = TRUE)[1])) {
      power_bars$state$active <- c()
      strenght_bars$state$active <- c(which(strength_mapping == body_chart$state$active, arr.ind = TRUE)[1])
    }
  })
}


explosivePage <- R6Class("explosivePage",
  public = list(
    ui = NULL,
    server = NULL,

    options = reactiveValues(
      style = list(
        main_color = "red"
      )
    ),

    data = reactiveValues(
      widget_id = NULL,

      dataset = NULL
    ),

    active_player = reactiveValues(
      id = NULL,
      assessements = NULL
    ),

    initialize = function(id, dataset = NULL, style = NULL) {
      if(!is.null(style)) self$options$style <- style
      if(!is.null(dataset)) self$data$dataset <- dataset

      self$ui = ui(id)
      self$server = function() {
        callModule(server, id, self$data, self$active_player)
      }
    }
  )
)
explosivePage <- explosivePage$new
