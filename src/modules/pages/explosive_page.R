import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("jsonlite")
import("shiny")
import("shinyBody")
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
        tagAppendAttributes(
          class = "strength_chart",
          style = "height: 100%; width: 100%;"
      ),

      gridPanel(
        class = "body_chart",
        areas = c(
          "... ...       ...   ...   ...  ...         ...",
          "... ...       ...   title ...  ...         ...",
          "... ...       ...   ...   ...  ...         ...",
          "... str_upper body  body  body power_upper ...",
          "... ...       body  body  body ...         ...",
          "... str_code  body  body  body power_core  ...",
          "... ...       body  body  body ...         ...",
          "... str_lower body  body  body power_lower ...",
          "... ...       body  body  body ...         ..."
        ),
        rows = "25px 50px 0.5fr 50px 1fr 50px 2fr 50px 1fr",

        div(
          class = "body background_body",
          uiOutput(ns("body"))
        ),

        div(class = "explosion_value title", uiOutput(ns("explosion_score"))),
        div(`data-target`= "#explosivePage-strength_chart-1", class = "stat_value body_stat stat_upper stat_left str_upper", uiOutput(ns("strength_upper"))),
        div(`data-target`= "#explosivePage-strength_chart-2", class = "stat_value body_stat stat_center stat_left str_code", uiOutput(ns("strength_core"))),
        div(`data-target`= "#explosivePage-strength_chart-3", class = "stat_value body_stat stat_lower stat_left str_lower", uiOutput(ns("strength_lower"))),
        div(`data-target`= "#explosivePage-power_chart-1", class = "stat_value body_stat stat_upper stat_right power_upper", uiOutput(ns("power_upper"))),
        div(`data-target`= "#explosivePage-power_chart-2", class = "stat_value body_stat stat_center stat_right power_core", uiOutput(ns("power_core"))),
        div(`data-target`= "#explosivePage-power_chart-3", class = "stat_value body_stat stat_lower stat_right power_lower", uiOutput(ns("power_lower"))),

        tags$script(glue::glue("
          $('.stat_value').hover(
            function(){{
              $('.progress').removeClass('stat_focus');
              $(this).addClass('stat_focus');
              $($(this).data('target')).addClass('stat_focus');
            }}, function(){{
              $(this).removeClass('stat_focus');
              $('.progress').removeClass('stat_focus');
            }}
          )
          $('.stat_value').click(
            function(){{
              $('.progress').removeClass('stat_active');
              $('.stat_value').removeClass('stat_active');
              $(this).toggleClass('stat_active');
              $($(this).data('target')).toggleClass('stat_active', $(this).hasClass('stat_active'));
            }}
          )
        "))
      ),

      uiOutput(ns("power_bars")) %>%
        tagAppendAttributes(
          class = "power_chart",
          style = "height: 100%; width: 100%;"
        )
    )
  )
}

server <- function(input, output, session, data, active_player) {
  ns <- session$ns

  chart_options <- list(
    color = "#286BAF",
    background = "#9ED3F6",
    active_color = "#286BAF",
    active_background = "#B94A12"
  )

  strenght_bars <- use("modules/components/stat_chart.R")$statChart(
    "strength_chart", "Strength",
    "summary", 3, chart_options
  )

  power_bars <- use("modules/components/stat_chart.R")$statChart(
    "power_chart", "Power",
    "explosive", 3, chart_options
  )

  strenght_bars$server()
  power_bars$server()

  output$strenght_bars <- renderUI({ strenght_bars$ui(ns("strength_chart")) })
  output$power_bars <- renderUI({ power_bars$ui(ns("power_chart")) })

  output$body <- renderUI({
    bodyInput(ns("human"), color = c("#B4C2D1"))
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
        active_assessement$power_core,
        active_assessement$power_lower
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

    # TODO replace with R6
    output$explosion_score <- renderUI({ span(active_assessement$explosion_score) })
    output$strength_upper <- renderUI({ span(active_assessement$strength_upper) })
    output$strength_core <- renderUI({ span(active_assessement$strength_core) })
    output$strength_lower <- renderUI({ span(active_assessement$strength_lower) })
    output$power_upper <- renderUI({ span(active_assessement$power_upper) })
    output$power_core <- renderUI({ span(active_assessement$power_core) })
    output$power_lower <- renderUI({ span(active_assessement$power_lower) })
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
