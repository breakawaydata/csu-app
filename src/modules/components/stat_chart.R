import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("jsonlite")
import("shiny")
import("shinyBody")
import("shiny.grid")

export("statChart")

ui <- function(id, options) {
  ns <- NS(id)

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

    div( class = "chart_title", options$title),
    div( class = "chart_icon", tags$img( class = "icon", src = glue::glue("icons/{options$icon}.png"))),
    div(
      class = "chart_wrapper",
      tags$style(glue::glue('#{id} .progress {{ background: {options$background} }}')),
      tags$style(glue::glue('#{id} .bar {{ background: {options$color} }}')),

      div(class = "chart_container",
        div(
          class = "steps",
          lapply(1:options$steps, function(index) { div(class = "bar_step") })
        ),
        tagList(
          lapply(1:options$bar_number, function(index){
            div(
              id = paste0(id, "-", index),
              class = "ui indicating progress",
              `data-value` = 1,
              `data-total` = options$bar_total,
              div(class = "bar")
            )
          })
        )
      )
    ),
    uiOutput(ns("update_bars"))
  )
}

server <- function(input, output, session, state) {
  ns <- session$ns

  observeEvent(state$values, {
    output$update_bars <- renderUI({
      tagList(
        lapply(1:length(state$values), function(index) {
          tags$script(glue::glue('
            $("#{ns(index)}").progress({{value: {state$values[index]}}});
          '))
        })
      )
    })
  })
}

statChart <- R6Class("statChart",
  public = list(
    ui = NULL,
    server = NULL,

    state = reactiveValues(
      id = NULL,
      options = list(
        title = "Bars",
        icon = "explosive",
        bar_number = 5,
        steps = 25,
        bar_total = 100,
        color = "black",
        background = "lightslategray",
        active_color = "lightgray",
        active_background = "gray"
      ),
      values = c()
    ),

    initialize = function(id, title, icon, bar_number, options = NULL, values = NULL) {
      isolate({
        self$state$id <- id

        if(!is.null(values)) self$state$values <- values
        if(!is.null(options)) self$state$options <- options$style <- modifyList(self$state$options, options)

        self$state$options$title <- title
        self$state$options$icon <- icon
        self$state$options$bar_number <- bar_number
      })

      self$ui = function(id) {
        ui(id, isolate(self$state$options))
      }
      self$server = function() {
        callModule(server, id, self$state)
      }
    }
  )
)
statChart <- statChart$new
