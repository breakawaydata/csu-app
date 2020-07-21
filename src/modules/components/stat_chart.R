import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
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
      id = ns("bar_list"),
      class = "chart_wrapper",
      tags$style(glue::glue('#{id} .progress {{ background: {options$background} }}')),
      tags$style(glue::glue('#{id} .progress.selected_bar {{ background: {options$active_background} }}')),

      tags$style(glue::glue('#{id} .bar {{ background: {options$color} }}')),
      tags$style(glue::glue('#{id} .progress.selected_bar .bar {{ background: {options$active_color} }}')),

      div(class = "chart_container",
        div( id = ns("total_wrapper"), class = "chart_total", uiOutput(ns("total_score"))),
        div(
          class = "steps",
          lapply(1:options$steps, function(index) { div(class = "bar_step") })
        ),
        tagList(
          lapply(1:options$bar_number, function(index){
            div(
              id = paste0(id, "-", index),
              class = "ui indicating progress",
              `data-index` = index,
              `data-value` = 1,
              `data-total` = options$bar_total,
              div(class = "bar")
            )
          })
        ),
        tags$script(glue::glue('
          $("#{id}").on("click", "[data-index]", function() {{
            Shiny.setInputValue("{id}-bar_selected", $(this).data("index"), {{priority : "event"}});
          }})
        '))
      )
    ),
    uiOutput(ns("update_bars_script")),
    uiOutput(ns("active_bars_script"))
  )
}

server <- function(input, output, session, state) {
  ns <- session$ns

  observeEvent(state$values, {
    output$total_score <- renderUI({
      span(state$values$total)
    })

    output$update_bars_script <- renderUI({
      total_wrapper <- ns("total_wrapper")

      tagList(
        lapply(1:length(state$values$bars), function(index) {
          tags$script(glue::glue('
            $("#{ns(index)}").progress({{value: {state$values$bars[index]}}});
            $("#{total_wrapper}").css("left", "calc({state$values$total}% - 20px)");
          '))
        })
      )
    })
  })

  observeEvent(state$active, {
    output$active_bars_script <- renderUI({
      bar_wrapper <- ns("bar_list")

      tagList(
        tags$script(glue::glue('
          $("#{bar_wrapper} .progress").removeClass("selected_bar");
        ')),
        if (length(state$active) > 0) {
          lapply(1:length(state$active), function(index) {
            tags$script(glue::glue('
              $("#{ns(state$active[index])}").addClass("selected_bar")
            '))
          })
        }
      )
    })
  })

  observeEvent(input$bar_selected, {
    state$active <- c(input$bar_selected)
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
      values = list(
        total = 0,
        bars = c()
      ),
      active = c()
    ),

    initialize = function(id, title, icon, bar_number, options = NULL, values = NULL) {
      isolate({
        self$state$id <- id

        if(!is.null(values)) self$state$values <- modifyList(self$state$values, values)
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
