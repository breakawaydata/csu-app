import("R6")
import("utils")
import("shiny")
import("glue")
import("dplyr")
import("htmltools")
import("shiny.grid")

export("statChart")

#' Creates the UI for the stat widget.
#'
#' @description Used by the statChart class to generate the corresponding
#'   widget ui.
#'
#' @param id widget id.
#' @param options Ui settings for initializing the widget ui.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
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
    div( class = "chart_icon", tags$img( class = "icon", src = glue::glue("{options$icon}"))),
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

#' Creates the Server for the stat chart widget.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
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

#' Class representing a vertical multi bar chart.
#'
#' A statChart object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
statChart <- R6Class("statChart",
  public = list(
    #' @field ui UI definition of the chart.
    ui = NULL,

    #' @field server Module running a namespaced version specific to each R6 instance.
    server = NULL,

    #' @field state Internal state of the R6 instance. updating iner values from
    #'   outside the instance will trigger internal observers to update specific parts of the widget.
    #'   They can also be observed externally to trigger other functions when the instance state changes.
    #'   options Includes all values that are used for initializing the widget.
    #'   Values Can be set or updated directly to update the values displayed by the chart bars. Expects a
    #'     vector for integer values, with length equal to options$bar_number
    #'   active Vector of indexes of currently active bars. Can be updated directly to update the currently
    #'     active bar.
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
      values = list(total = 0, bars = c()),
      active = c()
    ),

    #' @description
    #' Create a new statChart object.
    #' @param id Unique ID for the widget instance. Also used for namespacing the server module.
    #' @param title Title to be displayed on the widget.
    #' @param icon Icon to be displayed on the widget.
    #' @param bar_number Number of bars to be created.
    #' @param options A named list of options that will overwrite the default state$options.
    #'   Partial named lists will result in only some options being overwritten while
    #'   non named ones stay with the default values.
    #' @param values Optional initial values to use for the bars and widget side label.
    #' @return A new `statChart` object.
    initialize = function(id, title, icon, bar_number, options = NULL, values = NULL) {
      isolate({
        self$state$id <- id

        if(!is.null(values)) self$state$values <- modifyList(self$state$values, values)
        if(!is.null(options)) self$state$options <- options$style <- modifyList(self$state$options, options)

        self$state$options$title <- title
        self$state$options$icon <- icon
        self$state$options$bar_number <- bar_number
      })

      #' @description
      #' Calls the ui for the widget instance.
      #' @param id Namespaced id. When calling UI for inside a diferent module,
      #'   sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
      #' @examples
      #' test_chart <- use("stat_chart.R")$statChart("test")
      #' test_chart$ui(ns("test"))
      self$ui = function(id) {
        ui(id, isolate(self$state$options))
      }

      #' @description
      #' Calls the server module for the widget instance.
      self$server = function() {
        callModule(server, id, self$state)
      }
    }
  )
)
statChart <- statChart$new
