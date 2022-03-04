import("R6")
import("utils")
import("shiny")
import("glue")
import("dplyr")
import("htmltools")
import("imola")

export("pillarChart")

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
      "chart_title chart_title chart_title",
      "pillar_icon pillar_icon pillar_icon",
      "pillar_score pillar_score pillar_score",
      "subpillar_name_1 ... subpillar_name_2",
      "subpillar_icon_1 ... subpillar_icon_2",
      "first_pillar_score ... second_pillar_score"
    ),
    rows = "30px 1fr 40px 30px 175px 40px",
    columns = ".5fr 15px .5fr",
    
    id = id,
    class = "pillar_chart",
    
    div(class = "chart_title title_label", options$title),
    div(class = "subpillar_name_1 title_label", options$subpillar_one),
    div(class = "subpillar_name_2 title_label", options$subpillar_two),
    
    div(class = "pillar_icon chart_icon", tags$img(class = "icon", src = glue::glue("{options$icon}"))),
    div(class = "subpillar_icon_1 chart_icon", tags$img(class = "banner", src = glue::glue("{options$icon_1}"))),
    div(class = "subpillar_icon_2 chart_icon", tags$img(class = "banner", src = glue::glue("{options$icon_2}"))),
    div(
      class = "pillar_score stat_score",
      uiOutput(ns("total_score"))
    ),
    div(
      class = "first_pillar_score stat_score",
      uiOutput(ns("first_pillar_score"))
    ),
    div(
      class = "second_pillar_score stat_score",
      uiOutput(ns("second_pillar_score"))
    )
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
    output$first_pillar_score <- renderUI({
      span(state$values$first_pillar)
    })
    output$second_pillar_score <- renderUI({
      span(state$values$second_pillar)
    })
  })
}

#' Class representing a vertical multi bar chart.
#'
#' A statChart object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
pillarChart <- R6Class("pillarChart",
                     public = list(
                       #' @description
                       #' Calls the ui for the widget instance.
                       #' @param id Namespaced id. When calling UI for inside a diferent module,
                       #'   sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
                       #' @examples
                       #' test_chart <- use("stat_chart.R")$statChart("test")
                       #' test_chart$ui(ns("test"))
                       ui = NULL,
                       
                       #' @description
                       #' Calls the server module for the widget instance.
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
                           subpillar_one = "SubPillar_1",
                           subpillar_two = "SubPillar_2",
                           icon = "pillar",
                           icon_1 = "subpillar1",
                           icon_2 = "supbillar2",
                           color = "black",
                           background = "lightslategray",
                           active_color = "lightgray",
                           active_background = "gray"
                         ),
                         values = list(total = 0, first_pillar = 0, second_pillar = 0),
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
                       initialize = function(id, title, subpillar_one, subpillar_two, icon, icon_1, icon_2, bar_number, options = NULL, values = NULL) {
                         isolate({
                           self$state$id <- id
                           
                           if(!is.null(values)) self$state$values <- modifyList(self$state$values, values)
                           if(!is.null(options)) self$state$options <- options$style <- modifyList(self$state$options, options)
                           
                           self$state$options$title <- title
                           self$state$options$subpillar_one <- subpillar_one
                           self$state$options$subpillar_two <- subpillar_two
                           self$state$options$icon <- icon
                           self$state$options$icon_1 <- icon_1
                           self$state$options$icon_2 <- icon_2
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
pillarChart <- pillarChart$new
