import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("imola")
import("gt")

export("scheduleallPage")




#' Creates the UI for the schedule page.
#'
#' @description Used by the schedulePage class to generate the corresponding ui.
#'
#' @param id widget id.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id) {
  ns <- NS(id)
  div(
    gridPanel(
      class = "title_card",
      areas = c("... ...",
                "... title",
                "... ..."),
      rows = "20px 30px 20px",
      columns = "10px 1fr",
      
      
      div(class = "title title_label", "Team Schedule")
    ),
    gridPanel(
      div(id = "test",
          gt_output(ns("schedule_table")))
    )
  )
}

#' Creates the Server for the schedule page.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
server <- function(input, output, session, data, dataset) {
  ns <- session$ns
  
  # gt_table  <- reactive({
  #   test <- dataset %>% gt()
  #   return(test)
  # })
  
  print(dataset)
  
  output$schedule_table <- gt::render_gt(dataset %>% gt::gt())
}

#' Class representing the schedule page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
scheduleallPage <- R6Class("scheduleallPage",
                        public = list(
                          #' @field ui UI definition of the chart.
                          ui = NULL,
                          
                          #' @field server Module running a namespaced version specific to each R6 instance.
                          server = NULL,
                          
                          #' @field data Page data that can be updated to trigger ui and server updates
                          data = reactiveValues(
                            widget_id = NULL
                          ),
                          #' @field datset Page data that is always used on this page
                          dataset = NULL,
                          
                          #' @description
                          #' Create a new explosivePage object.
                          #' @param id Unique ID for the page. Also used for namespacing the server module.
                          #' @param dataset Title Initial data to be passed to the page.
                          #'
                          #' @return A new `explosivePage` object.
                          initialize = function(id, dataset = NULL) {
                            if(!is.null(dataset)) self$dataset <- dataset
                            
                            self$ui = ui(id)
                            self$server = function() {
                              callModule(server, id, self$data, self$dataset)
                            }
                          }
                        )
)
scheduleallPage <- scheduleallPage$new
