import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")

export("pdfDownloader")

ui <- function(id, options) {
  ns <- NS(id)

  div(id = id, class = "pdf-downloader", uiOutput(ns("test")))
}

server <- function(input, output, session, state) {
  ns <- session$ns

  output$test <- renderUI({
    span("test")
  })
}

pdfDownloader <- R6Class("pdfDownloader",
  public = list(
    ui = NULL,
    server = NULL,

    state = reactiveValues(
      id = NULL,

      options = list()
    ),

    initialize = function(id, options = NULL) {
      isolate({
        self$state$id <- id

        if(!is.null(options)) self$state$options <- options$style <- modifyList(self$state$options, options)
      })

      self$ui = function() {
        ui(id)
      }
      self$server = function() {
        callModule(server, id, self$state)
      }
    }
  )
)
pdfDownloader <- pdfDownloader$new
