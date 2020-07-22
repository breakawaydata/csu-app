import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("datasets")

export("pdfDownloader")

ui <- function(id, options) {
  ns <- NS(id)

  div(
    id = id,
    class = "pdf-downloader",
    downloadLink(
      ns("downloadData"),
      div(
        class = "button-content",
        span(class = "title", "Export"),
        span(class = "subtitle", "Current View"),
        span(class = "name", uiOutput(ns("state"))),
        tags$img(class = "icon", src = "icons/pdf_download.svg")
      ),
      class = "export-view-button"
    )
  )
}

server <- function(input, output, session, state) {
  ns <- session$ns

  output$downloadData <- downloadHandler(
    filename = function() {

      browser()

      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {

      browser()

      pdf(data, file)
    }
  )

  output$state <- renderUI({
    span("something")
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
