import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("datasets")

export("fileDownloader")

ui <- function(id, options) {
  ns <- NS(id)

  div(
    id = id,
    class = "file-downloader",
    downloadLink(
      ns("downloadData"),
      div(
        class = "button-content",
        span(class = "title", "Export"),
        span(class = "subtitle", "Current View"),
        span(class = "name", uiOutput(ns("currentView"))),
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
      paste("data-", paste(state$stat, state$level, state$target_id, sep = "_"), ".txt", sep="")
    },
    content = function(file) {
      content <- paste(paste(state$stat, state$level, state$target_id, sep = " "), collapse = ", ")

      writeLines(content, file)
    }
  )

  observeEvent(session$userData$stat(), {
    state$stat <- session$userData$stat()
  })

  observeEvent(session$userData$level(), {
    state$level <- session$userData$level()

    if(session$userData$level()[1] == "all")
      state$target_id <- NULL
  })

  observeEvent(session$userData$player(), {
    state$target_id <- session$userData$player()
  })

  output$currentView <- renderUI({
    span(paste(state$stat, state$level, state$target_id, sep = " "))
  })
}

fileDownloader <- R6Class("fileDownloader",
  public = list(
    ui = NULL,
    server = NULL,

    state = reactiveValues(
      id = NULL,

      stat = NULL,
      level = NULL,
      target_id = NULL,

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
fileDownloader <- fileDownloader$new
