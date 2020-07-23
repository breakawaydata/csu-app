import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("datasets")

export("fileDownloader")

#' Creates the UI for the fileDownloader widget.
#'
#' @description Used by the fileDownloader class to generate the corresponding
#'   widget ui.
#'
#' @param id widget id.
#' @param id widget instance options.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id, options) {
  ns <- NS(id)

  div(
    id = id,
    class = "file-downloader",
    downloadLink(
      ns("downloadData"),
      div(
        class = "button-content",
        span(class = "title", options$title),
        span(class = "subtitle", options$subtitle),
        span(class = "name", uiOutput(ns("currentView"))),
        tags$img(class = "icon", src = options$icon)
      ),
      class = "export-view-button"
    )
  )
}

#' Creates the Server for the fileDownloader widget.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
server <- function(input, output, session, state) {
  ns <- session$ns

  output$downloadData <- downloadHandler(
    filename = function() {
      # Function resposible for naming the downloaded file.
      # Dinamicaly called whenever the button is triggered.
      paste("data-", paste(state$stat, state$level, state$target_id, sep = "_"), ".txt", sep="")
    },
    content = function(file) {
      # Function resposible for creating the downloaded file content.
      # Dinamicaly called whenever the button is triggered.
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

#' Class representing a file downloader.
#'
#' A fileDownloader object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
fileDownloader <- R6Class("fileDownloader",
  public = list(
    #' @description
    #' Calls the ui for the widget instance.
    #' @param id Namespaced id. When calling UI for inside a diferent module,
    #'   sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
    ui = NULL,

    #' @description
    #' Calls the server module for the widget instance.
    server = NULL,

    #' @field state Internal state of the R6 instance. updating iner values from
    #'   outside the instance will trigger internal observers to update specific parts of the widget.
    state = reactiveValues(
      id = NULL,

      stat = NULL,
      level = NULL,
      target_id = NULL
    ),

    #' @field options Initialization UI options
    options = list(
      title = "Export",
      subtitle = "Current View",
      icon = "icons/BA_PDF_icon.svg"
    ),

    #' @description
    #' Create a new fileDownloader object.
    #' @param id Unique ID for the widget instance. Also used for namespacing the server module.
    #' @param options Named list with optional UI options. Same structure as self$options.
    #'  Only values that overwrite self$options need to be passed.
    #' @return A new `fileDownloader` object.
    initialize = function(id, options = NULL) {
      isolate({
        self$state$id <- id

        if(!is.null(options)) self$options <- modifyList(self$options, options)
      })

      self$ui = function() {
        ui(id, self$options)
      }
      self$server = function() {
        callModule(server, id, self$state)
      }
    }
  )
)
fileDownloader <- fileDownloader$new
