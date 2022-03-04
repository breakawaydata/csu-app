import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("imola")

export("summaryPage")

#' Creates the styling for an active assessment_report menu item.
#'
#' @description Used by the explosivePage class to generate ui.
#'
#' @param ns The page namespace.
#' @param index the index of the active assessment_report.
#'
#' @return A style tag.
active_assessment_report_style <- function(ns, index) {
  tags$style(glue::glue('
    #{ns("assessment_report_actions")} [data-index="{index}"] {{
      text-decoration: underline;
      font-weight: bolder;
    }}
  '))
}

#' Creates the bindings for triggering assessment report view changes.
#'
#' @description Used by the explosivePage class to generate ui.
#'
#' @param ns The page namespace.
#'
#' @return A script tag.
assessment_report_bindings_script <- function(ns, nrow) {
  tags$script(glue::glue('
    Shiny.setInputValue("{ns("assessment_report_selected")}", {nrow}, {{priority : "event"}});
    $("#{ns("assessment_report_actions")}").on("click", "[data-index]", function() {{
      Shiny.setInputValue("{ns("assessment_report_selected")}", $(this).data("index"), {{priority : "event"}});
    }})
  '))
}
#' Creates the UI for the explosive page.
#'
#' @description Used by the summaryPage class to generate the corresponding ui.
#'
#' @param id widget id.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id) {
  ns <- NS(id)

  div(
    uiOutput(ns("assessment_report_toggle")),
    gridPanel(
      class = "summary_page_wrapper",
      areas = c("fingerprint_chart ... ... ... ",
      "fingerprint_chart explosion_info reach_info balance_info "),
      columns = "450px 20% 20% 20%",
      rows = "10px 1fr",
      
      uiOutput(ns("fingerprint_chart")) %>%
        tagAppendAttributes(class = "fingerprint_chart"),
      
      uiOutput(ns("explosion_info")) %>%
        tagAppendAttributes(class = "explosion_info"),  
      
      uiOutput(ns("reach_info")) %>%
        tagAppendAttributes(class = "reach_info"),   
      
      uiOutput(ns("balance_info")) %>%
        tagAppendAttributes(class = "balance_info")
    )
  )
}

#' Creates the Server for the explosive page.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
server <- function(input, output, session, data, active_player) {
  ns <- session$ns

  chart_options <- list(
    color = "#286BAF",
    background = "#9ED3F6",
    active_color = "#F66733",
    active_background = "#9ED3F6"
  )

  fingerprint_chart <- use("modules/components/fingerprint_chart.R")$fingerprintChart(
    "fingerprint_chart"
  )
  
  explosion_info <- use("modules/components/pillar_chart.R")$pillarChart(
    "explosion_info", "Explosion", "Strength", "Power",
    "icons/BA_explosive_circle.png", "icons/strength_banner.png", "icons/power_banner.png", 
    chart_options
  )

  reach_info <- use("modules/components/pillar_chart.R")$pillarChart(
    "reach_info", "Reach", "Speed", "Agility",
    "icons/BA_reach_circle.png", "icons/speed_banner.png", "icons/agility_banner.png",
    chart_options
  )
  
  balance_info <- use("modules/components/pillar_chart.R")$pillarChart(
    "balance_info", "Balance", "Mobility", "Stability",
    "icons/BA_balance_circle.png", "icons/mobility_banner.png", "icons/stability_banner.png",
    chart_options
  )
  
  fingerprint_chart$server()
  explosion_info$server()
  reach_info$server()
  balance_info$server()

  output$fingerprint_chart <- renderUI({ fingerprint_chart$ui(ns("fingerprint_chart")) })
  output$explosion_info <- renderUI({ explosion_info$ui(ns("explosion_info")) })
  output$reach_info <- renderUI({ reach_info$ui(ns("reach_info")) })
  output$balance_info <- renderUI({ balance_info$ui(ns("balance_info")) })

  observeEvent(active_player$id, {
    player_assessments <- data$dataset[which(data$dataset == active_player$id), ]
    sorted_assessments <- player_assessments[order(
      as.Date(player_assessments$assessment_date, format="%d/%m/%Y")
    ), ]

    active_player$assessments <- sorted_assessments
  })

  # Update the widget values when a new player is picked
  observeEvent(active_player$assessments, {
    active_player$active_assessment <- active_player$assessments[nrow(active_player$assessments), ]

    output$assessment_report_toggle <- renderUI({
      tagList(
        uiOutput(ns("selected_assessment_report_style")),
        div(
          class = "assessment_reports-container",
          id = ns("assessment_report_actions"),
          tagList(
            lapply(seq_len(nrow(active_player$assessments)), function(index) {
              div(
                `data-index` = index,
                class = "assessment_report-toggler",
                active_player$assessments[index, ]$assessment_date
              )
            })
          )
        ),
        assessment_report_bindings_script(ns, nrow(active_player$assessments))
      )
    })
  })

  observeEvent(input$assessment_report_selected, {
    active_player$active_assessment <- active_player$assessments[input$assessment_report_selected, ]
  })

  observeEvent(active_player$active_assessment, {
    active_assessment <- active_player$active_assessment

    output$selected_assessment_report_style <- renderUI({
      active_assessment_report_style(ns, input$assessment_report_selected)
    })

    fingerprint_chart$state$values <- list(
      total = active_assessment$total_score,
      explosion = active_assessment$explosion_score,
      reach = active_assessment$reach_score,
      balance = active_assessment$balance_score
    )
    
    explosion_info$state$values <- list(
      total = active_assessment$explosion_score,
      first_pillar = active_assessment$strength_score,
      second_pillar = active_assessment$power_score
    )
    
    reach_info$state$values <- list(
      total = active_assessment$reach_score,
      first_pillar = active_assessment$speed_score,
      second_pillar = active_assessment$agility_score
    )
    
    balance_info$state$values <- list(
      total = active_assessment$balance_score,
      first_pillar = active_assessment$mobility_score,
      second_pillar = active_assessment$stability_score
    )
  })

}

#' Class representing the explosive page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
summaryPage <- R6Class("summaryPage",
  public = list(
    #' @field ui UI definition of the chart.
    ui = NULL,

    #' @field server Module running a namespaced version specific to each R6 instance.
    server = NULL,

    #' @field data Page data that can be updated to trigger ui and server updates
    data = reactiveValues(
      widget_id = NULL,
      dataset = NULL
    ),

    #' @field active_player Current active played and corresponding assessments.
    active_player = reactiveValues(
      id = NULL,
      assessments = NULL,
      active_assessment = NULL
    ),

    #' @description
    #' Create a new summaryPage object.
    #' @param id Unique ID for the page. Also used for namespacing the server module.
    #' @param dataset Title Initial data to be passed to the page.
    #'
    #' @return A new `summaryPage` object.
    initialize = function(id, dataset = NULL) {
      if(!is.null(dataset)) self$data$dataset <- dataset

      self$ui = ui(id)
      self$server = function() {
        callModule(server, id, self$data, self$active_player)
      }
    }
  )
)
summaryPage <- summaryPage$new
