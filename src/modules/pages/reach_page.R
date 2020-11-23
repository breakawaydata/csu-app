import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")

export("reachPage")

cards_descriptions <- list(
  speed_card = "Speed is the ability for an athlete to get from A to B
  as fast as possible.",
  details_card = "To improve reach, focus on quick twitch muscles as this will
  allow him to break off the line faster and close quicker.",
  agility_card = "Agility is how quickly an athlete can get from A to B to C in
  response to an external stimulus."
)
#' Creates the styling for an active assessment_report menu item.
#'
#' @description Used by the speedPage class to generate ui.
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
#' @description Used by the speedPage class to generate ui.
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

#' Creates the UI for the speed page.
#'
#' @description Used by the speedPage class to generate the corresponding ui.
#'
#' @param id widget id.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id) {
  ns <- NS(id)

  div(
    uiOutput(ns("assessment_report_toggle")),
    gridPanel(
      class = "reach_page_wrapper",
      areas = c("speed_chart speed_agility_chart agility_chart"),
      columns = "1fr minmax(500px, 2.7fr) 1fr",
      gap = "20px",

      uiOutput(ns("speed_bars")) %>%
        tagAppendAttributes(class = "speed_chart"),

      uiOutput(ns("speed_agility_chart")) %>%
        tagAppendAttributes(class = "speed_agility_chart"),

      uiOutput(ns("agility_bars")) %>%
        tagAppendAttributes(class = "agility_chart")
    ),
    gridPanel(
      class = "reach_page_wrapper",
      areas = c("speed_card details_card agility_card"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",

      uiOutput(ns("speed_card")) %>%
        tagAppendAttributes(class = "speed_card"),

      uiOutput(ns("details_card")) %>%
        tagAppendAttributes(class = "details_card"),

      uiOutput(ns("agility_card")) %>%
        tagAppendAttributes(class = "agility_card")
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
    color = "#C4CB38",
    background = "#FCF9B9",
    active_color = "#F66733",
    active_background = "#FCF9B9"
  )

  speed_bars <- use("modules/components/stat_chart.R")$statChart(
    "speed_chart", "Speed",
    "icons/BA_Speed.svg", 4, chart_options
  )

  agility_bars <- use("modules/components/stat_chart.R")$statChart(
    "agility_chart", "Agility",
    "icons/BA_Agility.svg", 3, chart_options
  )

  speed_agility_chart <- use("modules/components/forty_agility_chart.R")$speedagilityChart(
    "speed_agility_chart",
    list(
      color = "#C4C4C4",
      active_color = "#F66733",
      labels = list(
        left = list(
          top = "Forty",
          middle = "Twenty",
          lower = "Ten",
          lowest = "Five"
        ),
        right = list(
          top = "In & Out",
          middle = "CoD",
          lower = "Initial Burst"
        )
      )
    )
  )



  speed_bars$server()
  agility_bars$server()
  speed_agility_chart$server()

  output$speed_bars <- renderUI({ speed_bars$ui(ns("speed_chart")) })
  output$agility_bars <- renderUI({ agility_bars$ui(ns("agility_chart")) })
  output$speed_agility_chart <- renderUI({ speed_agility_chart$ui(ns("speed_agility_chart")) })


  # Text cards with the information about overall strength, overall power and
  # detailed information about selected part of the body
  text_card <- use("modules/components/text_card.R")$text_card
  speed_agility_side <- reactive({ speed_agility_chart_coordinates()[1] })
  speed_agility_level <- reactive({ speed_agility_chart_coordinates()[2] })

  # UI of strength card on the left side
  output$speed_card <- renderUI({
    text_card(
      "Speed",
      speed_bars$state$values$total,
      cards_descriptions$speed_card,
      class = "custom-class"
    )
  })

  # UI of details card in the middle
  output$details_card <- renderUI({
    if(!is.null(speed_agility_side()) & !is.null(speed_agility_side())) {
      text_card(
        speed_agility_chart$state$options$labels[[speed_agility_side()]][[speed_agility_level()]],
        speed_agility_chart$state$values[[speed_agility_side()]][[speed_agility_level()]],
        cards_descriptions$details_card,
        class = "custom-class"
      )
    }

  })

  # UI of power card on the right side
  output$agility_card <- renderUI({
    text_card(
      "Agility",
      agility_bars$state$values$total,
      cards_descriptions$agility_card,
      class = "custom-class"
    )
  })

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

    speed_bars$state$values <- list(
      total = active_assessment$speed_score,
      bars = c(
        active_assessment$speed_upper,
        active_assessment$speed_core,
        active_assessment$speed_lower,
        active_assessment$speed_lower
      )
    )

    agility_bars$state$values <- list(
      total = active_assessment$agility_score,
      bars = c(
        active_assessment$agility_upper,
        active_assessment$agility_core,
        active_assessment$agility_lower
      )
    )

    speed_agility_chart$state$values <- list(
      total = active_assessment$reach_score,
      left = list(
        top = active_assessment$speed_upper,
        middle = active_assessment$speed_core,
        lower = active_assessment$speed_lower,
        lowest = active_assessment$speed_lower
      ),
      right = list(
        top = active_assessment$agility_upper,
        middle = active_assessment$agility_core,
        lower = active_assessment$agility_lower
      )
    )


  })

  # Widget to widget mapping of what body sections correspond to which stat bar
  agility_mapping <- c("in_out", "cod","initial_burst")
  agility_levels <- c("top", "middle", "lower")

  speed_mapping <- c("forty", "twenty", "ten", "five")
  speed_levels <- c("top", "middle", "lower", "lowest")

  # Inter widget bindings. State changes on one widget
  # will cascade to the other widgets.
  observeEvent(agility_bars$state$active, {
    speed_bars$state$active <- c()
    speed_agility_chart$state$active <- c(agility_mapping[agility_bars$state$active])
  })

  observeEvent(speed_bars$state$active, {
    agility_bars$state$active <- c()
    speed_agility_chart$state$active <- c(speed_mapping[speed_bars$state$active])
  })

  # Vector of coordinates regarding selected body part, e.g. c("left", "top")
  speed_agility_chart_coordinates <- reactive({
    if(!is.null(speed_bars$state$active)) {
      c("left", speed_levels[[speed_bars$state$active]])
    } else if(!is.null(agility_bars$state$active)) {
      c("right", agility_levels[[agility_bars$state$active]])
    }
  })


  observeEvent(speed_agility_chart$state$active, {
    if (!is.na(which(speed_mapping == speed_agility_chart$state$active, arr.ind = TRUE)[1])) {
      agility_bars$state$active <- c()
      speed_bars$state$active <- c(which(speed_mapping == speed_agility_chart$state$active, arr.ind = TRUE)[1])
    }
  })

  observeEvent(speed_agility_chart$state$active, {
    if (!is.na(which(agility_mapping == speed_agility_chart$state$active, arr.ind = TRUE)[1])) {
      speed_bars$state$active <- c()
      agility_bars$state$active <- c(which(agility_mapping == speed_agility_chart$state$active, arr.ind = TRUE)[1])
    }
  })
}

#' Class representing the explosive page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
reachPage <- R6Class("reachPage",
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
    #' Create a new explosivePage object.
    #' @param id Unique ID for the page. Also used for namespacing the server module.
    #' @param dataset Title Initial data to be passed to the page.
    #'
    #' @return A new `explosivePage` object.
    initialize = function(id, dataset = NULL) {
      if(!is.null(dataset)) self$data$dataset <- dataset

      self$ui = ui(id)
      self$server = function() {
        callModule(server, id, self$data, self$active_player)
      }
    }
  )
)
reachPage <- reachPage$new
