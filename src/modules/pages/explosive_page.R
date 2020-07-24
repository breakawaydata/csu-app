import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")

export("explosivePage")

cards_descriptions <- list(
  strength_card = "Strength Card Test Description Strength Card Test Description
  Strength Card Test Description Strength Card Test Description Strength Card
  Test Description",
  details_card = "Details Card Test Description Details Card Test Description
  Details Card Test Description Details Card Test Description Details Card Test
  Description",
  power_card = "Power Card Test Description Power Card Test Description Power
  Card Test Description Power Card Test Description Power Card Test Description"
)
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
#' @description Used by the explosivePage class to generate the corresponding ui.
#'
#' @param id widget id.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id) {
  ns <- NS(id)

  div(
    uiOutput(ns("assessment_report_toggle")),
    gridPanel(
      class = "explosive_page_wrapper",
      areas = c("strength_chart body_chart power_chart"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",

      uiOutput(ns("strenght_bars")) %>%
        tagAppendAttributes(class = "strength_chart"),

      uiOutput(ns("body_chart")) %>%
        tagAppendAttributes(class = "body_chart"),

      uiOutput(ns("power_bars")) %>%
        tagAppendAttributes(class = "power_chart")
    ),
    gridPanel(
      class = "explosive_page_wrapper",
      areas = c("strength_card details_card power_card"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",

      uiOutput(ns("strength_card")) %>%
        tagAppendAttributes(class = "strength_card"),

      uiOutput(ns("details_card")) %>%
        tagAppendAttributes(class = "details_card"),

      uiOutput(ns("power_card")) %>%
        tagAppendAttributes(class = "power_card")
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

  strenght_bars <- use("modules/components/stat_chart.R")$statChart(
    "strength_chart", "Strength",
    "icons/BA_Strength.svg", 3, chart_options
  )

  power_bars <- use("modules/components/stat_chart.R")$statChart(
    "power_chart", "Power",
    "icons/BA_Power.svg", 3, chart_options
  )

  body_chart <- use("modules/components/body_chart.R")$bodyChart(
    "body_chart",
    list(
      color = "#C4C4C4",
      active_color = "#F66733",
      labels = list(
        left = list(
          top = "Upper Strength",
          middle = "Core Strength",
          bottom = "Lower Strength"
        ),
        right = list(
          top = "Upper Power",
          middle = "Core Power",
          bottom = "Lower Power"
        )
      )
    )
  )

  strenght_bars$server()
  power_bars$server()
  body_chart$server()

  output$strenght_bars <- renderUI({ strenght_bars$ui(ns("strength_chart")) })
  output$power_bars <- renderUI({ power_bars$ui(ns("power_chart")) })
  output$body_chart <- renderUI({ body_chart$ui(ns("body_chart")) })

  # Text cards with the information about overall strength, overall power and
  # detailed information about selected part of the body
  text_card <- use("modules/components/text_card.R")$text_card
  body_part_side <- reactive({ body_part_coordinates()[1] })
  body_part_level <- reactive({ body_part_coordinates()[2] })

  # UI of strength card on the left side
  output$strength_card <- renderUI({ text_card(
    "Strength",
    strenght_bars$state$values$total,
    cards_descriptions$strength_card,
    class = "custom-class"
  )})

  # UI of details card in the middle
  output$details_card <- renderUI({
    if(!is.null(body_part_side()) & !is.null(body_part_level())) {
      text_card(
        body_chart$state$options$labels[[body_part_side()]][[body_part_level()]],
        body_chart$state$values[[body_part_side()]][[body_part_level()]],
        cards_descriptions$details_card,
        class = "custom-class"
      )
    }
  })

  # UI of power card on the right side
  output$power_card <- renderUI({ text_card(
    "Power",
    power_bars$state$values$total,
    cards_descriptions$power_card,
    class = "custom-class"
  )})

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

    strenght_bars$state$values <- list(
      total = active_assessment$strength_score,
      bars = c(
        active_assessment$strength_upper,
        active_assessment$strength_core,
        active_assessment$strength_lower
      )
    )

    power_bars$state$values <- list(
      total = active_assessment$power_score,
      bars = c(
        active_assessment$power_upper,
        active_assessment$power_core,
        active_assessment$power_lower
      )
    )

    body_chart$state$values <- list(
      total = active_assessment$explosion_score,
      left = list(
        top = active_assessment$strength_upper,
        middle = active_assessment$strength_core,
        bottom = active_assessment$strength_lower
      ),
      right = list(
        top = active_assessment$power_upper,
        middle = active_assessment$power_core,
        bottom = active_assessment$power_lower
      )
    )
  })

  # Widget to widget mapping of what body sections correspont to which stat bar
  power_mapping <- c("right_arm", "bottom_right_torso", "right_leg")
  strength_mapping <- c("left_arm", "bottom_left_torso", "left_leg")
  body_levels <- c("top", "middle", "bottom")

  # Inter widget bindings. State changes on one widget
  # will cascade to the other widgets.
  observeEvent(power_bars$state$active, {
    strenght_bars$state$active <- c()
    body_chart$state$active <- c(power_mapping[power_bars$state$active])
  })

  observeEvent(strenght_bars$state$active, {
    power_bars$state$active <- c()
    body_chart$state$active <- c(strength_mapping[strenght_bars$state$active])
  })

  # Vector of coordinates regarding selected body part, e.g. c("left", "top")
  body_part_coordinates <- reactive({
    if(!is.null(strenght_bars$state$active)) {
      c("left", body_levels[[strenght_bars$state$active]])
    } else if(!is.null(power_bars$state$active)) {
      c("right", body_levels[[power_bars$state$active]])
    }
  })

  observeEvent(body_chart$state$active, {
    if (!is.na(which(power_mapping == body_chart$state$active, arr.ind = TRUE)[1])) {
      strenght_bars$state$active <- c()
      power_bars$state$active <- c(which(power_mapping == body_chart$state$active, arr.ind = TRUE)[1])
    }
    if (!is.na(which(strength_mapping == body_chart$state$active, arr.ind = TRUE)[1])) {
      power_bars$state$active <- c()
      strenght_bars$state$active <- c(which(strength_mapping == body_chart$state$active, arr.ind = TRUE)[1])
    }
  })
}

#' Class representing the explosive page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
explosivePage <- R6Class("explosivePage",
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
explosivePage <- explosivePage$new
