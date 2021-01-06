import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")

export("capacityPage")

cards_descriptions <- list(
  fitness_card = "Capacity is the ability to maintain running speed, make sure
  you eat your vegetables to have it.",
  details_card = "To improve capacity, run a lot and then also do things specific
  to your position - then you'll be good.",
  skill_card = "Skill is how good you are at your position. Practice a ton and if
  you are good it'll help. If not, sorry dude."
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

#' Creates the UI for the capacity page.
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
      class = "capacity_page_wrapper",
      areas = c("fitness_chart capacity_skill_chart skill_chart"),
      columns = "1fr minmax(500px, 2.7fr) 1fr",
      gap = "20px",

      uiOutput(ns("fitness_bars")) %>%
        tagAppendAttributes(class = "fitness_chart"),

      uiOutput(ns("capacity_skill_chart")) %>%
        tagAppendAttributes(class = "capacity_skill_chart"),

      uiOutput(ns("skill_bars")) %>%
        tagAppendAttributes(class = "skill_chart")
    ),
    gridPanel(
      class = "capacity_page_wrapper",
      areas = c("fitness_card details_card skill_card"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",

      uiOutput(ns("fitness_card")) %>%
        tagAppendAttributes(class = "fitness_card"),

      uiOutput(ns("details_card")) %>%
        tagAppendAttributes(class = "details_card"),

      uiOutput(ns("skill_card")) %>%
        tagAppendAttributes(class = "skill_card")
    )
  )
}

#' Creates the Server for the capacity page.
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
    color = "#B46E47",
    background = "#E0BBA2",
    active_color = "#F66733",
    active_background = "#E0BBA2"
  )

  fitness_bars <- use("modules/components/stat_chart.R")$statChart(
    "fitness_chart", "Fitness",
    "icons/BA_Fitness.svg", 3, chart_options
  )

  skill_bars <- use("modules/components/stat_chart.R")$statChart(
    "skill_chart", "Skill",
    "icons/BA_Skill.svg", 3, chart_options
  )

  capacity_skill_chart <- use("modules/components/capacity_skill_chart.R")$capacityskillChart(
    "capacity_skill_chart",
    list(
      color = "#C4C4C4",
      active_color = "#F66733",
      labels = list(
        left = list(
          top = "Long Distance",
          middle = "Medium Distance",
          bottom = "Short Distance"
        ),
        right = list(
          top = "Skill One",
          middle = "Skill Two",
          bottom = "Skill Three"
        )
      )
    )
  )



  fitness_bars$server()
  skill_bars$server()
  capacity_skill_chart$server()

  output$fitness_bars <- renderUI({ fitness_bars$ui(ns("fitness_chart")) })
  output$skill_bars <- renderUI({ skill_bars$ui(ns("skill_chart")) })
  output$capacity_skill_chart <- renderUI({ capacity_skill_chart$ui(ns("capacity_skill_chart")) })


  # Text cards with the information about overall strength, overall power and
  # detailed information about selected part of the body
  text_card <- use("modules/components/text_card.R")$text_card
  capacity_skill_side <- reactive({ capacity_skill_chart_coordinates()[1] })
  capacity_skill_level <- reactive({ capacity_skill_chart_coordinates()[2] })

  # UI of Fitness card on the left side
  output$fitness_card <- renderUI({
    text_card(
      "Fitness",
      fitness_bars$state$values$total,
      cards_descriptions$fitness_card,
      class = "custom-class"
    )
  })

  # UI of details card in the middle
  output$details_card <- renderUI({
    if(!is.null(capacity_skill_side()) & !is.null(capacity_skill_side())) {
      text_card(
        capacity_skill_chart$state$options$labels[[capacity_skill_side()]][[capacity_skill_level()]],
        capacity_skill_chart$state$values[[capacity_skill_side()]][[capacity_skill_level()]],
        cards_descriptions$details_card,
        class = "custom-class"
      )
    }

  })

  # UI of skill card on the right side
  output$skill_card <- renderUI({
    text_card(
      "Skill",
      skill_bars$state$values$total,
      cards_descriptions$skill_card,
      class = "custom-class"
    )
  })

  observeEvent(active_player$id, {
    print(active_player$id)

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

    ## This is where we map the stored data to the bars and chart
    fitness_bars$state$values <- list(
      total = active_assessment$fitness_score,
      bars = c(
        active_assessment$fitness_upper,
        active_assessment$fitness_core,
        active_assessment$fitness_lower
      )
    )

    skill_bars$state$values <- list(
      total = active_assessment$skill_score,
      bars = c(
        active_assessment$skill_upper,
        active_assessment$skill_core,
        active_assessment$skill_lower
      )
    )

    capacity_skill_chart$state$values <- list(
      total = active_assessment$capacity_score,
      left = list(
        top = active_assessment$fitness_upper,
        middle = active_assessment$fitness_core,
        bottom = active_assessment$fitness_lower
      ),
      right = list(
        top = active_assessment$skill_upper,
        middle = active_assessment$skill_core,
        bottom = active_assessment$skill_lower
      )
    )


  })

  # Widget to widget mapping of what body sections correspond to which stat bar
  skill_mapping <- c("skill-one", "skill-two","skill-three")
  skill_levels <- c("top", "middle", "bottom")

  fitness_mapping <- c("long-distance", "medium-distance", "short-distance")
  fitness_levels <- c("top", "middle", "bottom")

  # Inter widget bindings. State changes on one widget
  # will cascade to the other widgets.
  observeEvent(skill_bars$state$active, {
    fitness_bars$state$active <- c()
    capacity_skill_chart$state$active <- c(skill_mapping[skill_bars$state$active])
  })

  observeEvent(fitness_bars$state$active, {
    skill_bars$state$active <- c()
    capacity_skill_chart$state$active <- c(fitness_mapping[fitness_bars$state$active])
  })

  # Vector of coordinates regarding selected body part, e.g. c("left", "top")
  capacity_skill_chart_coordinates <- reactive({
    if(!is.null(fitness_bars$state$active)) {
      c("left", fitness_levels[[fitness_bars$state$active]])
    } else if(!is.null(skill_bars$state$active)) {
      c("right", skill_levels[[skill_bars$state$active]])
    }
  })


  observeEvent(capacity_skill_chart$state$active, {
    if (!is.na(which(fitness_mapping == capacity_skill_chart$state$active, arr.ind = TRUE)[1])) {
      skill_bars$state$active <- c()
      fitness_bars$state$active <- c(which(fitness_mapping == capacity_skill_chart$state$active, arr.ind = TRUE)[1])
    }
  })

  observeEvent(capacity_skill_chart$state$active, {
    if (!is.na(which(skill_mapping == capacity_skill_chart$state$active, arr.ind = TRUE)[1])) {
      fitness_bars$state$active <- c()
      skill_bars$state$active <- c(which(skill_mapping == capacity_skill_chart$state$active, arr.ind = TRUE)[1])
    }
  })
}

#' Class representing the explosive page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
capacityPage <- R6Class("capacityPage",
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
    #' Create a new capacityPage object.
    #' @param id Unique ID for the page. Also used for namespacing the server module.
    #' @param dataset Title Initial data to be passed to the page.
    #'
    #' @return A new `capacityPage` object.
    initialize = function(id, dataset = NULL) {
      if(!is.null(dataset)) self$data$dataset <- dataset

      self$ui = ui(id)
      self$server = function() {
        callModule(server, id, self$data, self$active_player)
      }
    }
  )
)
capacityPage <- capacityPage$new
