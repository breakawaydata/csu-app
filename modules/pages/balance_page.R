import("R6")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("imola")

export("balancePage")

cards_descriptions <- list(
  mobility_card = "Propery mobility allows the body to move freely without 
  sacraficing stability and is part of the foundation for efficient power production.",
  stability_card = "Stability is required for the body to remain balanced 
  without being disturbed by outside forces."
)

#' Creates the styling for an active assessment_report menu item.
#'
#' @description Used by the balancePage class to generate ui.
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
#' @description Used by the balancePage class to generate ui.
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

#' Creates the UI for the balance page.
#'
#' @description Used by the balancePage class to generate the corresponding ui.
#'
#' @param id widget id.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id) {
  ns <- NS(id)
  
  div(
    uiOutput(ns("assessment_report_toggle")),
    gridPanel(
      class = "balance_page_wrapper",
      areas = c("mobility_chart balance_chart stability_chart"),
      columns = "1fr minmax(500px, 2.7fr) 1fr",
      gap = "20px",
      
      uiOutput(ns("mobility_bars")) %>%
        tagAppendAttributes(class = "mobility_chart"),
      
      uiOutput(ns("balance_chart")) %>%
        tagAppendAttributes(class = "balance_chart"),
      
      uiOutput(ns("stability_bars")) %>%
        tagAppendAttributes(class = "stability_chart")
    ),
    
    gridPanel(
      class = "balance_page_wrapper",
      areas = c("mobility_card ... stability_card"),
      columns = "1fr minmax(500px, 2fr) 1fr",
      gap = "20px",
      
      uiOutput(ns("mobility_card")) %>%
        tagAppendAttributes(class = "mobility_card"),
      
      uiOutput(ns("stability_card")) %>%
        tagAppendAttributes(class = "stability_card")
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
    color = "#F58A2D",
    background = "#F8B376",
    active_color = "#F66733",
    active_background = "#F8B376"
  )
  
  mobility_bars <- use("modules/components/stat_chart.R")$statChart(
    "mobility_chart", "Mobility",
    "icons/BA_Mobility.svg", 1, chart_options
  )
  
  stability_bars <- use("modules/components/stat_chart.R")$statChart(
    "stability_chart", "Stability",
    "icons/BA_Mechanics.svg", 1, chart_options
  )
  
  balance_chart <- use("modules/components/balance_chart.R")$balanceChart(
    "balance_chart",
    list(
      color = "#C4C4C4",
      active_color = "#F66733",
      labels = list(
        exercise = list(
          one = "FMS Total",
          two = "Deep Squat",
          three = "Trunk Stability Push Up",
          four = "Hurdle Step",
          five = "In Line Lunge",
          six = "Shoulder Mobility",
          seven = "Active Straight Leg Raise",
          eight = "Rotary Stability",
          nine = "Max Imbalance",
          ten = "Impulse Imbalance",
          eleven = "FMS Asymmetry"
        ),
        headers = list(
          left = "Exercise", 
          left_middle = "Left", 
          right_middle = "Right", 
          right = "Diff"
        )
      )
    )
  )
  
  mobility_bars$server()
  stability_bars$server()
  balance_chart$server()
  
  output$mobility_bars <- renderUI({ mobility_bars$ui(ns("mobility_chart")) })
  output$stability_bars <- renderUI({ stability_bars$ui(ns("stability_chart")) })
  output$balance_chart <- renderUI({ balance_chart$ui(ns("balance_chart")) })
  
  
  # Text cards with the information about overall strength, overall power and
  # detailed information about selected part of the body
  text_card <- use("modules/components/text_card.R")$text_card
  
  # UI of strength card on the left side
  output$mobility_card <- renderUI({
    text_card(
      "Mobility",
      mobility_bars$state$values$total,
      cards_descriptions$mobility_card,
      class = "custom-class"
    )
  })
  
  # UI of power card on the right side
  output$stability_card <- renderUI({
    text_card(
      "Stability",
      stability_bars$state$values$total,
      cards_descriptions$stability_card,
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
    
    mobility_bars$state$values <- list(
      total = active_assessment$mobility_score,
      bars = c(
        active_assessment$mobility_score
      )
    )
    
    stability_bars$state$values <- list(
      total = active_assessment$stability_score,
      bars = c(
        active_assessment$stability_score
      )
    )
    
    
    balance_chart$state$values <- list(
      total = active_assessment$balance_score,
      data = list(
        fms_score = active_assessment$fms_raw,
        ds_score = active_assessment$ods_raw,
        tspu_score = active_assessment$tspu_raw,
        
        hs_left_score = active_assessment$hs_left_raw,
        hs_right_score = active_assessment$hs_right_raw,
        hs_diff_score = active_assessment$hs_diff_raw,
        
        ill_left_score = active_assessment$ill_left_raw,
        ill_right_score = active_assessment$ill_right_raw,
        ill_diff_score = active_assessment$ill_diff_raw,
        
        sm_left_score = active_assessment$sm_left_raw,
        sm_right_score = active_assessment$sm_right_raw,
        sm_diff_score = active_assessment$sm_diff_raw,
        
        aslr_left_score = active_assessment$aslr_left_raw,
        aslr_right_score = active_assessment$aslr_right_raw,
        aslr_diff_score = active_assessment$aslr_diff_raw,
        
        rs_left_score = active_assessment$rs_left_raw,
        rs_right_score = active_assessment$rs_right_raw,
        rs_diff_score = active_assessment$rs_diff_raw,
        
        max_imbalance_score = active_assessment$max_imbalance_score,
        impulse_imbalance_score = active_assessment$impulse_imbalance_score,
        fms_asym_score = active_assessment$fms_asym_score
      )
    )
  })
}

#' Class representing the explosive page.
#'
#' The object contains a ui and server definition that must be called after instancing.
#' The namespace will be based on the ID provided.
balancePage <- R6Class("balancePage",
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
balancePage <- balancePage$new
