import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")

export("balanceChart")

#' Creates the UI for a stat box.
#'
#' @description Used by the balanceChart class to generate ui.
#'
#' @param group Corresponding score section.
#' @param position_v Exercise (thus horiziontal position) in the widget, possible values are:
#'  fms, ds, tspu, hs, ill, sm, aslr, rs
#' @param position_h Type of score (thus vertical position) in the widget, possible values are:
#'  score, left_score, right_score, diff_score
#' @param value Value to be displayed in the box.
#' @return A UI definition.
stat_wrapper <- function(group, position_h, position_v, value) {
  div(
    `data-group` = group,
    class = glue::glue("{position_h}_{position_v} stat_value"),
    value
  )
}

exercise_wrapper <- function(group, exercise, number, value) {
  div(
    `exercise-group` = group, 
    class = glue::glue("{exercise}_{number} exercise_value balance_exercise"),
    value  
  )
}


#' Creates the UI for the body widget.
#'
#' @description Used by the statChart class to generate the corresponding
#'   widget ui.
#'
#' @param id widget id.
#' @param options Ui settings for initializing the widget ui.
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
ui <- function(id, options) {
  ns <- NS(id)
  
  gridPanel(
    id = id,
    areas = c(
      " .... ",
      "total",
      "title",
      "header",
      " .... ",
      "balance_chart"
    ),
    rows = "25px 50px 25px 50px 10px 1fr",
    
    div(class = "title", uiOutput(ns("title"))),
    div(class = "total", uiOutput(ns("total_score"))),
    
    div(
      id = id,
      class = glue::glue("header {ns('grid')}"),
      gridPanel(
        areas = c(
          "exercise_header left_header right_header diff_header"
        ),
        
        uiOutput(ns("exercise_header")),
        uiOutput(ns("left_header")),
        uiOutput(ns("right_header")),
        uiOutput(ns("diff_header"))
      ),
    ),
    div(
      id = id,
      class = glue::glue("balance_chart {ns('grid')}"),
      gridPanel(
        areas = c(
          " exercise_one fms_score fms_score fms_score ... ... ...",
          " exercise_two ds_score ds_score ds_score ... ... ...",
          " exercise_three tspu_score tspu_score tspu_score ... ... ... ",
          " exercise_four hs_left_score ... hs_right_score ... hs_diff_score ... ",
          " exercise_five ill_left_score ... ill_right_score ... ill_diff_score ...",
          " exercise_six sm_left_score ... sm_right_score ... sm_diff_score ...",
          " exercise_seven aslr_left_score ... aslr_right_score ... aslr_diff_score ...",
          " exercise_eight rs_left_score ... rs_right_score ... rs_diff_score ...",
          " exercise_nine max_imbalance_score max_imbalance_score max_imbalance_score ... ... ... ",
          " exercise_ten impulse_imbalance_score impulse_imbalance_score  impulse_imbalance_score ... ... ...",
          " exercise_eleven fms_asym_score fms_asym_score fms_asym_score ... ... ..."
        ),
        rows = "1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr",
        columns = "300px 1fr 10px 1fr 10px 1fr 10px ",
        gap = "2px",

        exercise_wrapper("exercise_one", "exercise", "one", uiOutput(ns("exercise_one"))),
        exercise_wrapper("exercise_two", "exercise", "two", uiOutput(ns("exercise_two"))),
        exercise_wrapper("exercise_three", "exercise", "three", uiOutput(ns("exercise_three"))),
        exercise_wrapper("exercise_four", "exercise", "four", uiOutput(ns("exercise_four"))),
        exercise_wrapper("exercise_five", "exercise", "five", uiOutput(ns("exercise_five"))),
        exercise_wrapper("exercise_six", "exercise", "six", uiOutput(ns("exercise_six"))),
        exercise_wrapper("exercise_seven", "exercise", "seven", uiOutput(ns("exercise_seven"))),
        exercise_wrapper("exercise_eight", "exercise", "eight", uiOutput(ns("exercise_eight"))),
        exercise_wrapper("exercise_nine", "exercise", "nine", uiOutput(ns("exercise_nine"))),
        exercise_wrapper("exercise_ten", "exercise", "ten", uiOutput(ns("exercise_ten"))),
        exercise_wrapper("exercise_eleven", "exercise", "eleven", uiOutput(ns("exercise_eleven"))),
          
        stat_wrapper("fms_score_full", "fms", "score", uiOutput(ns("fms_score"))),
        stat_wrapper("fms_score", "ds", "score", uiOutput(ns("ds_score"))),
        stat_wrapper("fms_score", "tspu", "score", uiOutput(ns("tspu_score"))),

        stat_wrapper("fms_score", "hs", "left_score", uiOutput(ns("hs_left_score"))),
        stat_wrapper("fms_score", "hs", "right_score", uiOutput(ns("hs_right_score"))),
        stat_wrapper("fms_score", "hs", "diff_score", uiOutput(ns("hs_diff_score"))),

        stat_wrapper("fms_score", "ill", "left_score", uiOutput(ns("ill_left_score"))),
        stat_wrapper("fms_score", "ill", "right_score", uiOutput(ns("ill_right_score"))),
        stat_wrapper("fms_score", "ill", "diff_score", uiOutput(ns("ill_diff_score"))),

        stat_wrapper("fms_score", "sm", "left_score", uiOutput(ns("sm_left_score"))),
        stat_wrapper("fms_score", "sm", "right_score", uiOutput(ns("sm_right_score"))),
        stat_wrapper("fms_score", "sm", "diff_score", uiOutput(ns("sm_diff_score"))),

        stat_wrapper("fms_score", "aslr", "left_score", uiOutput(ns("aslr_left_score"))),
        stat_wrapper("fms_score", "aslr", "right_score", uiOutput(ns("aslr_right_score"))),
        stat_wrapper("fms_score", "aslr", "diff_score", uiOutput(ns("aslr_diff_score"))),

        stat_wrapper("fms_score", "rs", "left_score", uiOutput(ns("rs_left_score"))),
        stat_wrapper("fms_score", "rs", "right_score", uiOutput(ns("rs_right_score"))),
        stat_wrapper("fms_score", "rs", "diff_score", uiOutput(ns("rs_diff_score"))),

        stat_wrapper("standard", "max_imbalance", "score", uiOutput(ns("max_imbalance_score"))),
        stat_wrapper("standard", "impulse_imbalance", "score", uiOutput(ns("impulse_imbalance_score"))),
        stat_wrapper("standard", "fms_asym", "score", uiOutput(ns("fms_asym_score")))
      )
    )
  )
}


color_decision <- function(group, stat_value){
  if (length(stat_value) != 0) {
    if (!is.na(stat_value)) {
      if (group == "fms_score") {
        if (stat_value == 3){
          return("green")
        }
        else if (stat_value == 2){
          return("yellow")
        }
        else if (stat_value == 1){
          return("red")
        }
      }
      else if (group == "fms_score_full"){
        if (stat_value > 17){
          return("green")
        }
        else if (stat_value > 13){
          return("yellow")
        }
        else if (stat_value <= 13){
          return("red")
        }
      }
      else if (group == "differences"){
        if (stat_value == 0){
          return("green")
        }
        else (
          return("red")
        )
      }
    }
    else {
      return("black")
    }
  }
  else {
    return("black")
  }
}

#' Creates the Server for the body chart widget.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
server <- function(input, output, session, state) {
  ns <- session$ns
  
  output$title <- renderUI({state$options$title})
  
  output$exercise_header <- renderUI({state$options$labels$headers$left})
  output$left_header <- renderUI({state$options$labels$headers$left_middle})
  output$right_header <- renderUI({state$options$labels$headers$right_middle})
  output$diff_header <- renderUI({state$options$labels$headers$right})
  
  output$exercise_one <- renderUI({state$options$labels$exercise$one})
  output$exercise_two <- renderUI({state$options$labels$exercise$two})
  output$exercise_three <- renderUI({state$options$labels$exercise$three})
  output$exercise_four <- renderUI({state$options$labels$exercise$four})
  output$exercise_five <- renderUI({state$options$labels$exercise$five})
  output$exercise_six <- renderUI({state$options$labels$exercise$six})
  output$exercise_seven <- renderUI({state$options$labels$exercise$seven})
  output$exercise_eight <- renderUI({state$options$labels$exercise$eight})
  output$exercise_nine <- renderUI({state$options$labels$exercise$nine})
  output$exercise_ten <- renderUI({state$options$labels$exercise$ten})
  output$exercise_eleven <- renderUI({state$options$labels$exercise$eleven})
  
  observeEvent(state$values, {
    
    output$total_score <- renderUI({ span(state$values$total) })
    output$fms_score <- renderUI({div(class = "score", `data-color` = color_decision("fms_score_full", state$values$data$fms_score), span(state$values$data$fms_score)) })
    
    output$ds_score <- renderUI({div(class = "score", `data-color` = color_decision("fms_score", state$values$data$ds_score), span(state$values$data$ds_score)) })
    output$tspu_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$tspu_score), span(state$values$data$tspu_score)) })
    
    output$hs_left_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$hs_left_score), span(state$values$data$hs_left_score)) })
    output$hs_right_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$hs_right_score), span(state$values$data$hs_right_score)) })
    output$hs_diff_score <- renderUI({ div(class = "score", `data-color` = color_decision("differences", state$values$data$hs_diff_score), span(state$values$data$hs_diff_score)) })
   
    output$ill_left_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$ill_left_score), span(state$values$data$ill_left_score)) })
    output$ill_right_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$ill_right_score), span(state$values$data$ill_right_score)) })
    output$ill_diff_score <- renderUI({ div(class = "score", `data-color` = color_decision("differences", state$values$data$ill_diff_score), span(state$values$data$ill_diff_score)) })
    
    output$sm_left_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$sm_left_score), span(state$values$data$sm_left_score)) })
    output$sm_right_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$sm_right_score), span(state$values$data$sm_right_score)) })
    output$sm_diff_score <- renderUI({ div(class = "score", `data-color` = color_decision("differences", state$values$data$sm_diff_score), span(state$values$data$sm_diff_score)) })
    
    output$aslr_left_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$aslr_left_score), span(state$values$data$aslr_left_score)) })
    output$aslr_right_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$aslr_right_score), span(state$values$data$aslr_right_score)) })
    output$aslr_diff_score <- renderUI({ div(class = "score", `data-color` = color_decision("differences", state$values$data$aslr_diff_score), span(state$values$data$aslr_diff_score)) })
    
    output$rs_left_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$rs_left_score), span(state$values$data$rs_left_score)) })
    output$rs_right_score <- renderUI({ div(class = "score", `data-color` = color_decision("fms_score", state$values$data$rs_right_score), span(state$values$data$rs_left_score)) })
    output$rs_diff_score <- renderUI({ div(class = "score", `data-color` = color_decision("differences", state$values$data$rs_diff_score), span(state$values$data$rs_diff_score)) })

    output$max_imbalance_score <- renderUI({ div(class = "score", `data-color` = "black", span(state$values$data$max_imbalance_score)) })
    output$impulse_imbalance_score <- renderUI({ div(class = "score", `data-color` = "black", span(state$values$data$impulse_imbalance_score)) })
    output$fms_asym_score <- renderUI({ div(class = "score", `data-color` = "black", span(state$values$data$fms_asym_score)) })

    observeEvent(input$part_selected, {
      state$active <- c(input$part_selected)
      print(paste0("You have chosen: ", input$part_selected))
    })
  })
}
#' Class representing a body shape chart.
#'
#' A bodyChart object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
balanceChart <- R6Class("balanceChart",
  public = list(
    #' @description
    #' Calls the ui for the widget instance.
    #' @param id Namespaced id. When calling UI for inside a diferent module,
    #' sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
    #'@examples
    #'test_chart <- use("forty_agility_chart.R")$speedagilityChart("test")
    #'test_chart$ui(ns("test"))
    ui = NULL,
    
    #' @description
    #' #' Calls the server module for the widget instance.
    server = NULL,
    state = reactiveValues(
      id = NULL,
      options = list(
        title = "Balance Score",
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
            nine = "Max Imbalace",
            ten = "Impulse Imbalance",
            eleven = "FMS Asymmetry"
            ),
          headers = list(
            left = "Exercise", 
            left_middle = "Right", 
            right_middle = "Left", 
            right = "Difference"
            )
          )
        ),
      values = list(
        total = 0,
        data = list(
          fms_score = 0,
          ds_score = 0,
          tspu_score = 0,
          hs_left_score = 0,
          hs_right_score = 0,
          hs_diff_score = 0,
          ill_left_score = 0,
          ill_right_score = 0,
          ill_diff_score = 0,
          sm_left_score = 0,
          sm_right_score = 0,
          sm_diff_score = 0,
          aslr_left_score = 0,
          aslr_right_score = 0,
          aslr_diff_score = 0,
          rs_left_score = 0,
          rs_right_score = 0,
          rs_diff_score = 0,
          max_imbalance_score = 0,
          impulse_imbalance_score = 0,
          fms_asym_score = 0
          )
        ),
      active = c()
      ),
    #' @description
    #' Create a new bodyChart object.
    #' @param id Unique ID for the widget instance. Also used for namespacing the server module.
    #' @param options A named list of options that will overwrite the default state$options.
    #' Partial named lists will result in only some options being overwritten while
    #' non named ones stay with the default values.
    #' @param values Optional initial values to use for the bars and widget side label.
    #' @return A new `bodyChart` object.
    initialize = function(id, options = NULL, values = NULL) {
      isolate({
        self$state$id <- id
        if(!is.null(values)) self$state$values <- modifyList(self$state$values, values)
        if(!is.null(options)) self$state$options <- options$style <- modifyList(self$state$options, options)
        })
      self$ui = function(id) {
        ui(id, isolate(self$state$options))
        }
      self$server = function() {
        callModule(server, id, self$state)
        }
      }
  )
)
balanceChart <- balanceChart$new
