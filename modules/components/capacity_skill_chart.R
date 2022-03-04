import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("imola")

export("capacityskillChart")

#' Creates the UI for a stat box.
#'
#' @description Used by the bodyChart class to generate ui.
#'
#' @param group Corresponding Body section.
#' @param position_h Horizontal position in the widget, possible values are:
#'  left, right
#' @param position_v Vertical position in the widget, possible values are:
#'  top, middle, bottom
#' @param value Value to be displayed in the box.
#' @param label Text to be shown when the box is active.
#'
#' @return A UI definition.
stat_wrapper <- function(group, position_h, position_v, value, label) {
  div(
    `data-group` = group,
    class = glue::glue("{position_h}_{position_v} stat_value capacity_stat stat_{position_v} stat_{position_h}"),
    value,
    span(class = "stat_label", label)
  )
}

#' Creates the css styles for the default body parts according to the provided options.
#'
#' @description Used by the bodyChart class ui.
#'
#' @param id The widget ID.
#' @param color The default body part color.
#'
#' @return A style tag.
default_body_style <- function(id, color) {
  tags$style(glue::glue('
    #{id} [data-part="section"],
    #{id} [data-part="section"] path,
    #{id} [data-part="section"] polygon,
    #{id} [data-part="section"] line,
    #{id} [data-part="section"] rect
    {{ fill : {color};
       stroke : {color}  
    }}
  '))
}

#' Creates the css styles for the active body parts according to the provided options.
#'
#' @description Used by the bodyChart class ui.
#'
#' @param ns The widget namespace function.
#' @param state The widget state.
#'
#' @return A group of style tags.
active_body_style <- function(ns, state) {
  tagList(
    lapply(seq_len(length(state$active)), function(index) {
      tags$style(glue::glue('
        .{ns("grid")} .capacity_stat[data-group="{state$active[index]}"]
          {{
            border : 2px solid {state$options$active_color};
            color: {state$options$active_color};
          }}
        .{ns("grid")} .capacity_stat[data-group="{state$active[index]}"] .stat_label {{
            opacity: 1;
          }}
        #{ns("capacity_wrapper")} [data-part="section"][data-group="{state$active[index]}"],
        #{ns("capacity_wrapper")} [data-part="section"][data-group="{state$active[index]}"] path,
        #{ns("capacity_wrapper")} [data-part="section"][data-group="{state$active[index]}"] polygon,
        #{ns("capacity_wrapper")} [data-part="section"][data-group="{state$active[index]}"] line,
        #{ns("capacity_wrapper")} [data-part="section"][data-group="{state$active[index]}"] rect
        {{ fill: {state$options$active_color};
           stroke: {state$options$active_color}
        }}
      '))
    })
  )
}

#' Creates the javascript event functions for the stat boxes and body parts.
#'
#' @description Used by the bodyChart class ui.
#'
#' @param id The widget ID.
#'
#' @return A script tag.
selectionCallbacks <- function(id) {
  tags$script(glue::glue('
    $("#{id}").on("click", "[data-part]", function() {{
      Shiny.setInputValue("{id}-part_selected", $(this).data("group"), {{priority : "event"}});
    }})
    $("#{id}").on("click", ".capacity_stat", function() {{
      Shiny.setInputValue("{id}-part_selected", $(this).data("group"), {{priority : "event"}});
    }})
  '))
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
    class = glue::glue("capacity_chart {ns('grid')}"),
    areas = c(
      "... ...         ...   ...   ...   ... ...",
      "... ...         ...   total ...   ... ...",
      "... ...        title title title  ... ...",
      "left_upper  body  body  body  body  body right_upper",
      "...         body  body  body  body  body ...",
      "left_middle body  body  body  body  body right_middle",
      "...         body  body  body  body  body ...",
      "left_bottom body  body  body  body  body right_bottom",
      "...         ...   ...   ...   ...   ...  ..."
    ),
    rows = "25px 50px 0.5fr 50px .5fr 50px .5fr 50px .5fr",

    div(
      id = ns("capacity_wrapper"),
      class = "body background_body",
      htmltools::htmlTemplate(
        "modules/templates/capacity_skill_wr.svg"  ### this is where we can dynamically change the image? # path naming has to be consistent across skill SVGs
      ),
      default_body_style(id, options$color),
      uiOutput(ns("active_body_style"))
    ),

    div(class = "title", uiOutput(ns("title"))),
    div(class = "total", uiOutput(ns("total"))),


    stat_wrapper("long-distance", "left", "upper", uiOutput(ns("left_top")), options$labels$left$top),
    stat_wrapper("medium-distance", "left", "middle", uiOutput(ns("left_middle")), options$labels$left$middle),
    stat_wrapper("short-distance", "left", "bottom", uiOutput(ns("left_bottom")), options$labels$left$bottom),

    stat_wrapper("skill-one", "right", "upper", uiOutput(ns("right_top")), options$labels$right$top),
    stat_wrapper("skill-two", "right", "middle", uiOutput(ns("right_middle")), options$labels$right$middle),
    stat_wrapper("skill-three", "right", "bottom", uiOutput(ns("right_bottom")), options$labels$right$bottom),


    selectionCallbacks(id)
  )
}


#' Creates the Server for the capacity skill chart widget.
#'
#' @description Internal module under the widget id namespace. Run internal
#'   widget updates and state changes that are widget specific. Responsible
#'   for updating the widget data and ui elements.
#'
#' @return A server module that can be initialized from the application
#'   server function.
server <- function(input, output, session, state) {
  ns <- session$ns

  output$title <- renderUI({
    state$options$title
  })

  observeEvent(state$values, {
    output$total_score <- renderUI({ span(state$values$total) })
    output$total <- renderUI({ span(state$values$total) })
    output$left_top <- renderUI({ span(state$values$left$top) })
    output$left_middle <- renderUI({ span(state$values$left$middle) })
    output$left_bottom <- renderUI({ span(state$values$left$bottom) })
    output$right_top <- renderUI({ span(state$values$right$top) })
    output$right_middle <- renderUI({ span(state$values$right$middle) })
    output$right_bottom <- renderUI({ span(state$values$right$bottom) })


    observeEvent(state$active, {
      output$active_body_style <- renderUI({
        active_body_style(ns, state)
      })
    })

    observeEvent(input$part_selected, {
      state$active <- c(input$part_selected)
    })
  })
}

#' Class representing a body shape chart.
#'
#' A bodyChart object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
capacityskillChart <- R6Class("capacityskillChart",
  public = list(

    #' @description
    #' Calls the ui for the widget instance.
    #' @param id Namespaced id. When calling UI for inside a diferent module,
    #'   sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
    #' @examples
    #' test_chart <- use("forty_agility_chart.R")$speedagilityChart("test")
    #' test_chart$ui(ns("test"))
    ui = NULL,

    #' @description
    #' Calls the server module for the widget instance.
    server = NULL,

    state = reactiveValues(
      id = NULL,
      options = list(
        title = "Capacity Score",
        color = "black",
        active_color = "lightslategray",
        labels = list(
          left = list(top = "Long Distance", middle = "Medium Distance", bottom = "Short Distance"),  ### here is where we dynamically change label
          right = list(top = "Skill One", middle = "Skill Two", bottom = "Skill Three")
        )
      ),
      values = list(
        total = 0,
        left = list(top = 0, middle = 0, bottom = 0),
        right = list(top =0, middle = 0, bottom = 0)
      ),
      active = c()
    ),

    #' @description
    #' Create a new capacityChart object.
    #' @param id Unique ID for the widget instance. Also used for namespacing the server module.
    #' @param options A named list of options that will overwrite the default state$options.
    #'   Partial named lists will result in only some options being overwritten while
    #'   non named ones stay with the default values.
    #' @param values Optional initial values to use for the bars and widget side label.
    #' @return A new `capacityChart` object.
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
capacityskillChart <- capacityskillChart$new
