import("R6")
import("utils")
import("glue")
import("dplyr")
import("htmltools")
import("shiny")
import("shiny.grid")
import("ggplot2")

export("fingerprintChart")


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
  
  tags$style(".fingerprint_plot{
    background: #f1f1f1;
  }")
  
  plotOutput(ns("fingerprint_plot"), height = 450)
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
  
  #Make blank loops
  len <- 2
  empty_df <- data.frame(category = letters[1:len],
                         dummy = rep("", len))
  
  fingerprint_df <- data.frame(
    category = as.factor(c("Explosion", "Reach", "Balance"))) %>%
    mutate(dummy = category)
  
  #Append number to category name
  fingerprint_df <- rbind(fingerprint_df, empty_df)
  
  fingerprint_df$category <-
    factor(fingerprint_df$category,
           levels=rev(fingerprint_df$category))
  
  
  makeReactiveBinding("fingerprint_df")
  
  ## Create the DF for the fingerprint chart 
  fingerprint_df_final <- reactive({

    ## Get overall score
    fingerprint_df$value <- c(
      state$values$explosion, 
      state$values$reach, 
      state$values$balance, 
      0, 
      0)
    
    return(fingerprint_df)
  })
  
  overall_score <- reactive({
    return(state$values$total)
  })
  
  #Output plot
  output$fingerprint_plot <- renderPlot({
    datadata <- fingerprint_df_final()
    score <- overall_score()
    
    
    plot <- ggplot(datadata, aes(x = category, y = value, fill = dummy)) + 
      geom_bar(width = 0.8, stat="identity") + 
      coord_polar(theta = "y", direction = 1, start = 3.1) +
      xlab("") + ylab("") +
      scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
      scale_x_discrete(expand = c(0,0)) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "Explosion" = "#246AB1",
          "Reach" = "#C4CB38",
          "Balance" = "#F58A2D",
          " " = "white")) +
      theme_void() +
      geom_text(aes(label = score, x = 0, y = 25),   # need to update font here:: family = "Axia Stencil Black"
                size= 30) +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "lines"),
            plot.background = element_rect(fill = "#f1f1f1", color = "#f1f1f1"),
            panel.background = element_rect(fill = "#f1f1f1", color = "#f1f1f1"))
    return(plot)
    })
    
}

#' Class representing a body shape chart.
#'
#' A fingerprintChart object contains a ui and server definition that must be called after instancing.
#' Multiple independent instances can be created.
#' The name space of each instance will be based on the ID provided.
fingerprintChart <- R6Class("fingerprintChart",
                     public = list(
                       
                       #' @description
                       #' Calls the ui for the widget instance.
                       #' @param id Namespaced id. When calling UI for inside a diferent module,
                       #'   sometimes the namespace will not be passed correctly. PAssing the namespaced ID where will solve this.
                       #' @examples
                       #' test_chart <- use("body_chart.R")$fingerprintChart("test")
                       #' test_chart$ui(ns("test"))
                       ui = NULL,
                       
                       #' @description
                       #' Calls the server module for the widget instance.
                       server = NULL,
                       
                       state = reactiveValues(
                         id = NULL,
                         options = list(
                           title = "Explosive Score",
                           color = "black",
                           active_color = "lightslategray"
                         ),
                         values = list(
                           total = 0,
                           explosion = 0,
                           reach = 0,
                           balance = 0
                         ),
                         active = c()
                       ),
                       
                       #' @description
                       #' Create a new fingerprintChart object.
                       #' @param id Unique ID for the widget instance. Also used for namespacing the server module.
                       #' @param options A named list of options that will overwrite the default state$options.
                       #'   Partial named lists will result in only some options being overwritten while
                       #'   non named ones stay with the default values.
                       #' @param values Optional initial values to use for the bars and widget side label.
                       #' @return A new `fingerprintChart` object.
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
fingerprintChart <- fingerprintChart$new
