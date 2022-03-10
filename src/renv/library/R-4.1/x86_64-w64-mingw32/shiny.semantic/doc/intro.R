## ----setup, include = FALSE, screenshot.force = FALSE-------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(shiny)
library(shiny.semantic)

## ----echo=T, include = T, screenshot.force = FALSE----------------------------
uirender(
  shiny::tagList(
    h4("Numeric input", icon("world")),
    shiny.semantic::numeric_input("input", value = 0, label = ""),
    shiny.semantic::list_container(list(
        list(header = "Item 1", description = "My text for item 1", icon = "cat"),
        list(header = "Item 2", description = "My text for item 2", icon = "tree"),
        list(header = "Item 3", description = "My text for item 3", icon = "dog")
      ), is_divided = TRUE
    )
  ),
width = "200px", height = "280px")

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page")
#  )
#  
#  server <- function(input, output, session) {}
#  
#  shinyApp(ui, server)

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page"),
#    sidebar_layout(
#      sidebar_panel(),
#      main_panel()
#      )
#  )
#  
#  server <- function(input, output, session) {}
#  
#  shinyApp(ui, server)

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page"),
#    sidebar_layout(
#      sidebar_panel(),
#      main_panel(
#        segment(
#           cards(
#             class = "two",
#             card(class = "red",
#               div(class = "content",
#                   div(class = "header", "Main title card 1"),
#                   div(class = "meta", "Sub title card 1"),
#                   div(class = "description", "More detail description card 1")
#               )
#             ),
#             card(class = "blue",
#               div(class = "content",
#                   div(class = "header", "Main title card 2"),
#                   div(class = "meta", "Sub title card 2"),
#                   div(class = "description", "More detail description card 2")
#                   )
#               )
#             )
#           )
#        )
#      )
#    )
#  
#  server <- function(input, output, session) {}
#  
#  shinyApp(ui, server)

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page"),
#    sidebar_layout(
#      sidebar_panel(
#        dropdown_input("mtcars_dropdown", c("mpg", "cyl", "disp", "hp"), value = "mpg"),
#        textOutput("dropdown")
#      ),
#      main_panel(
#        segment(
#           cards(
#             class = "two",
#             card(class = "red",
#               div(class = "content",
#                   div(class = "header", "Main title card 1"),
#                   div(class = "meta", "Sub title card 1"),
#                   div(class = "description", "More detail description card 1")
#               )
#             ),
#             card(class = "blue",
#               div(class = "content",
#                   div(class = "header", "Main title card 2"),
#                   div(class = "meta", "Sub title card 2"),
#                   div(class = "description", "More detail description card 2")
#               )
#             )
#           )
#        )
#        )
#      )
#  )
#  
#  server <- function(input, output, session) {
#    output$dropdown <- renderText(input$mtcars_dropdown)
#  }
#  
#  shinyApp(ui, server)

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page"),
#    sidebar_layout(
#      sidebar_panel(
#        p("Select variable for plots:"),
#        dropdown_input("mtcars_dropdown", c("mpg", "cyl", "disp", "hp"), value = "mpg")
#      ),
#      main_panel(
#        segment(
#           cards(
#             class = "two",
#             card(class = "red",
#               div(class = "content",
#                   div(class = "header", "Main title card 1"),
#                   div(class = "meta", "Sub title card 1"),
#                   div(class = "description", "More detail description card 1")
#               )
#             ),
#             card(class = "blue",
#               div(class = "content",
#                   div(class = "header", "Main title card 2"),
#                   div(class = "meta", "Sub title card 2"),
#                   div(class = "description", "More detail description card 2")
#               )
#             )
#           )
#        ),
#        plotOutput("histogram"),
#        plotOutput("plot")
#        )
#      )
#  )
#  
#  server <- function(input, output, session) {
#    output$dropdown <- renderText(input$mtcars_dropdown)
#    output$histogram <- renderPlot(hist(mtcars[[input$mtcars_dropdown]]))
#    output$plot <- renderPlot(plot(mtcars[[input$mtcars_dropdown]]))
#  }
#  
#  shinyApp(ui, server)

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  grid_charts <- grid_template(
#    default = list(areas = rbind(c("chart1", "chart2")),
#                   rows_height = c("100%"),
#                   cols_width = c("50%", "50%"))
#  )

## ----echo=TRUE, include=TRUE, eval=FALSE, screenshot.force = FALSE------------
#  ui <- semanticPage(
#    title = "My first page",
#    h1("My page"),
#    sidebar_layout(
#      sidebar_panel(
#        p("Select variable for plots:"),
#        dropdown_input("mtcars_dropdown", c("mpg", "cyl", "disp", "hp"), value = "mpg")
#      ),
#      main_panel(
#        segment(
#           cards(
#             class = "two",
#             card(class = "red",
#               div(class = "content",
#                   div(class = "header", "Main title card 1"),
#                   div(class = "meta", "Sub title card 1"),
#                   div(class = "description", "More detail description card 1")
#               )
#             ),
#             card(class = "blue",
#               div(class = "content",
#                   div(class = "header", "Main title card 2"),
#                   div(class = "meta", "Sub title card 2"),
#                   div(class = "description", "More detail description card 2")
#               )
#             )
#           )
#        ),
#        grid(grid_charts,
#             chart1 = plotOutput("histogram"),
#             chart2 = plotOutput("plot")
#           )
#        )
#      )
#  )
#  
#  server <- function(input, output, session) {
#    output$dropdown <- renderText(input$mtcars_dropdown)
#    output$histogram <- renderPlot(hist(mtcars[[input$mtcars_dropdown]]))
#    output$plot <- renderPlot(plot(mtcars[[input$mtcars_dropdown]]))
#  }
#  
#  shinyApp(ui, server)

