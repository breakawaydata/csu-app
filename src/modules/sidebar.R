import("htmltools")
import("shiny")
import("shiny.semantic")

export("ui", "init_server")

expose("utils/utils.R")
consts <- modules::use(consts)

ui <- function(id) {
  ns <- NS(id)
  htmlTemplate(
    "modules/templates/sidebar.html",
    sidebar_elements = 
      tagList(
        sidebar_element(item_id = "summary", icon = "summary", text = "SUMMARY"),
        sidebar_element(item_id = "explosive", icon = "explosive", text = "EXPLOSIVE"),
        sidebar_element(item_id = "reach", icon = "reach", text = "REACH"),
        sidebar_element(item_id = "balance", icon = "balance", text = "BALANCE"),
        sidebar_element(item_id = "capacity", icon = "capacity", text = "CAPACITY"),
      )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns
  
  session$userData$stat <- reactiveVal("summary")

  observeEvent(input$stat, {
    session$userData$stat(input$stat)
    print(isolate(session$userData$stat()))
  }, ignoreInit = TRUE)
}

sidebar_element <- function(item_id, icon, text) {
  tags$a(
    class = "item",
    id = glue::glue("{item_id}"),
    tags$div(
      class = "sidebar-item",
      tags$img(
        class = "icon",
        src = glue::glue("icons/{icon}.png"),
        text
      )
    )
  )
}
