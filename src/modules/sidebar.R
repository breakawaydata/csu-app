import("htmltools")
import("shiny")
import("shiny.semantic")

export("ui", "init_server")

consts <- modules::use(consts)

ui <- function(id) {
  ns <- NS(id)
  htmltools::htmlTemplate(
    "modules/templates/sidebar.html",
    sidebar_elements =
      tagList(
        sidebar_element(page_id = "summary", item_id = "summary", icon = "summary", text = "SUMMARY", class = "sidebar-active"),
        sidebar_element(page_id = "explosive", item_id = "explosive", icon = "explosive", text = "EXPLOSIVE"),
        sidebar_element(page_id = "reach", item_id = "reach", icon = "reach", text = "REACH"),
        sidebar_element(page_id = "balance", item_id = "balance", icon = "balance", text = "BALANCE"),
        sidebar_element(page_id = "capacity", item_id = "capacity", icon = "capacity", text = "CAPACITY"),
        sidebar_element(page_id = "game", item_id = "summary", icon = "analytics_bars", text = "GAME")
      ),
    id = consts$dom$sidebar_navigation_id
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns

  session$userData$stat <- reactiveVal("summary")
  session$userData$page <- reactiveVal("summary")

  observeEvent(input$stat, {
    session$userData$stat(input$stat)
  }, ignoreInit = TRUE)

  observeEvent(input$page, {
    session$userData$page(input$page)
  }, ignoreInit = TRUE)
}

sidebar_element <- function(page_id, item_id, icon, text, class = "") {
  tags$a(
    class = glue::glue("item {class}"),
    id = glue::glue("{item_id}"),
    `data-page-id` = page_id,
    tags$div(
      class = "sidebar-item",
      tags$div(
        class = "sidebar-image",
        tags$img(
          class = "icon",
          src = glue::glue("icons/{icon}.png"),
          text
        )
      )
    )
  )
}
