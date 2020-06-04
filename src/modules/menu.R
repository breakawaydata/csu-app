import("htmltools")
import("shiny")
import("shiny.semantic")
import("shiny.grid")
import("magrittr")

export("ui", "init_server")

expose("utils/utils.R")
consts <- modules::use(consts)

ui <- function(id) {
  ns <- NS(id)
  gridPanel(
    class = "header",
    columns = "25% 50% 25%",
    areas = "card filters user",
    gap = list(
      default = "20px",
      xs = "5px"
    ),
    div(
      class = "card",
      horizontal_card(
        "Clemson Football",
        "All Players",
        "assets/card_logo.png"
      )
    ),
    filters(ns),
    user_tools()
  )
}

init_server <- function(id, data) {
  callModule(server, id, data)
}

server <- function(input, output, session, data) {
  ns <- session$ns
  session$userData$level <- reactiveVal("all")
  search_api_url <- register_search(session, data, search_api)
  
  output$search_field <- renderUI({
    browser_search("players", search_api_url, "player-card", "position-card")
  })
  
  observeEvent(input$level, {
    session$userData$level(input$level)
  }, ignoreInit = TRUE)

}

horizontal_card <- function(header, sub_header, img_path, description = NULL) {
  htmlTemplate(
    "modules/templates/horizontal-card.html", 
    header = header, sub_header = sub_header, img_path = img_path, description = description
  )
}

filters <- function(ns) {
  gridPanel(
    class = "filters",
    rows = "50% 25% 25%",
    areas = c("logo", "search-container", "levels"),
    div(class = "logo", tags$img(class = "ui centered tiny image", src = "assets/logo.png")),
    div(class = "search-container", uiOutput(ns("search_field"))),
    div(class = "levels", style = "text-align: center;", 
      htmlTemplate("modules/templates/breadcrumb.html")    
    )
  )
}

search_api <- function(data, q) {
  has_matching <- function(field) {
    startsWith(toupper(field), toupper(q))
  }
  players = data %>%
    dplyr::filter(has_matching(first) | has_matching(last) | has_matching(positions) | has_matching(as.character(number))) %>% 
    dplyr::mutate(search = "player")
  positions = data %>% 
    dplyr::filter(has_matching(positions)) %>% 
    dplyr::mutate(search = "position")
  rbind(players, positions)
}

browser_search <- function(id, search_api_url, player_card_class, position_card_class) {
  htmltools::htmlTemplate(
    "modules/templates/search.html",
    id = id, 
    search_api_url = search_api_url, 
    player_card_class = player_card_class, 
    position_card_class = position_card_class
  )
}

user_tools <- function() {
  div(class = "user")
}
