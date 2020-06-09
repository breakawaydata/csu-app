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
    columns = "33% 34% 33%",
    areas = "card filters user",
    gap = list(
      default = "20px",
      xs = "5px"
    ),
    div(
      id = consts$dom$logo_card_id,
      class = "card",
      horizontal_card(
        consts$global$team_name,
        "All Players",
        glue::glue("url('{consts$global$team_logo}')")
      )
    ),
    filters(ns),
    user_tools()
  )
}

init_server <- function(id, data) {
  callModule(server, id, data)
}

get_username <- function(session) {
  shinyapp_user <- isolate(session$user)
  if (is.null(shinyapp_user)) {
    return(Sys.getenv("username"))
  }
  return(shinyapp_user)
}

server <- function(input, output, session, data) {
  ns <- session$ns
  
  session$sendCustomMessage("update_user_data", list(username = get_username(session)))
  
  session$userData$level <- reactiveVal("all")
  search_api_url <- register_search(session, data, search_api)

  output$search_field <- renderUI({
    browser_search(consts$search$id, search_api_url)
  })
  
  
  observeEvent(input$level, {
    session$userData$level(input$level)
  }, ignoreInit = TRUE)

}

horizontal_card <- function(header, sub_header, img_path, description = NULL) {
  htmltools::htmlTemplate(
    "modules/templates/horizontal-card.html", 
    header = header, sub_header = sub_header, img_path = img_path, description = description
  )
}

menu_navigation <- function(id) {
  htmltools::htmlTemplate(
    "modules/templates/breadcrumb.html",
    id = id, 
    all_level_id = consts$dom$all_level_id,
    position_level_id = consts$dom$position_level_id,
    player_level_id = consts$dom$player_level_id
  )
}

filters <- function(ns) {
  gridPanel(
    class = "filters",
    rows = "50% 25% 25%",
    areas = c("logo", "search-container", "levels"),
    div(class = "logo", tags$img(class = "ui centered tiny image", src = consts$global$team_logo_small)),
    div(class = "search-container", uiOutput(ns("search_field"))),
    div(class = "levels", style = "text-align: center;", 
      menu_navigation(consts$dom$menu_navigation_id)
    )
  )
}

search_api <- function(data, q) {
  has_matching <- function(field) {
    startsWith(toupper(field), toupper(q))
  }
  players = data %>%
    dplyr::filter(has_matching(first) | has_matching(last) | has_matching(positions) | has_matching(as.character(number))) %>% 
    dplyr::mutate(search = consts$search$player_search_type)
  positions = data %>% 
    dplyr::filter(has_matching(positions)) %>% 
    dplyr::mutate(search = consts$search$position_search_type)
  rbind(players, positions)
}

browser_search <- function(id, search_api_url) {
  htmltools::htmlTemplate(
    "modules/templates/search.html",
    id = id, 
    search_api_url = search_api_url, 
    player_card_class = consts$dom$player_card_class, 
    position_card_class = consts$dom$position_card_class,
    player_search_type = consts$search$player_search_type,
    position_search_type = consts$search$position_search_type
  )
}

user_tools <- function() {
  div(class = "user", htmltools::htmlTemplate("modules/templates/user.html"))
}
