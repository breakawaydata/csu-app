import("htmltools")
import("shiny")
import("shiny.semantic")
import("shiny.grid")
import("magrittr")

export("ui", "init_server")

expose("utils/utils.R")
# referenced to object that lives in the environment that calls module, global.R in this case.
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
        "assets/clemson.png"
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
    browser_search("players", search_api_url, "")
  })
  
  observeEvent(input$level, {
    session$userData$level(input$level)
    print(isolate(session$userData$level()))
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
    div(class = "logo", tags$img(class = "ui centered tiny image", src = "assets/ball.png")),
    div(class = "search-container", uiOutput(ns("search_field"))),
    div(class = "levels", style = "text-align: center;", 
      htmlTemplate("modules/templates/breadcrumb.html")    
    )
  )
}

search_api <- function(data, q) {
  has_matching <- function(field) {
    startsWith(field, q)
  }
  data %>%
    dplyr::filter(has_matching(first) | has_matching(last) | has_matching(positions) | has_matching(as.character(number)))
}

browser_search <- function(name, search_api_url, default_text = "Search") {
  shiny::tagList(
    div(class = "ui search", id = name,
        div(class = "ui icon fluid input",
            tags$input(class = "prompt search field",
                       type = "text",
                       placeholder = default_text,
                       oninput = "null"),
            uiicon("search")
        ),
        div(class = "results")
    ),
    tags$script(browser_search_js(name, search_api_url))
  )
}

browser_search_js <- function(name, search_api_url) {
  HTML(glue::glue("
    $('#{name}').search({{
      apiSettings: {{
        url: '{search_api_url}&q={{query}}'
      }},
      maxResults: 40,
      cache: false,
      onResults: function(response) {{
        console.log(response);
        let ids = response.results.map(entry => entry.player_id);
        let elements = document.querySelectorAll(ids.map(id => `#${{id}}`).join(', '));
        $('.player-card').hide();
        $(elements).show();
      }},
      onResultsClose: function(results) {{
        $('.player-card').show();
      }}
    }})             
  "))
}

user_tools <- function() {
  div(class = "user")
}
