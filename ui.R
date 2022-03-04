function(input, output, session) {
  semanticPage(
    tags$head(
      suppressDependencies("bootstrap"),
      tags$script(glue::glue("var consts = {jsonlite::toJSON(consts, auto_unbox = TRUE)}")),
      tags$script(async = NA, src = glue::glue("https://www.googletagmanager.com/gtag/js?id={consts$ga$config}")),
      tags$script(src = "js/google-analytics.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
      tags$script(src = "js/md5.min.js"),
      tags$script(src = "js/menu.js"),
      tags$script(src = "js/sidebar.js"),
      tags$script(src = "js/player.js"),
      tags$script(src = "js/main.js"),
      tags$script(src = "js/toggle_main.js")
    ),
    gridPage(
      title = "Gains Group",
      rows = "200px 1fr",
      areas = c(
        "header",
        "body"
      ),
      menu$ui("menu", file_downloader),
      gridPanel(
        class = "body",
        columns = "minmax(200px, 15%) 1fr",
        areas = "sidebar main",
        gap = list(
          default = "20px",
          xs = "5px"
        ),
        div(
          class = "sidebar",
          sidebar$ui("sidebar")
        ),
        div(
          class = "main",
          main$ui("main", data, position_stats)
        )
      )
    )
  )
}
