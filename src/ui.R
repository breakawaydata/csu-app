function(input, output, session){
  semanticPage(
    tags$head(
      suppressDependencies("bootstrap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
      tags$script(src = "js/menu.js"),
      tags$script(src = "js/sidebar.js"),
      tags$script(src = "js/player.js")
    ),
    gridPage(
      title = "Gains Group",
      rows = "200px 100%",
      areas = c(
        "header",
        "body"
      ),
      gridPanel(
        class = "header",
        menu$ui("menu")
      ),
      gridPanel(
        class = "body",
        columns = "15% 85%",
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
