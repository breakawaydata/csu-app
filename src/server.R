server <- function(input, output, session) {

  menu$init_server("menu", data)
  sidebar$init_server("sidebar")
  main$init_server("module_id")
  
}
