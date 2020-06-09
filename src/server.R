server <- function(input, output, session) {

  session$senCustomMessage("trackEvent", list(category = "login", action = "state", label = "1"))
  menu$init_server("menu", data)
  sidebar$init_server("sidebar")
  main$init_server("main")
  
}
