server <- function(input, output, session) {

  session$sendCustomMessage("trackEvent", list(category = "login", action = "state", label = session$user))
  menu$init_server("menu", data)
  sidebar$init_server("sidebar")
  main$init_server("main", pages)

  pages$summary$server()
  pages$explosion$server()
  pages$reach$server()
  pages$balance$server()
  pages$schedule$server()
  file_downloader$server()
}
