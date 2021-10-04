library(rsconnect)
# make sure your working directory is inside src folder
#readRenviron("../deployment/credentials")
rsconnect::setAccountInfo(
  name = "breakaway",
  token = "B52B06FDCA4E6833CF8118C71A9285AF",
  secret = "gdBZ0iOgw5+vvQPpKWevp3vmrvMJVb8Rs06KDAEw"
)
rsconnect::deployApp(getwd(), appName = "co-state")
