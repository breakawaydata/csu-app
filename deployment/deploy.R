library(rsconnect)
# make sure your working directory is inside src folder
readRenviron("../deployment/credentials")
rsconnect::setAccountInfo(
  name = Sys.getenv("name"),
  token = Sys.getenv("token"),
  secret = Sys.getenv("secret")
)
rsconnect::deployApp(getwd(), appName = "breakaway")
