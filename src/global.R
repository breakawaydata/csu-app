library(shiny)
library(shiny.semantic)
library(shiny.grid)
library(shinyjs)
library(modules)
library(config)
library(sass)
library(dplyr)
library(Cairo)
options(shiny.usecairo=TRUE)

consts <- config::get(file = "constants/constants.yml")
data <- data.table::fread("data/production/data_players.csv", data.table = FALSE)
position_stats <- data.table::fread("data/production/data_positions.csv", data.table = FALSE)


summary_data <- data.table::fread("data/production/summary_table.csv", data.table = FALSE)
explosion_data <- data.table::fread("data/explosion_data.csv", data.table = FALSE)
reach_data <- data.table::fread("data/reach_data.csv", data.table = FALSE)
balance_data <- data.table::fread("data/production/balance_data.csv", data.table = FALSE)
mock_data <- data.table::fread("data/college_schedule.csv", data.table = FALSE) %>%
  filter(home_team_name == "Colorado State" | away_team_name == "Colorado State") %>%
  select(week, home_team_name, home_score, away_team_name, away_score, venue_name, timestamp_utc)

file_downloader <- use("modules/components/file_generator.R")$fileDownloader("fileDownloader")

pages <- list(
  summary = use("modules/pages/summary_page.R")$summaryPage("summaryPage", summary_data),
  explosion = use("modules/pages/explosive_page.R")$explosivePage("explosivePage", explosion_data),
  reach = use("modules/pages/reach_page.R")$reachPage("reachPage", reach_data),
  balance = use("modules/pages/balance_page.R")$balancePage("balancePage", balance_data),
<<<<<<< Updated upstream
  schedule = use("modules/pages/schedule_page.R")$schedulePage("schedulePage", mock_data)
=======
  schedule = use("modules/pages/schedule_page.R")$schedulePage("schedulePage", mock_data),
  scheduleall = use("modules/pages/scheduleall_page.R")$scheduleallPage("scheduleallPage", mock_data)
>>>>>>> Stashed changes
)

sass(
  sass::sass_file(consts$sass$input),
  cache = sass_cache_options(FALSE),
  options = sass_options(output_style = consts$sass$style),
  output = consts$sass$output
)

main <- use("modules/main.R")
menu <- use("modules/menu.R")
sidebar <- use("modules/sidebar.R")
