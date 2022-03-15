library(shiny)
library(shiny.semantic)
library(imola)
library(shinyjs)
library(modules)
library(config)
library(sass)
library(dplyr)
library(gt)
library(Cairo) ## Helps clarity of graphics
options(shiny.usecairo=TRUE) ## Helps clarity of graphics

## Load in constants
consts <- config::get(file = "constants/constants.yml")
## Load in player data for player cards in main
data <- data.table::fread("data/production/data_players.csv", data.table = FALSE)
## Load in position data for position cards in main
position_stats <- data.table::fread("data/production/data_positions.csv", data.table = FALSE)

## Load in data to power each page
summary_data <- data.table::fread("data/production/summary_table.csv", data.table = FALSE)
explosion_data <- data.table::fread("data/explosion_data.csv", data.table = FALSE)
reach_data <- data.table::fread("data/reach_data.csv", data.table = FALSE)
balance_data <- data.table::fread("data/production/balance_data.csv", data.table = FALSE)
mock_data <<- data.table::fread("data/college_schedule.csv", data.table = FALSE) %>%
  filter(home_team_name == "Colorado State" | away_team_name == "Colorado State") %>%
  select(week, home_team_name, home_score, away_team_name, away_score, venue_name, timestamp_utc)

## Load in file downloader component to be used within main
file_downloader <- use("modules/components/file_generator.R")$fileDownloader("fileDownloader")

## Load in pages to be used within main
pages <- list(
  summary = use("modules/pages/summary_page.R")$summaryPage("summaryPage", summary_data),
  explosion = use("modules/pages/explosive_page.R")$explosivePage("explosivePage", explosion_data),
  reach = use("modules/pages/reach_page.R")$reachPage("reachPage", reach_data),
  balance = use("modules/pages/balance_page.R")$balancePage("balancePage", balance_data),
  schedule = use("modules/pages/schedule_page.R")$schedulePage("schedulePage", mock_data),
  schedule2 = use("modules/pages/schedule2_page.R")$schedule2Page("schedule2Page", mock_data)
)

## Load in styling
sass(
  sass::sass_file(consts$sass$input),
  cache = sass_cache_options(FALSE),
  options = sass_options(output_style = consts$sass$style),
  output = consts$sass$output
)

## Run our three main modules
main <- use("modules/main.R")
menu <- use("modules/menu.R")
sidebar <- use("modules/sidebar.R")
