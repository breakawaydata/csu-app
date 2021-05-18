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
# capacity_data <- data.table::fread("data/production/capacity_data.csv", data.table = FALSE)
balance_data <- data.table::fread("data/production/balance_data.csv", data.table = FALSE)

file_downloader <- use("modules/components/file_generator.R")$fileDownloader("fileDownloader")

pages <- list(
  summary = use("modules/pages/summary_page.R")$summaryPage("summaryPage", summary_data),
  explosion = use("modules/pages/explosive_page.R")$explosivePage("explosivePage", explosion_data),
  reach = use("modules/pages/reach_page.R")$reachPage("reachPage", reach_data),
  balance = use("modules/pages/balance_page.R")$balancePage("balancePage", balance_data)
  # ,
  # capacity = use("modules/pages/capacity_page.R")$capacityPage("capacityPage", capacity_data)

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
