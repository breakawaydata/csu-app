library(shiny)
library(shiny.semantic)
library(shiny.grid)
library(shinyjs)
library(modules)
library(config)
library(sass)
library(dplyr)
library(dplyr)

consts <- config::get(file = "constants/constants.yml")
data <- data.table::fread("data/data_players.csv", data.table = FALSE)
position_stats <- data.table::fread("data/data_positions.csv", data.table = FALSE)

explosion_data <- data.table::fread("data/explosion_data.csv", data.table = FALSE)

pages <- list(
  explosion = use("modules/pages/explosive_page.R")$explosivePage("explosivePage", explosion_data)
)

text_card <- use("modules/components/text_card.R")$text_card

sass(
  sass::sass_file(consts$sass$input),
  cache_options = sass_cache_options(FALSE),
  options = sass_options(output_style = consts$sass$style),
  output = consts$sass$output
)

main <- use("modules/main.R")
menu <- use("modules/menu.R")
sidebar <- use("modules/sidebar.R")