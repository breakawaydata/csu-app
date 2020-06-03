library(shiny)
library(shiny.semantic)
library(shiny.grid)
library(shinyjs)
library(modules)
library(config)
library(sass)

consts <- config::get(file = "constants/constants.yml")
data <- data.table::fread("data/data.csv", data.table = FALSE)

sass(
  sass::sass_file(consts$sass$input),
  cache_options = sass_cache_options(FALSE),
  options = sass_options(output_style = consts$sass$style),
  output = consts$sass$output
)

main <- use("modules/main.R")
menu <- use("modules/menu.R")
sidebar <- use("modules/sidebar.R")
