library(magrittr)
library(dplyr)
library(data.table)
library(assertr)
library(data.validator)

players <- fread("data/players.csv", data.table = FALSE)
positions <- fread("data/positions.csv", data.table = FALSE)

validator <- create_validator()
players %>%
  chain_start(store_success = TRUE) %>%
  verify(
    description = "All required columns exist", 
    has_all_names("player_id", "first", "last", "position", "number", "picture"), 
    obligatory = TRUE
  ) %>% 
  verify(
    description = "explosive, reach, balance, capacity should exist in data",
    has_all_names("explosive", "reach", "balance", "capacity"), 
    skip_chain_opts = TRUE, error_fun = warning_append
  ) %>% 
  verify(description = "number is integer", has_class("number", class = "integer")) %>% 
  assert(description = "No NA's inside all columns", not_na, player_id, first, last, position, number, picture, skip_chain_opts = TRUE,
         error_fun = warning_append, success_fun = success_append) %>%
  assert(description = "Player ID is unique", is_uniq, player_id) %>% 
  chain_end(error_fun = error_append) %>%
  add_results(validator)

positions %>%
  chain_start(store_success = TRUE) %>%
  verify(
    description = "All required columns exist", 
    has_all_names("positions", "abbreviation"), 
    obligatory = TRUE
  ) %>% 
  assert(description = "No NA's inside all existing columns", not_na, positions, abbreviation, skip_chain_opts = TRUE,
         error_fun = warning_append, success_fun = success_append) %>%
  assert(description = "Abbreviation is unique", is_uniq, abbreviation) %>% 
  chain_end(error_fun = error_append) %>%
  add_results(validator)

print(validator)
results <- get_results(validator)

if (any(results$type == "error")) {
  save_report(validator, output_dir = "data")
  rstudioapi::viewer("data/validation_report.html")
  message("Data was not update due to validation violations")
} else {
  if (any(results$type == "warning")) {
    save_report(validator, output_dir = "data")
    rstudioapi::viewer("data/validation_report.html")
  }
  data <- players %>% 
    left_join(positions, by = c("position" = "abbreviation"))
  
  generate_stats <- function(data, stats_names = c("explosive", "reach", "balance", "capacity")) {
    stats <- split(round(runif(length(stats_names) * nrow(data), max = 100), 0),
                   rep(1:length(stats_names), each = nrow(data))) %>% 
      as.data.frame() %>% 
      setNames(stats_names) %>% 
      dplyr::mutate(summary = round(rowSums(.)/length(stats_names), 0))
    cbind(data, stats)
  }
  
  if (!all(c("explosive", "reach", "balance", "capacity") %in% colnames(data))) {
    data <- generate_stats(data)
  }
  
  data <- data %>% dplyr::arrange(desc(summary))
  position_stats <- data %>% 
    dplyr::group_by(position, positions) %>% 
    summarise_at(vars(explosive, reach, balance, capacity, summary), mean) %>% 
    dplyr::mutate_if(is.numeric, round, digits = 0) %>% 
    dplyr::arrange(desc(summary))
  
  fwrite(data, "data/data_players.csv")
  fwrite(position_stats, "data/data_positions.csv")  
  message("Data successfuly updated")
}
