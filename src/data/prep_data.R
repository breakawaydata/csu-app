library(magrittr)
library(dplyr)
library(data.table)
library(assertr)
library(data.validator)

players <- fread("data/players.csv", data.table = FALSE)
positions <- fread("data/positions.csv", data.table = FALSE)



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

data <- generate_stats(data) %>% 
  dplyr::arrange(desc(summary))
position_stats <- data %>% 
  dplyr::group_by(position, positions) %>% 
  summarise_at(vars(explosive, reach, balance, capacity, summary), mean) %>% 
  dplyr::mutate_if(is.numeric, round, digits = 0) %>% 
  dplyr::arrange(desc(summary))

fwrite(data, "data/data_players.csv")
fwrite(position_stats, "data/data_positions.csv")
