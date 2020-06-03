library(magrittr)
library(dplyr)
library(data.table)

players <- fread("data/players.csv", data.table = FALSE)
positions <- fread("data/positions.csv", data.table = FALSE)

data <- players %>% 
  left_join(positions, by = c("position" = "abbreviation"))

generate_stats <- function(data) {
  stats <- split(runif(4 * nrow(data), max = 100), rep(1:4, each = nrow(data))) %>% 
    as.data.frame() %>% 
    setNames(c("explosive", "reach", "balance", "capacity"))
  cbind(data, stats)
}

data <- generate_stats(data)
fwrite(data, "data/data.csv")
