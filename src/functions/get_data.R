######################### MAIN #########################

get_data <- function(filename) {
  require(tidyverse)

  data <- readxl::read_excel(path = filename)

  reach_table <- get_reach(data)
  balance_table <- get_balance(data)
  explosion_table <- get_explosion(data)
  anthro_table <- get_anthro(data)

  fwrite(reach_table, "data/reach_data_1.csv")
  fwrite(balance_table, "data/balance_data_1.csv")
  fwrite(explosion_table, "data/explosion_data_1.csv")
  fwrite(anthro_table, "data/anthro_data_1.csv")

  return()
}
