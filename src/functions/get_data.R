######################### MAIN #########################

get_data <- function(filename) {
  require(tidyverse)
  require(readxl)
  
  data <- read_excel(path = filename)
  
  reach_table <- get_reach(data)
  mobility_table <- get_mobility(data)
  explosion_table <- get_explosion(data)
  bodycomp_table <- get_bodycomp(data)
  
  write.csv(reach_table, "reach_data.csv")
  write.csv(mobility_table, "mobility_data.csv")
  write.csv(explosion_table, "explosion_data.csv")
  write.csv(bodycomp_table, "bodycomp_data.csv")
  
  data_a <- merge(reach_table, mobility_table)
  data_b <- merge(explosion_table, bodycomp_table)
  data_final <- merge(data_a, data_b)
  
  data_final[is.na(data_final)] <- "N/A"
  
  return(data_final)
}