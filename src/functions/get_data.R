#Takes in the complete player file, trimed player file and all cleaned data sources
get_data <- function(players, players_trim, data_source_1, data_source_2, data_source_3) {
  require(tidyverse)
  
  #Get data include player information to link and the data sources needed
  reach_table <- get_reach(players_trim, data_source_1)
  balance_table <- get_balance(players_trim, data_source_1, data_source_3)
  explosion_table <- get_explosion(players_trim, data_source_1, data_source_2, data_source_3)
  anthro_table <- get_anthro(players, data_source_1)

  #Merge tables into master table and calculate final score
  master_table <- merge(reach_table, balance_table, by = c('player', 'player_id', 'first', 'last', 'suffix'), all =  TRUE) 
  master_table <- merge(master_table, explosion_table, by = c('player', 'player_id', 'first', 'last', 'suffix'), all = TRUE) %>%
    rowwise () %>%
    mutate(total_score = round(mean(c(explosion_score, reach_score, balance_score), na.rm = TRUE)))
  
  #Get summary table of just the players scores
  summary_table <- master_table %>%
    select(player, total_score,
           explosion_score, strength_score, power_score,
           reach_score, speed_score, agility_score,
           balance_score, mobility_score, stability_score)
  
  #Write out all data to necessary csv files
  write.csv(reach_table, "data/reach_data_1.csv")
  write.csv(balance_table, "data/balance_data_1.csv")
  write.csv(explosion_table, "data/explosion_data_1.csv")
  write.csv(anthro_table, "data/anthro_data_1.csv")
  write.csv(master_table, "data/master_data.csv" )
  write.csv(summary_table, "data/summary_table.csv")

  return()
}
