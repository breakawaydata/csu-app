
#Takes in the complete player file, trimed player file and all cleaned data sources
get_data <- function(players, players_trim, positions, data_source_1, data_source_2, data_source_3) {
  
  require(tidyverse)
  
  #ASSESSMENT DATE
  players_trim$assessment_date = "Spring 2021"
  
  #Get data include player information to link and the data sources needed
  reach_table <- get_reach(players_trim, data_source_1)
  balance_table <- get_balance(players_trim, data_source_1, data_source_3)
  explosion_table <- get_explosion(players_trim, data_source_1, data_source_2, data_source_3)
  anthro_table <- get_anthro(players_trim, data_source_1)

  #Make all NA values 0 - ignore the anthro table
  reach_table[is.na(reach_table)] <-0
  balance_table[is.na(balance_table)] <-0
  explosion_table[is.na(explosion_table)] <-0
  
  #Merge tables into master table and calculate final score
  master_table <- base::merge(reach_table, balance_table, by = c('player_id', 'first', 'last', 'suffix', 'assessment_date'), all =  TRUE) 
  master_table <- base::merge(master_table, anthro_table, by = c('player_id', 'first', 'last', 'suffix',  'assessment_date'), all =  TRUE)
  master_table <- base::merge(master_table, explosion_table, by = c('player_id', 'first', 'last', 'suffix',  'assessment_date'), all = TRUE) %>%
    rowwise () %>%
    mutate(total_score = round(mean(c(explosion_score, reach_score, balance_score), na.rm = TRUE)))
  
  master_table$total_score = round(scales::rescale(master_table$total_score, to = c(40, 100)))
  
  #Get summary table of just the players scores
  summary_table <- master_table %>%
    select(player_id, first, last, suffix, assessment_date, total_score,
           explosion_score, strength_score, power_score,
           reach_score, speed_score, agility_score,
           balance_score, mobility_score, stability_score,
           height, weight, wingspan)
  

  #Get data_players for main menu page
  data_players <- master_table %>%
    select(player_id, 
           total_score,
           explosion_score,
           reach_score,
           balance_score)
  
  data_players <- base::merge(data_players, players, by = "player_id")
  data_players <- base::merge(data_players, positions, by.x = "position", by.y = "abbreviation")
  data_players <- data_players %>%
    select(player_id, first, last, suffix, position, position_detailed,
           number, class, picture, positions,
           total_score,
           explosion_score,
           reach_score,
           balance_score)
  
  #Get data_positions for main menu page
  data_positions <- data_players %>%
    group_by(position, positions) %>%
    summarise(summary = round(mean(total_score, na.rm = TRUE)),
              explosion =round(mean(explosion_score, na.rm = TRUE)),
              reach = round(mean(reach_score, na.rm = TRUE)),
              balance = round(mean(balance_score, na.rm = TRUE)))
  
  #Write out all data to necessary csv files
  write.csv(reach_table, "data/production/reach_data.csv", row.names = FALSE)
  write.csv(balance_table, "data/production/balance_data.csv", row.names = FALSE)
  write.csv(explosion_table, "data/production/explosion_data.csv", row.names = FALSE)
  write.csv(anthro_table, "data/production/anthro_data.csv", row.names = FALSE)
  write.csv(master_table, "data/production/master_data.csv" , row.names = FALSE)
  write.csv(summary_table, "data/production/summary_table.csv", row.names = FALSE)
  write.csv(data_players, "data/production/data_players.csv", row.names = FALSE)
  write.csv(data_positions, "data/production/data_positions.csv", row.names = FALSE)

  return()
}
