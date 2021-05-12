
#Gets reach data table
get_reach <- function(players_trim, data) {
  require(tidyverse)
  
  #Get trimmed data
  data_trim <- data %>%
    select(player, 
           delta_10,
           delta_20,
           delta_40,
           peak_40,
           x3_cone,
           shuttle)

  #Get scores for each movement
  data_scoring <- percentile_function(data_trim, "low")
  
  #Get pillar scores, outline which score goes to which pillar
  #Speed first than agility
  data_pillar <- ba_scoring(data_scoring, 
                            c('player', 'delta_10_score', 'delta_20_score', 'delta_40_score', 'peak_40_score'),
                            c('player', 'x3_cone_score', 'shuttle_score'),
                            "reach")
  
  #Merge pillar scores, with full data scoring and slim profile information
  data_final <- base::merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  
  data_final <- data_final %>%
    rename(delta_10_raw = delta_10,
           delta_20_raw = delta_20,
           delta_40_raw = delta_40,
           peak_40_raw = peak_40,
           x3_cone_raw = x3_cone,
           shuttle_raw = shuttle)
  
  data_final <- left_join(players_trim, data_final, by = "player")
  
  return(data_final %>%
           select(-player))
}
