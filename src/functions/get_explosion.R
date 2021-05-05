
#Gets explosion data table
get_explosion <- function(players_trim, data1, data2, data3) {
  require(tidyverse)
  
  #Get relevant data
  data_trim_1 <- data1 %>%
    select(player, bench, vertical_jump, broad_jump)
  
  data_trim_2 <- data2 %>%
    select(player, sparta_load, sparta_explode, sparta_drive)
  
  data_trim_3 <- data3 %>%
    select(player, max_force, max_impulse)
   
  #Link data sets
  data_linked <- base::merge(data_trim_1,data_trim_2, all = TRUE)
  data_linked <- base::merge(data_linked, data_trim_3, all = TRUE)
  
  #Get scores for each movement
  data_scoring <- percentile_function(data_linked, "high")
  
  #Get pillar scores, outline which score goes to which pillar
  #Strength first than power
  data_pillar <- ba_scoring(data_scoring, 
                            c('player', 'bench_score', 'max_force_score', 'max_impulse_score'),
                            c('player', 'broad_jump_score', 'vertical_jump_score','sparta_load_score', 
                              'sparta_drive_score', 'sparta_explode_score'),
                            "explosion")
  
  #Merge pillar scores, with full data scoring and slim profile information
  data_final <- base::merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  data_final <- data_final %>%
    rename(bench_raw = bench,
           vertical_jump_raw = vertical_jump,
           broad_jump_raw = broad_jump,
           sparta_load_raw = sparta_load,
           sparta_explode_raw = sparta_explode,
           sparta_drive_raw = sparta_drive,
           max_force_raw = max_force,
           max_impulse_raw = max_impulse)
  
  data_final <- left_join(players_trim, data_final, by = "player")
  
  return(data_final)
}
