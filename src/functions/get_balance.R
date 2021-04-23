
#Gets balance data table
get_balance <- function(players_trim, data1, data2) {
  require(tidyverse)
  
  #Get trimmed data sets for balance
  data_trim_1 <- data1 %>%
    select(player, ods, hs_left, hs_right, hs_final,
           ill_left, ill_right, ill_final, ill_ac, ill_extra,
           sm_left, sm_right, sm_final,
           aslr_left, aslr_right, aslr_final,
           rs_left, rs_right, rs_final,
           ods, tspu, fms, fms_asym)
  
  data_trim_2 <- data2 %>%
    select(player, max_imbalance, impulse_imbalance)

  #Link data sets
  data_linked <- merge(data_trim_1, data_trim_2, by = 'player', all = TRUE)

  #Break up data into high and low calculations
  data_integer_high <- data_linked %>%
    select(-max_imbalance, -impulse_imbalance, -fms_asym)
  
  data_integer_low <- data_linked %>%
    select(player,max_imbalance, impulse_imbalance, fms_asym)
  
  #Get scores for each movement
  data_scoring1 <- percentile_function(data_integer_high,"high")
  data_scoring2 <- percentile_function(data_integer_low,"low")

  data_scoring <- merge(data_scoring1, data_scoring2, all = TRUE)
  
  #Get pillar scores, outline which score goes to which pillar
  #Mobility first than stability 
  data_pillar <- ba_scoring(data_scoring, 
                            c('player', 'fms_score', 'ods_score', 'tspu_score', 
                            'hs_left_score', 'hs_right_score',
                            'ill_left_score', 'ill_right_score',
                            'sm_left_score', 'sm_right_score',
                            'aslr_left_score', 'aslr_right_score',
                            'rs_left_score', 'rs_right_score'),
                            c('player', 'fms_asym_score', 'max_imbalance_score', 'impulse_imbalance_score'),
                            "balance")
  
  #Merge pillar scores, with full data scoring and slim profile information
  data_final <- merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  data_final <- left_join(players_trim, data_final, by = "player")
  
  return(data_final)
}
