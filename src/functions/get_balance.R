
#Gets balance data table
get_balance <- function(players_trim, data1, data2) {
  require(tidyverse)
  
  #Get trimmed data sets for balance
  data_trim_1 <- data1 %>%
    select(player, hs_left, hs_right, hs_diff, hs_final,
           ill_left, ill_right, ill_diff, ill_final, ill_ac, ill_extra,
           sm_left, sm_right, sm_diff, sm_final,
           aslr_left, aslr_right, aslr_diff, aslr_final,
           rs_left, rs_right, rs_diff, rs_final,
           ods, tspu, fms, fms_asym)
  
  data_trim_2 <- data2 %>%
    select(player, max_imbalance, impulse_imbalance)

  #Link data sets
  data_linked <- base::merge(data_trim_1, data_trim_2, by = 'player', all = TRUE)

  #Break up data into high and low calculations
  data_integer_high <- data_linked %>%
    select(-max_imbalance, -impulse_imbalance, -fms_asym)
  
  data_integer_low <- data_linked %>%
    select(player,max_imbalance, impulse_imbalance, fms_asym)
  
  #Get scores for each movement
  data_scoring1 <- percentile_function(data_integer_high,"high")
  data_scoring2 <- percentile_function(data_integer_low,"low")

  data_scoring <- base::merge(data_scoring1, data_scoring2, all = TRUE)
  
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
  data_final <- base::merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  data_final <- data_final %>%
    rename(ods_raw = ods, 
           hs_left_raw = hs_left, 
           hs_right_raw = hs_right,
           hs_diff_raw = hs_diff,
           hs_final_raw = hs_final,
           ill_left_raw = ill_left, 
           ill_right_raw = ill_right,
           ill_diff_raw = ill_diff,
           ill_final_raw = ill_final, 
           ill_ac_raw = ill_ac, 
           ill_extra_raw = ill_extra,
           sm_left_raw = sm_left, 
           sm_right_raw = sm_right,
           sm_diff_raw = sm_diff,
           sm_final_raw = sm_final,
           aslr_left_raw = aslr_left, 
           aslr_right_raw = aslr_right,
           aslr_diff_raw = aslr_diff,
           aslr_final_raw = aslr_final,
           rs_left_raw = rs_left, 
           rs_right_raw = rs_right,
           rs_diff_raw = rs_diff,
           rs_final_raw = rs_final, 
           tspu_raw = tspu, 
           fms_raw = fms, 
           fms_asym_raw = fms_asym,
           max_imbalance_raw = max_imbalance, 
           impulse_imbalance_raw = impulse_imbalance)
  
  data_final <- left_join(players_trim, data_final, by = "player")
  
  return(data_final %>%
           select(-player))
}
