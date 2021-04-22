######################### MOBILITY #########################

get_balance <- function(data1, data2) {
  require(tidyverse)
  
  data_trim_1 <- data1 %>%
    select('Player', 'ODS', 'HS (L/R=F)', 'ILL', 'SM', 'ASLR', 'TSPU', 'RS', 'FMS')
  data_trim_2 <- data2 %>%
    select('Name', 'Max.Imbalance....', 'Impulse.Imbalance....')
  
  data_split <- data_trim_1 %>%
    separate(col = 'HS (L/R=F)', c("hs_left", "hs_right", "hs_final"), sep = "([/=])") %>%
    separate(col = 'ILL', c("ill_left", "ill_right", "ill_final", "ill_ac", "ill_extra"), sep = "([/=()])") %>%
    separate(col = 'SM', c("sm_left", "sm_right", "sm_final"), sep = "([/=])") %>%
    separate(col = 'ASLR', c("aslr_left", "aslr_right", "aslr_final"), sep = "([/=])") %>%
    separate(col = 'RS', c("rs_left", "rs_right", "rs_final"), sep = "([/=])")

  data_rename_1 <- data_split %>%
    rename('player' = 'Player') %>%
    rename('ods' = 'ODS') %>%
    rename('tspu' = 'TSPU') %>%
    rename('fms' = 'FMS')
  
  data_rename_2 <- data_trim_2 %>%
    rename('player' = 'Name') %>%
    rename('max_imbalance' = 'Max.Imbalance....') %>%
    rename('impulse_imbalance' = 'Impulse.Imbalance....')

  data_rename <- merge(data_rename_1, data_rename_2, by = 'player', all = TRUE)
  
  #Handle AC for ill movement
  data_rename$ill_ac[data_rename$ill_ac == "AC"] <- 1
  data_rename$ill_ac[data_rename$ill_ac == "-AC"] <- -1
  data_rename$ill_ac[is.na(data_rename$ill_ac)] <- 0
  
  #Get FMS asymmetry 
  data_rename$hs_asym[data_rename$hs_left != data_rename$hs_right] <- 1
  data_rename$hs_asym[data_rename$hs_left == data_rename$hs_right] <- 0
  data_rename$ill_asym[data_rename$ill_left != data_rename$ill_right] <- 1
  data_rename$ill_asym[data_rename$ill_left == data_rename$ill_right] <- 0
  data_rename$sm_asym[data_rename$sm_left != data_rename$sm_right] <- 1
  data_rename$sm_asym[data_rename$sm_left == data_rename$sm_right] <- 0
  data_rename$aslr_asym[data_rename$aslr_left != data_rename$aslr_right] <- 1
  data_rename$aslr_asym[data_rename$aslr_left == data_rename$aslr_right] <- 0
  data_rename$rs_asym[data_rename$rs_left != data_rename$rs_right] <- 1
  data_rename$rs_asym[data_rename$rs_left == data_rename$rs_right] <- 0
  
  data_integer <- data_rename %>%
    mutate(fms_asym = hs_asym + ill_asym + sm_asym + aslr_asym + rs_asym)
  
  
  #Ready for scores after removing columns
  data_integer <- data_integer %>%
    select(-ill_extra, -hs_asym, -ill_asym, -sm_asym, -aslr_asym, -rs_asym)
  
  data_integer_high <- data_integer%>%
    select(-max_imbalance, -impulse_imbalance, -fms_asym)
  
  data_integer_low <- data_integer%>%
    select(player,max_imbalance, impulse_imbalance, fms_asym)
  
  data_scoring1 <- percentile_function(data_integer_high,"high")
  data_scoring2 <- percentile_function(data_integer_low,"low")

  data_scoring <- merge(data_scoring1,data_scoring2, all = TRUE)
  
  data_pillar <- ba_scoring(data_scoring, 
                            c('player', 'fms_score', 'ods_score', 'tspu_score', 
                            'hs_left_score', 'hs_right_score',
                            'ill_left_score', 'ill_right_score',
                            'sm_left_score', 'sm_right_score',
                            'aslr_left_score', 'aslr_right_score',
                            'rs_left_score', 'rs_right_score'),
                            c('player', 'fms_asym_score', 'max_imbalance_score', 'impulse_imbalance_score'),
                            "balance")
  
  data_final <- merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  
  return(data_final)
}
