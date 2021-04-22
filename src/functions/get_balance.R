######################### MOBILITY #########################

get_balance <- function(data1, data2) {
  require(tidyverse)
  
  data_trim_1 <- data1 %>%
    select('Player', 'ODS', 'HS (L/R=F)', 'ILL', 'SM', 'ASLR', 'TSPU', 'RS', 'FMS')
  data_trim_2 <- data2 %>%
    select('Name', 'Max.Imbalance....', 'Impulse.Imbalance....')
  
  data_split <- data_trim_1 %>%
    separate(col = 'HS (L/R=F)', c("hs_left_raw", "hs_right_raw", "hs_final_raw"), sep = "([/=])") %>%
    separate(col = 'ILL', c("ill_left_raw", "ill_right_raw", "ill_final_raw", "ill_ac_raw", "ill_extra"), sep = "([/=()])") %>%
    separate(col = 'SM', c("sm_left_raw", "sm_right_raw", "sm_final_raw"), sep = "([/=])") %>%
    separate(col = 'ASLR', c("aslr_left_raw", "aslr_right_raw", "aslr_final_raw"), sep = "([/=])") %>%
    separate(col = 'RS', c("rs_left_raw", "rs_right_raw", "rs_final_raw"), sep = "([/=])")

  data_rename_1 <- data_split %>%
    rename('player' = 'Player') %>%
    rename('ods_raw' = 'ODS') %>%
    rename('tspu_raw' = 'TSPU') %>%
    rename('fms_raw' = 'FMS')
  
  data_rename_2 <- data_trim_2 %>%
    rename('player' = 'Name') %>%
    rename('max_imbalance_raw' = 'Max.Imbalance....') %>%
    rename('impulse_imbalance_raw' = 'Impulse.Imbalance....')

  data_rename <- merge(data_rename_1, data_rename_2, by = 'player')
  
  #Handle AC for ill movement
  data_rename$ill_ac_raw[data_rename$ill_ac_raw == "AC"] <- 1
  data_rename$ill_ac_raw[data_rename$ill_ac_raw == "-AC"] <- -1
  data_rename$ill_ac_raw[is.na(data_rename$ill_ac_raw)] <- 0
  
  #Get FMS asymmetry 
  data_rename$hs_asym[data_rename$hs_left_raw != data_rename$hs_right_raw] <- 1
  data_rename$hs_asym[data_rename$hs_left_raw == data_rename$hs_right_raw] <- 0
  data_rename$ill_asym[data_rename$ill_left_raw != data_rename$ill_right_raw] <- 1
  data_rename$ill_asym[data_rename$ill_left_raw == data_rename$ill_right_raw] <- 0
  data_rename$sm_asym[data_rename$sm_left_raw != data_rename$sm_right_raw] <- 1
  data_rename$sm_asym[data_rename$sm_left_raw == data_rename$sm_right_raw] <- 0
  data_rename$aslr_asym[data_rename$aslr_left_raw != data_rename$aslr_right_raw] <- 1
  data_rename$aslr_asym[data_rename$aslr_left_raw == data_rename$aslr_right_raw] <- 0
  data_rename$rs_asym[data_rename$rs_left_raw != data_rename$rs_right_raw] <- 1
  data_rename$rs_asym[data_rename$rs_left_raw == data_rename$rs_right_raw] <- 0
  
  data_integer <- data_rename %>%
    mutate(fms_asym_raw = hs_asym + ill_asym + sm_asym + aslr_asym + rs_asym)
  
  
  #Ready for scores after removing columns
  data_integer <- data_integer %>%
    select(-ill_extra, -hs_asym, -ill_asym, -sm_asym, -aslr_asym, -rs_asym)
  
  data_integer_high <- data_integer%>%
    select(-max_imbalance_raw, -impulse_imbalance_raw,-fms_asym_raw)
  
  data_integer_low <- data_integer%>%
    select(player,max_imbalance_raw,impulse_imbalance_raw, fms_asym_raw)
  
  data_scoring1 <- percentile_function(data_integer_high,"high")
  data_scoring2 <- percentile_function(data_integer_low,"low")

  data_scoring <- merge(data_scoring1,data_scoring2)
  
  data_final <- data_scoring
  
  return(data_final)
}
