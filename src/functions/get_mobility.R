######################### MOBILITY #########################

get_mobility <- function(data) {
  require(tidyverse)
  data_trim <- data %>%
    select('Player',
           'ODS',
           'HS (L/R=F)',
           'ILL',
           'SM',
           'ASLR',
           'TSPU',
           'RS',
           'FMS')

  data_split <- data_trim %>%
    separate(col = 'HS (L/R=F)', c("hs_left_raw", "hs_right_raw", "hs_final_raw"), sep = "([/=])") %>%
    separate(col = 'ILL', c("ill_left_raw", "ill_right_raw", "ill_final_raw", "ill_ac_raw", "ill_extra"), sep = "([/=()])") %>%
    separate(col = 'SM', c("sm_left_raw", "sm_right_raw", "sm_final_raw"), sep = "([/=])") %>%
    separate(col = 'ASLR', c("aslr_left_raw", "aslr_right_raw", "aslr_final_raw"), sep = "([/=])") %>%
    separate(col = 'RS', c("rs_left_raw", "rs_right_raw", "rs_final_raw"), sep = "([/=])")

  data_rename <- data_split %>%
    rename('player' = 'Player') %>%
    rename('ods_raw' = 'ODS') %>%
    rename('tspu_raw' = 'TSPU') %>%
    rename('fms_raw' = 'FMS')

  data_integer <- data_rename %>%
    mutate(hs_left_raw = as.numeric(hs_left_raw)) %>%
    mutate(hs_right_raw = as.numeric(hs_right_raw)) %>%
    mutate(hs_final_raw = as.numeric(hs_final_raw)) %>%
    mutate(ill_left_raw = as.numeric(ill_left_raw)) %>%
    mutate(ill_right_raw = as.numeric(ill_right_raw)) %>%
    mutate(ill_final_raw = as.numeric(ill_final_raw)) %>%
    mutate(ill_ac_raw = as.character(ill_ac_raw)) %>%
    mutate(sm_left_raw = as.numeric(sm_left_raw)) %>%
    mutate(sm_right_raw = as.numeric(sm_right_raw)) %>%
    mutate(sm_final_raw = as.numeric(sm_final_raw)) %>%
    mutate(aslr_left_raw = as.numeric(aslr_left_raw)) %>%
    mutate(aslr_right_raw = as.numeric(fms_raw)) %>%
    mutate(aslr_final_raw = as.numeric(aslr_final_raw)) %>%
    mutate(tspu_raw = as.numeric(tspu_raw)) %>%
    mutate(rs_left_raw = as.numeric(rs_left_raw)) %>%
    mutate(rs_right_raw = as.numeric(rs_right_raw)) %>%
    mutate(rs_final_raw = as.numeric(rs_final_raw))

  data_scoring <- data_integer %>%
    mutate(ods_score = ba_scoring(ods_raw, data_integer$ods_raw, "High")) %>%
    mutate(hs_left_score = ba_scoring(hs_left_raw, data_integer$hs_left_raw, "High")) %>%
    mutate(hs_right_score = ba_scoring(hs_right_raw, data_integer$hs_right_raw, "High")) %>%
    mutate(hs_final_score = ba_scoring(hs_final_raw, data_integer$hs_final_raw, "High")) %>%
    mutate(ill_left_score = ba_scoring(ill_left_raw, data_integer$ill_left_raw, "High")) %>%
    mutate(ill_right_score = ba_scoring(ill_right_raw, data_integer$ill_right_raw, "High")) %>%
    mutate(ill_final_score = ba_scoring(ill_final_raw, data_integer$ill_final_raw, "High")) %>%
    mutate(sm_left_score = ba_scoring(sm_left_raw, data_integer$sm_left_raw, "High")) %>%
    mutate(sm_right_score = ba_scoring(sm_right_raw, data_integer$sm_right_raw, "High")) %>%
    mutate(sm_final_score = ba_scoring(sm_final_raw, data_integer$sm_final_raw, "High")) %>%
    mutate(aslr_left_score = ba_scoring(aslr_left_raw, data_integer$aslr_left_raw, "High")) %>%
    mutate(aslr_right_score = ba_scoring(fms_raw, data_integer$fms_raw, "High")) %>%
    mutate(aslr_final_score = ba_scoring(aslr_final_raw, data_integer$aslr_final_raw, "High")) %>%
    mutate(tspu_score = ba_scoring(tspu_raw, data_integer$tspu_raw, "High")) %>%
    mutate(rs_left_score = ba_scoring(rs_left_raw, data_integer$rs_left_raw, "High")) %>%
    mutate(rs_right_score = ba_scoring(rs_right_raw, data_integer$rs_right_raw, "High")) %>%
    mutate(rs_final_score = ba_scoring(rs_final_raw, data_integer$rs_final_raw, "High"))

  data_scoring$ill_ac_raw[data_scoring$ill_ac_raw == "AC"] <- 1
  data_scoring$ill_ac_raw[data_scoring$ill_ac_raw == "-AC"] <- -1
  data_scoring$ill_ac_raw[is.na(data_scoring$ill_ac_raw)] <- 0

  data_final <- data_scoring %>%
    select(-ill_extra)

  return(data_final)
}
