######################### SCORING #########################

ba_scoring <- function(data_point, data_range, direction) {
  if (direction == "High") {
    return(round((data_point - min(data_range, na.rm = TRUE))/ (max(data_range, na.rm = TRUE) - min(data_range, na.rm = TRUE)) * 100))
  }
  else {
    return(round((max(data_range, na.rm = TRUE) - data_point)/ (max(data_range, na.rm = TRUE) - min(data_range, na.rm = TRUE)) * 100)) 
  }
}



######################### EXPLOSION #########################

get_explosion <- function(data) {
  require(tidyverse)
  data_trim <- data %>%
    select('Player',
           'Bench',
           'Vertical Jump',
           'Broad Jump')
  
  data_rename <- data_trim %>%
    rename('player' = 'Player') %>%
    rename('bench_raw' = 'Bench') %>%
    rename('vertical_jump_raw' = 'Vertical Jump') %>%
    rename('broad_jump_raw' = 'Broad Jump')
  
  
  data_scoring <- data_rename %>%
    mutate(bench_score = ba_scoring(bench_raw, data_rename$bench_raw, "High")) %>%
    mutate(vertical_jump_score = ba_scoring(vertical_jump_raw, data_rename$vertical_jump_raw, "High")) %>%
    mutate(broad_jump_score = ba_scoring(broad_jump_raw, data_rename$broad_jump_raw, "High"))
  
  data_final <- data_scoring
  return(data_final)
}



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

######################### BODY COMP #########################

get_bodycomp <- function(data) {
  data_trim <- data %>%
    select('Player',
           'Height',
           '2021 Weight',
           'Wingspan')
  
  data_rename <- data_trim %>%
    rename('player' = 'Player') %>%
    rename('height' = 'Height') %>%
    rename('weight' = '2021 Weight') %>%
    rename('wingspan' = 'Wingspan')
  
  data_final <- data_rename
  return(data_final)
}

######################### REACH #########################

get_reach <- function(data) {
  require(tidyverse)
  data_trim <- data %>%
    select('Player',
           'Delta 10',
           'Delta 20',
           'Delta 40',
           'Peak 40 MPH',
           '3-Cone',
           'Shuttle')
  
  data_rename <- data_trim %>%
    rename(player = 'Player') %>%
    rename('delta_10_raw' = 'Delta 10') %>%
    rename('delta_20_raw' = 'Delta 20') %>%
    rename('delta_40_raw' = 'Delta 40') %>%
    rename('peak_40_raw' = 'Peak 40 MPH') %>%
    rename('x3_cone_raw' = '3-Cone') %>%
    rename('shuttle_raw' = 'Shuttle')
  
  data_scoring <- data_rename %>%
    mutate(delta_10_score = ba_scoring(delta_10_raw, data_rename$delta_10_raw, "Low")) %>%
    mutate(delta_20_score = ba_scoring(delta_20_raw, data_rename$delta_20_raw, "Low")) %>%
    mutate(delta_40_score = ba_scoring(delta_40_raw, data_rename$delta_40_raw, "Low")) %>%
    mutate(peak_40_score = ba_scoring(peak_40_raw, data_rename$peak_40_raw, "Low")) %>%
    mutate(x3_cone_score = ba_scoring(x3_cone_raw, data_rename$x3_cone_raw, "Low")) %>%
    mutate(shuttle_score = ba_scoring(shuttle_raw, data_rename$shuttle_raw, "Low")) 
  
  data_final <- data_scoring
  return(data_final)
}

######################### MAIN #########################

get_data <- function(filename) {
  require(tidyverse)
  require(readxl)
  
  data <- read_excel(path = filename)

  reach_table <- get_reach(data)
  mobility_table <- get_mobility(data)
  explosion_table <- get_explosion(data)
  bodycomp_table <- get_bodycomp(data)
  
  data_a <- merge(reach_table, mobility_table)
  data_b <- merge(explosion_table, bodycomp_table)
  data_final <- merge(data_a, data_b)
  
  data_final[is.na(data_final)] <- "N/A"
  
  return(data_final)
}