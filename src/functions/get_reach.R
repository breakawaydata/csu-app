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
    mutate(delta_10_score = percent_rank(delta_10_raw)) %>%
    mutate(delta_20_score = ba_scoring(delta_20_raw, data_rename$delta_20_raw, "Low")) %>%
    mutate(delta_40_score = ba_scoring(delta_40_raw, data_rename$delta_40_raw, "Low")) %>%
    mutate(peak_40_score = ba_scoring(peak_40_raw, data_rename$peak_40_raw, "Low")) %>%
    mutate(x3_cone_score = ba_scoring(x3_cone_raw, data_rename$x3_cone_raw, "Low")) %>%
    mutate(shuttle_score = ba_scoring(shuttle_raw, data_rename$shuttle_raw, "Low"))

  data_final <- data_scoring
  return(data_final)
}
