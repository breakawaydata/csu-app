######################### REACH #########################

get_reach <- function(data) {
  require(tidyverse)
  
  data_trim <- data %>%
    select('Player', 'Delta 10', 'Delta 20', 'Delta 40', 'Peak 40 MPH', '3-Cone', 'Shuttle')

  data_rename <- data_trim %>%
    rename(player = 'Player') %>%
    rename('delta_10' = 'Delta 10') %>%
    rename('delta_20' = 'Delta 20') %>%
    rename('delta_40' = 'Delta 40') %>%
    rename('peak_40' = 'Peak 40 MPH') %>%
    rename('x3_cone' = '3-Cone') %>%
    rename('shuttle' = 'Shuttle')
  
  data_scoring <- percentile_function(data_rename, "low")
  
  data_final <- data_scoring 
  
  return(data_final)
}
