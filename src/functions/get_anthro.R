
#Links data for anthro data on a player
get_anthro <- function(players, data) {
  data_trim <- data %>%
    select('player',
           'height',
           'weight',
           'wingspan')

  data_final <- left_join(players, data_trim, by = "player")
  
  return(data_final)
}
