######################### BODY COMP #########################

get_anthro <- function(data) {
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
