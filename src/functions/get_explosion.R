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
