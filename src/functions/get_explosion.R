######################### EXPLOSION #########################

get_explosion <- function(data1, data2, data3) {
  require(tidyverse)
  
  data_trim_1 <- data1 %>%
    select('Player', 'Bench', 'Vertical Jump', 'Broad Jump')
  
  data_trim_2 <- data2 %>%
    select('player', 'Load.t.score', 'Explode.t.score', 'Drive.t.score')
  
  data_trim_3 <- data3 %>%
    select('Name', 'L.Max.Force..N.', 'R.Max.Force..N.', 'L.Max.Impulse..Ns.', 'R.Max.Impulse..Ns.')
  
  data_rename_1 <- data_trim_1 %>%
    rename('player' = 'Player') %>%
    rename('bench' = 'Bench') %>%
    rename('vertical_jump' = 'Vertical Jump') %>%
    rename('broad_jump' = 'Broad Jump')
  
  data_rename_2 <- data_trim_2 %>%
    rename(sparta_load = 'Load.t.score',
           sparta_explode ='Explode.t.score',
           sparta_drive ='Drive.t.score')
  
  data_rename_3 <- data_trim_3 %>%
    rename('player' = 'Name') %>%
    rename('max_force_left'= 'L.Max.Force..N.') %>%
    rename('max_force_right' = 'R.Max.Force..N.') %>%
    rename('max_impulse_left' = 'L.Max.Impulse..Ns.') %>%
    rename('max_impulse_right' = 'R.Max.Impulse..Ns.')
   
  data_rename <- merge(data_rename_1,data_rename_2, all = TRUE)
  data_rename <- merge(data_rename, data_rename_3, all = TRUE)

  data_integer <- data_rename %>%
    mutate('max_force_left' = as.numeric(max_force_left)) %>%
    mutate('max_force_right' = as.numeric(max_force_right)) %>%
    mutate('max_impulse_left' = as.numeric(max_impulse_left)) %>%
    mutate('max_impulse_right' = as.numeric(max_impulse_right))
  
  data_calculating <- data_integer %>%
    mutate('max_force' = max_force_left + max_force_right) %>%
    mutate('max_impulse' = max_impulse_left + max_impulse_right)
  
  data_ready <- data_calculating%>%
    select(-max_force_left, -max_force_right, -max_impulse_left,-max_impulse_right)
  
  data_scoring <- percentile_function(data_ready,"high")
  
  data_pillar <- ba_scoring(data_scoring, 
                            c('player', 'bench_score', 'max_force_score', 'max_impulse_score'),
                            c('player', 'broad_jump_score', 'vertical_jump_score','sparta_load_score', 
                              'sparta_drive_score', 'sparta_explode_score'),
                            "explosion")
  
  data_final <- merge(data_pillar, data_scoring, by = 'player', all = TRUE) 
  
  
  return(data_final)
}
