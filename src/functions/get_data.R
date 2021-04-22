######################### MAIN #########################

get_data <- function(filename1, filename2, filename3) {
  require(tidyverse)
  
  players <- read.csv("data/players.csv") %>%
    mutate(player = paste(first,last, sep = " "))
  
  players_trim <- players %>%
    select(player, player_id, first, last, suffix)
  
  data_source_1 <- readxl::read_excel(filename1)
  data_source_2 <- read.csv(filename2, fileEncoding="UTF-8-BOM")
  data_source_3 <- read.csv(filename3, fileEncoding="UTF-8-BOM")
  
  data_source_2 <- data_source_2 %>%
    mutate(player = paste(First.name, Last.name, sep = " ")) %>%
    mutate(date = as.Date(Date, format = "%m/%d/%y"))
  
  data_source_3 <- data_source_3 %>%
    separate('Date.UTC', c("day", "month", "year"), "/") %>%
    mutate(date = as.Date(paste(month,day,year, sep = "/"), format = "%m/%d/%y"))
  
  dates_2 <- data_source_2 %>%
    group_by(player) %>%
    summarize(recent_date = max(date, na.rm = FALSE), .groups = 'drop')

  dates_3 <- data_source_3 %>%
    group_by(Name) %>%
    summarize(recent_date = max(date, na.rm = FALSE), .groups = 'drop')
  
  
  data_source_2_final <- data.frame()
  data_source_3_final <- data.frame()

  for (i in seq(1, nrow(dates_2))){
    info <- data_source_2 %>%
      filter(player == dates_2$player[[i]],
             date == dates_2$recent_date[[i]])
    
    data_source_2_final <- rbind(data_source_2_final,info)
  }
  
  for (i in seq(1, nrow(dates_3))){
    info <- data_source_3 %>%
      filter(Name == dates_3$Name[[i]],
             date == dates_3$recent_date[[i]])
    
    data_source_3_final <- rbind(data_source_3_final,info)
  }
  
  reach_table <- left_join(players_trim, get_reach(data_source_1), by = "player")
  balance_table <- left_join(players_trim, get_balance(data_source_1, data_source_3_final), by = "player")
  explosion_table <- left_join(players_trim, get_explosion(data_source_1, data_source_2_final, data_source_3_final), by = "player")

  anthro_table <- left_join(players, get_anthro(data_source_1), by = "player")

  write.csv(reach_table, "data/reach_data_1.csv")
  write.csv(balance_table, "data/balance_data_1.csv")
  write.csv(explosion_table, "data/explosion_data_1.csv")
  write.csv(anthro_table, "data/anthro_data_1.csv")

  return()
}
