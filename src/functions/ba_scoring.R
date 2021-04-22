

### First take in a table, list of first pillar, list of second pillar
ba_scoring <- function(data, first_pillar, second_pillar, pillar) {
  data_first <- data[,first_pillar]
  data_second <- data[,second_pillar]
  
  data_first$first_pillar_score = rowMeans(data_first[,seq(2,length(first_pillar))], na.rm = TRUE)
  data_second$second_pillar_score = rowMeans(data_second[,seq(2,length(second_pillar))], na.rm = TRUE)
  
  data_score <- merge(data_first, data_second, by = "player", all = TRUE) %>%
    select(player, first_pillar_score, second_pillar_score)
  
  data_score$pillar_score = rowMeans(data_score[,c(2,3)], na.rm = TRUE)
  
  data_score$pillar_score <- round(data_score$pillar_score)
  data_score$first_pillar_score <- round(data_score$first_pillar_score)
  data_score$second_pillar_score <- round(data_score$second_pillar_score)
  
  if (pillar == "reach") {
    data_score <- data_score %>%
      rename(speed_score = first_pillar_score) %>%
      rename(agility_score = second_pillar_score) %>%
      rename(reach_score = pillar_score)
  }
  else if (pillar == "explosion") {
    data_score <- data_score %>%
      rename(strength_score = first_pillar_score) %>%
      rename(power_score = second_pillar_score) %>%
      rename(explosion_score = pillar_score)
  }
  else if (pillar == "balance") {
    data_score <- data_score %>%
      rename(mobility_score = first_pillar_score) %>%
      rename(stability_score = second_pillar_score) %>%
      rename(balance_score = pillar_score)
  }
  else {
    data_score <- data_score
  }
  return(data_score)
}