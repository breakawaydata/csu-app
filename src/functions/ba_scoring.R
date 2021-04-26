

### First take in a table, list of first pillar, list of second pillar
ba_scoring <- function(data, first_pillar, second_pillar, pillar) {

  #Get list of elements for the first and second pillar
  data_first <- data[,first_pillar]
  data_second <- data[,second_pillar]
  
  #Get your sub pillar scores
  data_first$first_pillar_score = rowMeans(data_first[,seq(2,length(first_pillar))], na.rm = TRUE)
  data_first$first_pillar_score = round(scales::rescale(data_first$first_pillar_score, to = c(40, 100)))
  data_second$second_pillar_score = rowMeans(data_second[,seq(2,length(second_pillar))], na.rm = TRUE)
  data_second$second_pillar_score = round(scales::rescale(data_second$second_pillar_score, to = c(40, 100)))
  
  #Merge scores for each pillar together 
  data_score <- merge(data_first, data_second, by = "player", all = TRUE) %>%
    select(player, first_pillar_score, second_pillar_score)
  
  #Calculate pillar score
  data_score$pillar_score = rowMeans(data_score[,c(2,3)], na.rm = TRUE)
  
  #Round up pillars to whole numbers now calculations are complete
  data_score$pillar_score <- round(scales::rescale(data_score$pillar_score, to = c(40, 100)))
  data_score$first_pillar_score <- round(data_score$first_pillar_score)
  data_score$second_pillar_score <- round(data_score$second_pillar_score)
  
  #Rename based on pillar 
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
  
  #Return scores
  return(data_score)
}