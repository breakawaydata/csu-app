
#Gets percentile for individual metrics
percentile_function <- function(data, direction){
  if (direction == "high"){
    percentiles <- data %>%
      mutate_all(funs(score = round(percent_rank(abs(as.numeric(.)))*100)))
  }
  else {
    percentiles <- data %>%
      mutate_all(funs(score = 100 - round(percent_rank(abs(as.numeric(.)))*100)))
  }
  return(percentiles %>% select(-player_score))
}