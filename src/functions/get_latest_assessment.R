

#Requires the data set have a player column and date column labeled as such
get_latest_assessment <- function(data) { 

  #Select entries with the most recent dates for players
  dates <- data %>%
    group_by(player) %>%
    summarize(recent_date = max(date, na.rm = FALSE), .groups = 'drop')

  
  #Pull data for each player with corresponding data
  data_source <- data.frame()

  #Iterate through each player
  for (i in seq(1, nrow(dates))){
    #Grab info for player and date
    info <- data %>%
      filter(player == dates$player[[i]],
             date == dates$recent_date[[i]])
    #Append to new data table - data_source
    data_source <- rbind(data_source,info)
  }
  #Return the trimmed down, single entry data source
  return(data_source)
}
