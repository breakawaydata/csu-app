######################### SCORING #########################

ba_scoring <- function(data_point, data_range, direction) {
  if (direction == "High") {
    return(round((data_point - min(data_range, na.rm = TRUE))/ (max(data_range, na.rm = TRUE) - min(data_range, na.rm = TRUE)) * 100))
  }
  else {
    return(round((max(data_range, na.rm = TRUE) - data_point)/ (max(data_range, na.rm = TRUE) - min(data_range, na.rm = TRUE)) * 100))
  }
}
