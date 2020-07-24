# Function for linear modeling of initial rate. Used for easy purrr::map.
# Model duration is number of points. E.g. 0, 1, 2, 3, 4 min = 5
initial_rate <- function(df, start_modeltime = 0, num_timepoints = 5) {
  
  end_modeltime <- start_modeltime + num_timepoints - 1
  
  # Check if the end model time is greater than the latest acquired timepoint
  if (end_modeltime > max(df$time_m)) {
    return(NA)
  }
  
  df <- df %>%
    filter(time_m >= start_modeltime, time_m <= end_modeltime)

  lm(abs ~ time_m, data = df)
}
