# Function for decimating data.

decimate <- function(df, size = 10) {
  df %>%
    dplyr::slice(seq(1, n(), by = size))
}
