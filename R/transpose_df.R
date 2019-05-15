# Function for transposing tibbles.
# From Indrajeet Patil: https://stackoverflow.com/questions/42790219/how-do-i-transpose-a-tibble-in-r

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column() %>%
    tibble::as_data_frame(x = .)
  return(t_df)
}
