# export_gender_data.R
run_only_once <- FALSE
if (run_only_once) {
  index <- which(sapply(df, is.list))
  names(df)[index]
  df <- dplyr::select(df, -EDITOR, -CATEGORY, -CORRESPONDENCE_ADDRESS, -NOTE)
  write.csv(
    df,
    file = 'data/gender_in_musicology_exported.csv',
    row.names = FALSE
  )
}
