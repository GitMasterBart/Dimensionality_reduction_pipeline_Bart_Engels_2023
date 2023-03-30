change_NBB_Id <- function(df) {
  row.names(df) <- gsub(" ", ".", row.names(df))
  row.names(df) <- gsub("-", ".", row.names(df))
  return (df)
}