# Preparation of the data for plotting and the analysis

library(stats)

# Create a new dataset, Delete NA 
df <- base::subset(matrix_full, select = -Source) #we don't need the source for the analysis
df <- na.omit(df)

# Loop through each column to separate the variables based on type (factor or numeric)
df$Ecosystem <- as.factor(df$Ecosystem)
df_discr <- NULL
df_con <- NULL
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    df_con[[col]] <- df[[col]]
  } else {
    df_discr[[col]] <- df[[col]]
  }
}
df_discr <- data.frame(df_discr)
df_con <- data.frame(df_con)
#scale the data between 0 and 1
scale_to_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Columns to be excluded from scaling
exclude_cols <- c("Longitude", "Latitude", "Red", "Green", "Blue")
# Identify columns to be scaled
scale_cols <- setdiff(names(df_con), exclude_cols)
# Apply scaling to the selected columns
df_con[scale_cols] <- apply(df_con[scale_cols], 2, scale_to_01)
# Create a dataframe with all the data:
df_scaled <- cbind(df_discr, df_con)
# It's important that the real matrix (matrix_full) is untouched, in case we need "real" data for plotting or whatever, 
# and so that we do not compromise the data during the scaling.