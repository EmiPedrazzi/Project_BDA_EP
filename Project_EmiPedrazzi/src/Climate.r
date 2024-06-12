##### Wrld climate data #####

library(sp)
library(geodata) 
library(raster)


spatial_points <- SpatialPoints(coords = matrix_full[, c("Longitude", "Latitude")], 
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# Set vectors for the climate variables, months and names for dataframe
# tavg	average temperature	[°C], prec	total precipitation	[mm], 
# srad	incident solar radiation	[kJ m-2 day-1]
# wind	wind speed (2 m above the ground)	[m s-1]
var <- c("tavg", "prec", "wind", "srad")
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
clim <- c("Temp_", "Prec_", "Wind_", "Srad_")
# Create an empty matrix to store the results (in a way that if we change the variables, the code still works)
matrix_clim <- matrix(NA, nrow = nrow(matrix_full), ncol = length(var) * length(months))

# Generate column names for the result matrix
col_names <- c()
for (c in clim) {
  for (m in months) {
    col_names <- c(col_names, paste0(c, "_", m))
  }
}
colnames(matrix_clim) <- col_names #assign column names to the empty matrix

# Loop through variables and months to extract data
for (i in seq_along(var)) { 
  # seq_along generates a sequence of elements of the same length of the vector var
  for (m in seq_along(months)) {
    # Load WorldClim data for each variable 
    sw_clim <- worldclim_country("switzerland", var = var[i], path = tempdir())
    sw_clim_br <- brick(sw_clim) # separate rasters for every month
    # Extract raster data for every month, one variable at the time
    raster <- sw_clim_br[[paste0("CHE_wc2.1_30s_", var[i], "_", m)]] # select the raster directly through its name
    raster_values <- raster::extract(raster, spatial_points, method = "bilinear") 
    # Calculate the column index in the result matrix
    col_index <- (i - 1) * length(months) + m 
    # Store the extracted data in the result matrix at the right column index
    matrix_clim[, col_index] <- raster_values
  }
}
matrix_clim <- data.frame(matrix_clim)

# To calculate the annual mean for every variable:
for (i in seq_along(clim)) {
  # Select the column corresponding to the current variable using grep:
  variable_columns <- grep(clim[i], colnames(matrix_clim))
  # Extraction of data in the selected column:
  data <- matrix_clim[, variable_columns]
  # Calculate the mean row by row of the selected columns (so the annual mean)
  Annual <- rowMeans(data, na.rm = TRUE)
  # Add the columns of the annual means to the data.frame matrix_clim
  colname <- paste0(clim[i], "Annual")
  matrix_clim <- cbind(matrix_clim, setNames(as.data.frame(Annual), colname))
}

# Add the data to matrix_full:
matrix_full <- cbind(matrix_full, matrix_clim)
matrix_full <- na.omit(matrix_full)