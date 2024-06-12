##### NDVI #####
library(sf)
library(sp)
library(raster)
library(rgeoboundaries)
library(stats)

# Download the country boundary of switzerland:
map_boundary <- geoboundaries("switzerland") 

dir.create("./data/modis", recursive = TRUE) # Create a folder "modis" in the existing folder "data"
spatial_filepath <- "./data/modis/switzerland.shp" # Defining filepath to save downloaded spatial file 
st_write(map_boundary, paste0(spatial_filepath)) # Saving downloaded spatial file 
NDVI_raster <- raster("./data/NDVI.tif") # Reading in the NDVI raster data 

NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Transform the data
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary)) # Crop the data

spatial_points <- SpatialPoints(coords = matrix_full[, c("Longitude","Latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
# Extract the values:
NDVI <- raster::extract(NDVI_raster, spatial_points)

# Add data on the full matrix:
matrix_full <- cbind(matrix_full, NDVI)
matrix_full <- na.omit(matrix_full)