##### Elevation #####

library(sf)
library(sp)
library(elevatr)
library(raster)
library(rnaturalearth)

sf_use_s2(FALSE)

# Delimit the range to Switzerland:
Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" ) # to get the polygon
elevation_switzerland <- get_elev_raster(Switzerland, z = 8) 
# Provides access to the Amazon Web Services Terrain Tiles and the Open Topography global datasets API. 
# From a dataframe (long, lat) returns a raster.

# Crop et mask the raster and limit it to Switzerland:
r2 <- raster::crop(elevation_switzerland, extent(Switzerland)) 
elevation_switzerland <- raster::mask(r2, Switzerland)

# Create a data frame for our GBIF coordoinates and extract elevation values:
latitude <- matrix_full$Latitude
longitude <- matrix_full$Longitude
gbif_coord <- data.frame(longitude, latitude)
ll_prj <- "EPSG:4326" 
spatial_points <- sp::SpatialPoints(gbif_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
Elevation <- raster::extract(elevation_switzerland, spatial_points, method='bilinear')

# Transform dataframe:
Elevation <- data.frame(Elevation)
# Add it to the full matrix:
matrix_full <- cbind(matrix_full, Elevation)
matrix_full <- na.omit(matrix_full)