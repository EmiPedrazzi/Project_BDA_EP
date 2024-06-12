##### Ecosystem #####

library(raster)
library(rnaturalearth)
library(sp)
library(stats)

# Set the file path to THE GeoTIFF:
file_path <- "./Data/WorldEcosystem.tif"
# Read the raster GeoTIFF:
ecosystem_raster <- raster(file_path)
# Set path to the metadata file:
metadat_eco <- read.delim("./Data/WorldEcosystem.metadata.tsv")

#Delimit the range to Switzerland:
Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" )
## crop and mask:
r2 <- crop(ecosystem_raster, extent(Switzerland))
ecosystem_switzerland <- mask(r2, Switzerland)

# Extract values:
spatial_points <- SpatialPoints(coords = matrix_full[, c("Longitude","Latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
Ecosystem <- raster::extract(ecosystem_switzerland, spatial_points)
matrix_full <- cbind(matrix_full, Ecosystem)
# To eliminate the NA:
matrix_full <- na.omit(matrix_full)
# Merge the ecosystem values to the metadata:
metadat_eco <- read.delim("./Data/WorldEcosystem.metadata.tsv")
matrix_full <- merge(matrix_full, metadat_eco, by.x="Ecosystem", by.y="Value")