##### Import gbif data #####

library(rgbif)         # For accessing GBIF occurrence data
library(stats)

#### SPIDER ####
spider_gbif <- c("Argiope bruennichi") #called AB
# Extract the data and create dataframe:
gbif_data_AB_CH <- occ_data(scientificName = spider_gbif, hasCoordinate = TRUE, 
                            limit = 1000, country=c("CH"))
occur_AB_CH <- data.frame(gbif_data_AB_CH$data) 
matrix_gbif_AB_CH <- data.frame(occur_AB_CH$scientificName, 
                                occur_AB_CH$decimalLatitude, 
                                occur_AB_CH$decimalLongitude)
Source_AB_gbif <- rep("gbif", nrow(matrix_gbif_AB_CH))
matrix_gbif_AB_CH <- cbind(matrix_gbif_AB_CH, Source_AB_gbif)
colnames(matrix_gbif_AB_CH) <- c("Species", "Latitude", "Longitude", "Source")

matrix_AB <- matrix_gbif_AB_CH 
#seems useless, but in case I want to add data of other countries, I don't have 
#to change everything that I have already done.
#Same thing about adding the column "source". Useless, because all the data come from gbif,
#but you never know.

#### DRAGONFLY ####
# Broad-bodied Chaser Libellula depressa:
Libellula_depressa <- c("Libellula depressa") #CALLED LD
gbif_data_LD_CH <- occ_data(scientificName = Libellula_depressa, 
                            hasCoordinate = TRUE, limit = 1000, country=c("CH"))
occur_LD_CH <- data.frame(gbif_data_LD_CH$data) 
matrix_gbif_LD_CH <- data.frame(occur_LD_CH$scientificName, 
                                occur_LD_CH$decimalLatitude, 
                                occur_LD_CH$decimalLongitude)
Source_LD_gbif <- rep("gbif", nrow(matrix_gbif_LD_CH))
matrix_gbif_LD_CH <- cbind(matrix_gbif_LD_CH, Source_LD_gbif)
colnames(matrix_gbif_LD_CH) <- c("Species", "Latitude", "Longitude", "Source")

matrix_LD <- matrix_gbif_LD_CH

#### DAMSELFLY ####
# Common Darter Sympetrum striolatum:
Sympetrum_striolatum <- c("Sympetrum striolatum")
gbif_data_SST_CH <- occ_data(scientificName = Sympetrum_striolatum, 
                             hasCoordinate = TRUE, limit = 1000, country=c("CH"))
occur_SST_CH <- data.frame(gbif_data_SST_CH$data) 
matrix_gbif_SST_CH <- data.frame(occur_SST_CH$scientificName, 
                                 occur_SST_CH$decimalLatitude, 
                                 occur_SST_CH$decimalLongitude)
Source_SST_gbif <- rep("gbif", nrow(matrix_gbif_SST_CH))
matrix_gbif_SST_CH <- cbind(matrix_gbif_SST_CH, Source_SST_gbif)
colnames(matrix_gbif_SST_CH) <- c("Species", "Latitude", "Longitude", "Source")

matrix_SST <- matrix_gbif_SST_CH

#final matrix:
matrix_full <- rbind(matrix_AB, matrix_LD, matrix_SST)
matrix_full <- data.frame(matrix_full)
matrix_full <- na.omit(matrix_full)

# Change species names to make it easier later:
# Using grep we make sure to select all the species, even if the name has something more afterwards (like "Linneaus")
matrix_full$Species[grep("Argiope bruennichi", matrix_full$Species)] <- "Argiope bruennichi"
matrix_full$Species[grep("Libellula depressa", matrix_full$Species)] <- "Libellula depressa"
matrix_full$Species[grep("Sympetrum striolatum", matrix_full$Species)] <- "Sympetrum striolatum"