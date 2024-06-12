# Plotting

library(sp)
library(ggplot2)
library(rnaturalearth) 
library(rnaturalearthhires)
library(sf)
library(elevatr)
library(raster)
library(rayshader) 
library(eks)
library(png)
library(imagefx)

##################################################################################

##### Visualization of species occurrences #####
switzerland  <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", country = "switzerland")
windows()
ggplot(data = switzerland ) +
  geom_sf()   +
  geom_point(data =matrix_full, aes(x = Longitude, y = Latitude,fill=Species), size = 1, 
             shape = 23) + theme_classic()

windows()
plot(ecosystem_switzerland, main="Ecosystems plot")
spatial_points <- SpatialPoints(coords = matrix_full[, c("Longitude","Latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
# Determine the color for each point based on the species
species_colors <- c("Argiope bruennichi" = "darkred",
                    "Libellula depressa" = "darkgreen",
                    "Sympetrum striolatum" = "blue")
point_colors <- species_colors[matrix_full$Species]
# Plot the spatial points with the specified colors
plot(spatial_points, add = TRUE, pch = 16, cex = 0.5, col = point_colors)
legend("topright", legend = names(species_colors), col = species_colors, pch = 16)
# It seems that in general A. bruennichi and S. striolatum are more overlapped than L. depressa, 
# which seems more isolated. 

##### Visualization of frequences of species for each ecosystem type #####
windows()
ggplot(matrix_full, aes(x = Landcover, fill = Species)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequence of species per ecosystem type",
       x = "Landcover",
       y = "Frequence",
       fill = "Species") +
  theme_minimal()
# The species are present in each ecosystem with pretty much the same frequence,
#(Cropland, forest, grassland, settlement)
# with a preference for cropland for all three species.
# In shrubland, only Agriope bruennichi is present, 
# and in sparsly or non vegetated ecosystem only the damselfy and the dragonfly are present,
# all with a very low frequence.


##### Visualization of data for elevation #####
# Setting of spatial points and colors for each species
spatial_points <- SpatialPoints(coords = matrix_full[, c("Longitude","Latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
colors <- ifelse(matrix_full$Species == "Argiope bruennichi", "red", 
                ifelse(matrix_full$Species == "Libellula depressa", "darkgreen", "blue"))

sf_use_s2(FALSE)
windows()
Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" )
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)
r2 <- crop(elevation_switzerland, extent(Switzerland))
elevation_switzerland <- mask(r2, Switzerland)
plot(elevation_switzerland, main="Elevation plot")
plot(spatial_points, col=colors, add=T, pch=16,cex=0.5)
# It seems that all three species prefer lower elevations. Maybe only L. depressa prefers higher elevations.
# Let's analyse it better with a 3D plot:
elmat <- raster_to_matrix(elevation_switzerland)
attr(elmat, "extent") <- extent(elevation_switzerland)
elmat %>% 
  sphere_shade(texture = "bw") %>%
plot_3d(elmat, zscale = 50, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))
# Render points on the 3D elevation map
elevation_points <- extract(elevation_switzerland, spatial_points, method='bilinear')
colors <- ifelse(matrix_full$Species == "Argiope bruennichi", "red", 
                ifelse(matrix_full$Species == "Libellula depressa", "green", "blue"))
render_points(
  extent = extent(Switzerland), size = 5,
  lat = matrix_full$Latitude, 
  long = matrix_full$Longitude,
  altitude = elevation_points + 100, 
  zscale = 50, 
  color = colors, 
)
# From the plot it seems that L. depressa is found at higher elevations compared to the other two species.
# Let's check it with a density plot.

##########################################################################################

# Density plot for the climate variable

# Plot for the Elevation
windows()
ggplot(matrix_full, aes(x=Elevation,fill=factor(Species))) + 
  geom_density(adjust = 1,alpha=0.5)+                   
  theme_classic()+
  labs(x = "Elevation",    
     y = "Density",               
     title = "Density Plot of Elevation")   
# The pattern of A. bruennichi and S. striolatum are quite similar, with
# S. striolatum that prefers lower elevation.
# while L. depressa seems to prefer higher elevations.


# Plot for the Temperature [°C]
windows()
ggplot(matrix_full, aes(x=Temp_Annual,fill=factor(Species))) +
  geom_density(adjust = 3,alpha=0.5)+
  scale_fill_manual(values=c("darkred","darkgreen","blue"))+
  theme_classic()+
   labs(x = "Temperature",    
       y = "Density",               
       title = "Density Plot of Temperature")
# We find S. striolatum at a higher density with higher temperatures, 
# while A. bruennichi and L. depressa overlap, and are present even at lower temperatures. 


# Plot for the Precipitation [mm]
windows()
ggplot(matrix_full, aes(x=Prec_Annual,fill=factor(Species))) +
  geom_density(adjust = 3,alpha=0.5)+
  scale_fill_manual(values=c("darkred","darkgreen","blue"))+
  theme_classic()+
  labs(x = "Precipitation",    
       y = "Density",               
       title = "Density Plot of Precipitation")
# S. striolatum and A. bruennichi overlap.
# S. striolatum is and A. bruennichi are denser at precipitation around 100mm, 
# while L. depressa is less dense and has a wider range.

# Plot for the wind (2 m above the ground)	[m s-1]
windows()
ggplot(matrix_full, aes(x=Wind_Annual,fill=factor(Species))) +
  geom_density(adjust = 3,alpha=0.5)+
  scale_fill_manual(values=c("darkred","darkgreen","blue"))+
  theme_classic()+
  labs(x = "Wind",    
       y = "Density",               
       title = "Density Plot of Wind")
# S. striolatum seems to be more sensible to wind and denser at smaller values,
# while the other two species more resistant, and with wider ranges that overlap in part.

# Plot for the incident solar radiation	[kJ m-2 day-1]
windows()
ggplot(matrix_full, aes(x=Srad_Annual,fill=factor(Species))) +
  geom_density(adjust = 3,alpha=0.5)+
  scale_fill_manual(values=c("darkred","darkgreen","blue"))+
  theme_classic()+
  labs(x = "incident solar radiation",    
       y = "Density",               
       title = "Density Plot of incident solar radiation")
# L. depressa have a slight wider range. S. striolatum and A. bruennichi overlap.
# The three species prefers small values of incident solar radiation (12'000).


# S. striolatum and A. bruennichi overlap for the precipitation and incident solar radiation,
# while L. depressa and A. bruennichi overlap for the wind and the temperature.

##########################################################################################

###### Density plot #####

elevation.texture.map <- readPNG("./Data/Switzerland2.png")
sf_use_s2(FALSE)
# Create a data frame with latitude and longitude columns
df_points <- data.frame(lat = matrix_full$Latitude, lon = matrix_full$Longitude)
# Convert the data frame to an sf object
sf_points <- st_as_sf(df_points, coords = c("lon", "lat"), crs = 4326)
skde1 <- st_kde(sf_points, gridsize = c(100, 100))
windows()
dataxx = st_get_contour(skde1, cont = c(seq(1, 99, 5)), disjoint = FALSE)
# Create a function to generate the color palette
color_palette <- colorRampPalette(c("darkolivegreen4","darkolivegreen3","darkseagreen1","yellow","orange","red","darkred"))
# Define the number of colors in the palette
num_colors <- 20 
# Generate the color palette
palette <- color_palette(num_colors)
# Create the elevation map 3D
elevation.texture.map.crop <- crop.image(elevation.texture.map,xleft=146,ybottom=7,xright=203,ytop=256)
elmat <- raster_to_matrix(elevation_switzerland)
attr(elmat, "extent") <- extent(elevation_switzerland)
elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>% 
 add_overlay(generate_polygon_overlay(dataxx, 
                        palette = palette, linewidth=0,
                        extent = extent(elevation_switzerland), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_3d(elmat, zscale = 50, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))
elevation_points <- extract(elevation_switzerland, spatial_points, method='bilinear')
# Render points on the 3D elevation map 
colors <- ifelse(matrix_full$Species == "Argiope bruennichi", "red", 
                ifelse(matrix_full$Species == "Libellula depressa", "green", "blue"))
render_points(
  extent = extent(Switzerland), size = 5,
  lat = matrix_full$Latitude, 
  long = matrix_full$Longitude,
  altitude = elevation_points + 100, 
  zscale = 50, 
  color = colors, 
)
# We have more probability to find the three species in the North part of Switzerland.
# A. bruennichi and S. striolatum seems to overlap more than with L. depressa.

##########################################################################################

##### Plots with regression and lines for the three species #####

windows()
ggplot(matrix_full, aes(x=Elevation,y= Prec_Annual)) +
  geom_point(aes(colour = factor(Species)))+
  geom_smooth(method = "lm",aes(fill = factor(Species))) +
  theme_classic() + 
  theme(legend.position="top") +
  labs(x = "Elevation",    
       y = "Precipitation")
# The three species tend to prefer higher precipitations with increasing elevation.
# L. depressa tends to prefer higher elevations and higher precipitations, while A. bruennichi lower.
# S. striolatum is more similar to A. bruennichi, but seems to prefer lower precipitations.
# There are however some outliers that can influence the regression line.

windows()
ggplot(matrix_full, aes(x=NDVI,y= Temp_Annual)) +
  geom_point(aes(colour = factor(Species)))+
  geom_smooth(method = "lm",aes(fill = factor(Species))) +
  theme_classic() + 
  theme(legend.position="top")+
  labs(x = "NDVI",    
       y = "Temperature")
# The pattern are quite similar for A. bruennichi and S. striolatum, 
# While L. depressa tends to prefer lower temperature in general.
# There is still some outliers for S. striolatum that influence the regression line.