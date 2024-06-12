##### statistical test #####
library(stats)
library(ggplot2)
library(vegan)
library(plotly)
library(ggfortify)
library(corrplot)

# Correlation matrix
windows()
cor_matrix <- cor(df_con_selected)
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)
# We see that the temperature is negatively correlated with wind and elevation, 
# and the latters are positively correlated between them.
# We can then proceed and remove two variables from the analysis and keep only one, the elevation.


##### Elevation #####
windows()
ggplot(data = matrix_full, mapping = aes(y = Elevation , fill = Species)) +
  geom_boxplot()+
  labs(title = "Elevation",
       y = "Elevation",
       fill = "Species")
# We see that L. depressa prefers higher elevations, and S. striolatum lower.

var.test(matrix_full$Elevation[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Elevation[matrix_full$Species=="Libellula depressa"],)
#p-value < 2.2e-16 < 0.05
t.test(matrix_full$Elevation[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Elevation[matrix_full$Species=="Libellula depressa"],
         var.equal = F) 
# p value: 5.237e-06 < 0.05
# Significative difference between the two species. 

var.test(matrix_full$Elevation[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Elevation[matrix_full$Species=="Sympetrum striolatum"],
         )
# p-value = 0.5959 > 0.05
t.test(matrix_full$Elevation[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Elevation[matrix_full$Species=="Sympetrum striolatum"],
         var.equal = F) 
# p-value = 5.052e-10 < 0.05
# Very significative difference in mean, not in variance.

# This could mean that L. depressa varies a lot in elevation, while S. striolatum varies less, and
# it is more close to A. bruennichi than L. depressa.


##### Precipitation #####
windows()
ggplot(data = matrix_full, mapping = aes(y = Prec_Annual , fill = Species)) +
  geom_boxplot()+
  labs(title = "Annual Precipitation",
       y = "Annual Precipitation",
       fill = "Species")
# A. bruennichi and S. striolatum have the same preferences in terms of precipitation,
# even if for S. striolatum the mediane is a bit lower.
# L. depressa have a preference for higher precipitation.
# Overall, there is no clear difference between the three species.

var.test(matrix_full$Prec_Annual[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Prec_Annual[matrix_full$Species=="Libellula depressa"])
# p-value = 0.01088 < 0.05
t.test(matrix_full$Prec_Annual[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$Prec_Annual[matrix_full$Species=="Libellula depressa"],
       var.equal = F)
# p-value = 0.02183 < 0.05
# Again, not so significative difference.

var.test(matrix_full$Prec_Annual[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Prec_Annual[matrix_full$Species=="Sympetrum striolatum"])
# p-value = 0.0102 < 0.05
t.test(matrix_full$Prec_Annual[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$Prec_Annual[matrix_full$Species=="Sympetrum striolatum"],
       var.equal = F)
# p-value = 0.02314 < 0.05
# Not so significative difference.

# This could mean that the three species do not differ much in precipitation preferences. 


##### incident solar radiation #####
windows()
ggplot(data = matrix_full, mapping = aes(y = Srad_Annual , fill = Species)) +
  geom_boxplot()+
  labs(title = "incident solar radiation",
       y = "incident solar radiation",
       fill = "Species")
# L. depressa prefers higher values of incident solar radiation, 
# while there is no clear difference between the other two species. 

var.test(matrix_full$Srad_Annual[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Srad_Annual[matrix_full$Species=="Libellula depressa"])
# p-value = 4.148e-06 < 0.05
t.test(matrix_full$Srad_Annual[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$Srad_Annual[matrix_full$Species=="Libellula depressa"],
       var.equal = F)
# p-value = 1.404e-05 < 0.05
# Significative difference.

var.test(matrix_full$Srad_Annual[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$Srad_Annual[matrix_full$Species=="Sympetrum striolatum"])
# p-value = 0.856 > 0.05
t.test(matrix_full$Srad_Annual[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$Srad_Annual[matrix_full$Species=="Sympetrum striolatum"],
       var.equal = F)
# p-value = 6.895e-05 < 0.05
# Significative difference in mean, not in variance.

# Again, L. depressa varies more in incident solar radiation than the other two species.


##### NDVI #####
windows()
ggplot(data = matrix_full, mapping = aes(y = NDVI , fill = Species)) +
  geom_boxplot() +
  labs(title = "NDVI",
       y = "NDVI",
       fill = "Species")
# There is no clear difference between the three species.

var.test(matrix_full$NDVI[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$NDVI[matrix_full$Species=="Libellula depressa"])
# p-value = 0.02921 < 0.05
t.test(matrix_full$NDVI[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$NDVI[matrix_full$Species=="Libellula depressa"],
       var.equal = F)
# p-value = 0.0009038 < 0.05
# Again, not so significative difference.


var.test(matrix_full$NDVI[matrix_full$Species=="Argiope bruennichi"],
         matrix_full$NDVI[matrix_full$Species=="Sympetrum striolatum"])
# p-value = 0.00693 < 0.05
t.test(matrix_full$NDVI[matrix_full$Species=="Argiope bruennichi"],
       matrix_full$NDVI[matrix_full$Species=="Sympetrum striolatum"],
       var.equal = F)
# p-value = 0.03569 < 0.05
# Not so significative difference.

# The three species does not differ much in NDVI preferences. 


##############################################################################################

##### PCA #####
colors <- ifelse(df_scaled$Species == "Argiope bruennichi", "darkred", 
                ifelse(df_scaled$Species == "Libellula depressa", "darkgreen", "blue"))
df_con_selected <- df_scaled[,colnames(df_scaled) %in% c("Prec_Annual", "Srad_Annual", "Elevation","NDVI")]
pca_res <- stats::prcomp(df_con_selected)

windows()
ggplotly(autoplot(pca_res, data = df_discr, colour = colors,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour = 'black'))


# PCA
pca <- princomp(df_con_selected, scores=T, cor=T)
# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]
# Loadings
loads <- pca$loadings
# Loadings names
load_names <- rownames(loads)
# Scale factor for loadings
scale.loads <- 5
# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discr$Species)
for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}
print(p)


# We can see from the three PCA that the three species are all together. Just L. depressa seems to have a wider range.
# In fact, the three species varies a lot along the gradients, especially L. depressa. 
# The percipitation has a strong influence, but does not varies much among the three species, as we saw from previous analysis.
# L. depressa follow more the Incident solar radiation and the Elevation, compared to the other two species.
# The NDVI value has not a big influence, as we saw before as well. 

# Following the analysis, it's clear that the three species share in part the same niches.
# However, L. depressa varies more in elevation and incident solar radiation compared to the other two species.
# This could mean that A. bruennichi and S. striolatum are probably closer in niches than L. depressa.
# NDVI values and precipitation are not the best factors to discriminate the three species.

# In conclusion, it's possible that the spider A. bruennichi has as a part of its diet the two species of Odonata.
# We could say probably more S. striolatum, that seems closer in terms of niches and climate variables. 





