##############################################################################
##############################################################################
##                            R Script for                                  ##
##                 Spatial(-temporal) Interpolation                         ##
##                        by Avit Kumar Bhowmik                             ##
##############################################################################
##############################################################################

##################################
##  Hands-on: Learning by Doing ##
##################################

# Initiate the required packages
# library(rgdal)
# library(rgeos)
# library(spacetime)
# library(xts)
# library(reshape)
# library(maptools)

# Set your working directory
path <- "/Users/avitbhowmik/Teaching_Supervision/Teaching_Landau/Summer_academy_DAAD/September_2016/SA16/Spatial_interpolation_R/data"
setwd(path)

## Load your data
# Malawi administrative border

Malawi_border <- readShapePoly("Malawi_border")
proj4string(Malawi_border) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # visit http://spatialreference.org/

# Point samples with organic carbon in upper soil
Malawi_upper_OC <- readShapePoints("Malawi_upper_OC")
proj4string(Malawi_upper_OC) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
head(Malawi_upper_OC@coords)
head(Malawi_upper_OC@data)
class(Malawi_upper_OC$TIMESTRR)

# Create spacetime objects containing the OC information
Malawi_upper_OC_st <- Malawi_upper_OC@data
Malawi_upper_OC_st$TIMESTRR
class(Malawi_upper_OC_st$TIMESTRR)

# Extract years
Malawi_upper_OC_st$Years <- as.numeric(format(Malawi_upper_OC_st$TIMESTRR, "%Y"))
head(Malawi_upper_OC_st)

# Overview of space-time organic carbon
head(Malawi_upper_OC_st)
sort(unique(Malawi_upper_OC_st$Years))
nrow(Malawi_upper_OC_st[which(Malawi_upper_OC_st$Years==1987),])
nrow(Malawi_upper_OC_st[which(Malawi_upper_OC_st$Years==1998),])
Malawi_upper_OC_st <- Malawi_upper_OC_st[,c("LONWGS84", "LATWGS84", "Years", "ORCDRC")]
colnames(Malawi_upper_OC_st)[c(1:2,4)] <- c("Longitude", "Latitude", "Soil_Organic_Carbon")
head(Malawi_upper_OC_st)

# Create a wide table and store measured soil organic carbon in individual year column for each sample site
wide_table_Malawi_upper_OC <- as.data.frame(cast(Malawi_upper_OC_st,
Longitude+Latitude~Years,
  fun.aggregate=mean, value="Soil_Organic_Carbon"))
head(wide_table_Malawi_upper_OC)
wide_table_Malawi_upper_OC[,c("1993","1995", "1996", "1997")] <- NaN
head(wide_table_Malawi_upper_OC)
wide_table_Malawi_upper_OC_st <- wide_table_Malawi_upper_OC[,
c("Longitude", "Latitude", as.character(1987:1998))]
head(wide_table_Malawi_upper_OC_st)


# Create spacetimedataframe object of organic carbon for Malawi and visualize it
?STFDF
sp <- SpatialPoints(wide_table_Malawi_upper_OC_st[,1:2],
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
time <- as.xts(ts(data=1:12,start=1987,end=1998,frequency=1))
data <- data.frame(as.vector(as.matrix(wide_table_Malawi_upper_OC_st[,3:14])))
names(data) <- "Soil_Organic_Carbon"
Malawi_upper_OC_stfdf <- STFDF(sp, time, data)
str(Malawi_upper_OC_stfdf)
stplot(Malawi_upper_OC_stfdf, 1987:1998, col.regions=topo.colors(1000),
  sp.layout=list("sp.lines", as(Malawi_border, "SpatialLines")),
       xlab="Longitude", ylab="Latitude", scales=list(draw=T), colorkey=T)

############################
## Spatial Interpolation ##
############################

# Prepare your spatial organic carbon data
wide_table_Malawi_upper_OC$Average_Soil_Organic_Carbon <- rowMeans(wide_table_Malawi_upper_OC[,3:15],
  na.rm=TRUE)
Malawi_avg_upper_OC <- wide_table_Malawi_upper_OC[,c(1,2,16)]
head(Malawi_avg_upper_OC)

# Check if there is any spatial trend (large extent association with coordinates) in organic carbon
summary(lm(Average_Soil_Organic_Carbon~Longitude+Latitude, data=Malawi_avg_upper_OC))

# Turn the data into a spatialpointsdataframe
coordinates(Malawi_avg_upper_OC) <- ~Longitude+Latitude
plot(Malawi_avg_upper_OC)
Malawi_avg_upper_OC <- remove.duplicates(Malawi_avg_upper_OC)
proj4string(Malawi_avg_upper_OC) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
spplot(Malawi_avg_upper_OC, col.regions=topo.colors(1000), 
       sp.layout=list("sp.lines", as(Malawi_border, "SpatialLines")), 
       colorkey=T, xlab="Longitudes", ylab="Latitudes", scales=list(draw=T))

# Prepare your prediction grid
xgrid <- seq(from = bbox(Malawi_border)[1,1], to = bbox(Malawi_border)[1,2], by = 0.1)
ygrid <- seq(from = bbox(Malawi_border)[2,1], to = bbox(Malawi_border)[2,2], by = 0.1)
Malawi_grid <- expand.grid(Longitudes = xgrid, Latitudes = ygrid)
gridded(Malawi_grid) <- ~Longitudes+Latitudes
proj4string(Malawi_grid) <- proj4string(Malawi_avg_upper_OC)
plot(Malawi_grid)
plot(Malawi_border, add=TRUE)

# Interpolate using Inverse Distance Weighting method
library(gstat)
library(hydroGOF)
IDW_Average_Soil_Organic_Carbon <- krige(Average_Soil_Organic_Carbon~1, 
                                         Malawi_avg_upper_OC, newdata=Malawi_grid)
spplot(IDW_Average_Soil_Organic_Carbon["var1.pred"], 
       col.regions=topo.colors(1000), 
       sp.layout=list("sp.lines", as(Malawi_border, "SpatialLines")), 
       colorkey=T, xlab="Longitudes", ylab="Latitudes", scales=list(draw=T))

# Goodness of interpolation: Cross-validation
Cross_val_IDW <- krige.cv(Average_Soil_Organic_Carbon~1, Malawi_avg_upper_OC)
head(Cross_val_IDW@data)

#Root mean squarred error (RMSE)
rmse(Cross_val_IDW$var1.pred, Cross_val_IDW$observed)
d(Cross_val_IDW$var1.pred, Cross_val_IDW$observed)

# Kriging Interpolation
# Compute Variogram
Variogram_Average_Soil_Organic_Carbon <- variogram(Average_Soil_Organic_Carbon~1,
  Malawi_avg_upper_OC)
plot(Variogram_Average_Soil_Organic_Carbon)
Fitted_spherical_variogram_Average_Soil_Organic_Carbon <- fit.variogram(object=Variogram_Average_Soil_Organic_Carbon,
                model=vgm(psill=75, model="Sph", range=200, nugget=40), fit.sills=TRUE, 
                                                              fit.ranges=TRUE, fit.method=6)
plot(Variogram_Average_Soil_Organic_Carbon, Fitted_spherical_variogram_Average_Soil_Organic_Carbon)
Fitted_exponential_variogram_Average_Soil_Organic_Carbon <- fit.variogram(object=Variogram_Average_Soil_Organic_Carbon,
              model=vgm(psill=75, model="Exp", range=200, nugget=40), fit.sills=TRUE, 
              fit.ranges=TRUE, fit.method=6)
plot(Variogram_Average_Soil_Organic_Carbon, Fitted_exponential_variogram_Average_Soil_Organic_Carbon)

# Evaluate two fitted models by their goodness of fit (Sum of Squared Error)
attr(Fitted_spherical_variogram_Average_Soil_Organic_Carbon, "SSErr")
attr(Fitted_exponential_variogram_Average_Soil_Organic_Carbon, "SSErr")

# And the spherical model shows better fit as the sum of squared error is lower. So, we will fit the spherical model to kriging.

# Ordinary Kriging Interpolation
Kriged_Average_Soil_Organic_Carbon <- krige(Average_Soil_Organic_Carbon~1, 
                                            Malawi_avg_upper_OC, newdata=Malawi_grid, 
                         model=Fitted_spherical_variogram_Average_Soil_Organic_Carbon)

spplot(Kriged_Average_Soil_Organic_Carbon["var1.pred"], 
       col.regions=topo.colors(1000), sp.layout=list("sp.lines", as(Malawi_border, "SpatialLines")), 
       colorkey=T, xlab="Longitude", ylab="Latitude", scales=list(draw=T))

# Goodness of interpolation: Cross-validation
Cross_val_Krig <- krige.cv(Average_Soil_Organic_Carbon~1, Malawi_avg_upper_OC, 
                          model=Fitted_spherical_variogram_Average_Soil_Organic_Carbon)
head(Cross_val_Krig@data)

#Root mean squarred error (RMSE)
rmse(Cross_val_Krig$var1.pred, Cross_val_Krig$observed)
d(Cross_val_Krig$var1.pred, Cross_val_Krig$observed)

# Clip interpolated surface
over_Malawi_Grid <- over(Kriged_Average_Soil_Organic_Carbon, Malawi_border)
inside_Malawi_Grid <- !is.na(over_Malawi_Grid[,1])
Clipped_MalawiGrid  <- Kriged_Average_Soil_Organic_Carbon[inside_Malawi_Grid,]
spplot(Clipped_MalawiGrid["var1.pred"], 
       col.regions=topo.colors(1000), sp.layout=list("sp.lines", as(Malawi_border, "SpatialLines")), 
       colorkey=T, xlab="Longitudes", ylab="Latitudes", scales=list(draw=T))


#####################################
## Some very basic GIS operations ##
####################################

# Preferred packages for vector GIS operations
library(maptools)
library(sp)
library(rgdal)

# Please the see the documentation of the packages rgdal, maptools and rgeos for functions for basic GIS
# operations, e.g. overlay, buffer, offset etc.

# Packages for raster GIS operation
library(raster)

# We can handle the kriged continuous surface of organic carbon in the raster package
Average_Soil_Organic_Carbon_raster <- raster(Kriged_Average_Soil_Organic_Carbon)
plot(Average_Soil_Organic_Carbon_raster)
plot(Malawi_border, add=TRUE)

# And we can save it as a tif file for further uses
writeRaster(Average_Soil_Organic_Carbon_raster, "Average_Soil_Organic_Carbon_raster", format="GTiff", overwrite=T)

# We can mask the raster to the boundary of Malawi
Average_Soil_Organic_Carbon_raster_Malawi <- mask(Average_Soil_Organic_Carbon_raster, Malawi_border)
plot(Average_Soil_Organic_Carbon_raster_Malawi)
plot(Malawi_border, add=TRUE)

                        ##########################################
                        ## That's all at this stage. Thank you ##
                        #########################################