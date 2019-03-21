library(sp)
library(rgdal)
library(raster)
library(gstat)
library(tmap)
library(dplyr)

g4_points <- readOGR(dsn = "D:/Data_Collection/2018/GIS_data/G4", layer = "g4_random_points")
g4_points@data$fp_id <- seq(401, 420, 1)

## Nitrogen data

cn_data <- read.csv("./input_wiz/cn/grassland_cn.csv", header = TRUE)
cn_data$doy <- factor(cn_data$doy)

cn_data_g4_h1 <- cn_data %>% dplyr::select(field_id, fp_id, harvest, n) %>%
  dplyr::filter(harvest == "H1" & field_id == "g4")

# merge to gis file
g4_n <- merge(g4_points, cn_data_g4_h1)
projection(g4_n) <- projection(agdom)

# Read aggregated digital ortho mosaic
agdom <- brick("D:/Data_Processing/2018/G4/CUBERT/g4_20180510_cubert/DOM_DEM/g4_20180510_cubert_DOM_100cm.tif")
names(agdom) <- c("PAN", paste("WV_", seq(450, 998, 4), sep=""))
agdom

g <- as(agdom[[88]], 'SpatialGridDataFrame')
p <- as(agdom[[88]], 'SpatialPixels')

colnames(g4_n@coords) <- colnames(p@coords)

plot(new_grid, 
     breaks = seq(10, 2000, 50), 
     col = terrain.colors(20),
     main="Crop", 
     axes=TRUE)

gs = gstat(formula = n~1, locations = g4_n)
r_idw = interpolate(pred_n_svm_raster, gs, idp=2.0)
plot(r_idw)
plot(new_grid, 
     breaks = seq(10, 2000, 50), 
     col = terrain.colors(20),
     main="Crop", 
     axes=TRUE, add = TRUE)

n_pred <- stack(new_grid, r_idw)
plot(values(krig_r), values(pred_n_svm_raster))
cor(values(krig_r), values(pred_n_svm_raster), use = "complete.obs")
persp(r_idw)

# Define the 1st order polynomial equation
f.1 <- as.formula(n ~ x + y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, g4_n, cloud = FALSE, cutoff=50, width=25)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=20, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1))



# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
f.1 <- as.formula(n ~ x + y) 
dat.krg <- krige(f.1, g4_n, p, dat.fit)
krig_r <- raster(dat.krg)
plot(krig_r)
persp(krig_r)
