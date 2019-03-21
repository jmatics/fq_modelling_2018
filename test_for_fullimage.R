library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(caret)
library(ppls)
library(data.table)


# Read aggregated digital ortho mosaic
agdom <- brick("D:/Data_Processing/2018/G4/CUBERT/g4_20180606_cubert/DOM_DEM/g4_20180606_cubert_DOM_100cm.tif")
names(agdom) <- c("PAN", paste("WV_", seq(450, 998, 4), sep=""))
agdom

# Drop unwanted layers
spdom <- dropLayer(agdom, c(1:9, 128:139))
spdom

# Convert to SpatialPixelDataFrame
spdom_px <- as(spdom, 'SpatialPixels')

# Convert to matrix
spdom_org_mat <- as.matrix(as.data.frame(spdom))

# Normalise each pixel
spdom_nrm_mat <- t(apply(spdom_org_mat, 1, normalize.vector))

# Convert back to data frame
spdom_nrm_df <- data.frame(spdom_nrm_mat)

# Drop NAs
spdom_nrm_df_na <- tidyr::drop_na(spdom_nrm_df)

# Load models
load("./output/models/pls_rfr_gpr_svmr_for_n_adf.RData")

# Predict N and ADF
pred_n_svm <- predict(svm_n, spdom_nrm_df_na)
pred_n_svm_df <- data.frame(cbind(spdom_px@coords, pred_n_svm))


pred_adf_svm <- predict(svm_adf, spdom_nrm_df_na)
pred_adf_svm_df <- data.frame(cbind(spdom_px@coords, pred_adf_svm))

# Convert to SpatialGrid
pred_n_svm_spg <- pred_n_svm_df
coordinates(pred_n_svm_spg) <- ~ x + y

pred_adf_svm_spg <- pred_adf_svm_df
coordinates(pred_adf_svm_spg) <- ~ x + y

# coerce to SpatialPixelsDataFrame
gridded(pred_n_svm_spg) <- TRUE

gridded(pred_adf_svm_spg) <- TRUE


# coerce to raster
pred_n_svm_raster <- raster(pred_n_svm_spg)
projection(pred_n_svm_raster) <- projection(agdom)
writeRaster(pred_n_svm_raster, filename = "./output/predictRasters/g4_20180606_pred_n.tif", 
            overwrite = TRUE)

pred_adf_svm_raster <- raster(pred_adf_svm_spg)
projection(pred_adf_svm_raster) <- projection(agdom)
writeRaster(pred_adf_svm_raster, filename = "./output/predictRasters/g4_20180606_pred_adf.tif", 
            overwrite = TRUE)


# Read vector files for mapping
g4_poly <-  readOGR(dsn = "D:/Data_Collection/2018/GIS_data/G4", 
                    layer = "g4_poly_epsg3044")

g4_poly_buff <-  readOGR(dsn = "D:/Data_Collection/2018/GIS_data/G4", 
                    layer = "g4_poly_buffer_epsg3044")

g4_points <-  readOGR(dsn = "D:/Data_Collection/2018/GIS_data/G4", 
                      layer = "g4_random_points")

g4_sub <-  readOGR(dsn = "D:/Data_Collection/2018/GIS_data/G4", 
                      layer = "g4_random_subplots")


# Plotting
par(mfrow=c(1,2))
plot(pred_n_svm_raster, sub = "G4 - H2", main ="N (%DM)",
     font.main=2, font.lab=4, font.sub=4)
plot(g4_poly, add = TRUE)
plot(g4_points, add = TRUE)

plot(pred_adf_svm_raster, sub = "G4 - H2", main = "ADF (%DM)",
     font.main=2, font.lab=4, font.sub=4)
plot(g4_poly, add = TRUE)
plot(g4_points, add = TRUE)
par(mfrow=c(1,1))


# Compare preidct raster values and actuals

## Nitrogen data

cn_data <- read.csv("./input_wiz/cn/grassland_cn.csv", header = TRUE)
cn_data$doy <- factor(cn_data$doy)

g4_n_data <- cn_data %>% dplyr::select(field_id, fp_id, harvest, n) %>%
  dplyr::filter(harvest == "H2" & field_id == "g4")

## ADF data
adf_data <- read.csv("./input_wiz/adf/grassland_adf2.csv", header = TRUE)
adf_data$doy <- factor(adf_data$doy)

g4_adf_data <- adf_data %>% dplyr::select(field_id, fp_id, harvest, adf) %>%
  dplyr::filter(harvest == "H2" & field_id == "g4")

# Extract predict values from raster and combined to data frame

raster_op_n <- data.frame("Actual" = g4_n_data$n,
                        "Predicted" = extract(pred_n_svm_raster, g4_sub, fun = mean)) 
raster_op_n$Q <- rep("N", 20)

par(mfrow=c(1,2))
boxplot(raster_op_n$Actual, main = "Actual N")
boxplot(raster_op_n$PredictR, main = "Predict using image")
par(mfrow=c(1,1))

boxplot_n <- ggplot(data = reshape2::melt(raster_op_n), aes(x = variable, y = value)) +
  geom_boxplot(fill = "gray") +
  theme_light(base_size = 12, base_family = "Helvetica")+
  labs(x = "Data type",
       y = "N (%DM) per dry matter",
       title = "Predicted vs Actual") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom")

summary(lm(Actual ~ Predicted, data = raster_op_n))
cor.test(raster_op_n$Actual, raster_op_n$Predicted)
cor(raster_op_n %>% 
           dplyr::filter(Actual < 3.7) %>% 
           dplyr::select(Actual) %>% 
           as.vector(), 
         raster_op_n %>% 
           dplyr::filter(Actual < 3.7) %>% 
           dplyr::select(Predicted) %>% 
           as.vector())

postResample(raster_op_n$Predicted, raster_op_n$Actual)
(postResample(raster_op_n$Predicted, raster_op_n$Actual)[1]/mean(raster_op_n$Actual))*100
postResample(raster_op_n %>% dplyr::filter(Actual < 3.7) %>% dplyr::select(Actual),
             raster_op_n %>% dplyr::filter(Actual < 3.7) %>% dplyr::select(Predicted))
(postResample(raster_op_n %>% dplyr::filter(Actual < 3.7) %>% dplyr::select(Actual),
              raster_op_n %>% dplyr::filter(Actual < 3.7) %>% dplyr::select(Predicted))[1]/mean(raster_op_n$Actual))*100

ggplot(data = raster_op_n, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  coord_fixed()


raster_op_adf <- data.frame("Actual" = g4_adf_data$adf,
                          "Predicted" = extract(pred_adf_svm_raster, g4_sub, fun = mean))
raster_op_adf$Q <- rep("ADF", 20)
par(mfrow=c(1,2))
boxplot(raster_op_adf$Actual, main = "Actual ADF")
boxplot(raster_op_adf$PredictR, main = "Predict using image")
par(mfrow=c(1,1))

boxplot_adf <- ggplot(data = reshape2::melt(raster_op_adf), aes(x = variable, y = value)) +
  geom_boxplot(fill = "gray") +
  theme_light(base_size = 12, base_family = "Helvetica")+
  labs(x = "Data type",
       y = "ADF (%DM) per dry matter",
       title = "Predicted vs Actual") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom")



summary(lm(Actual ~ Predicted, data = raster_op_adf))
cor.test(raster_op_adf$Actual, raster_op_adf$Predicted)
cor(raster_op_adf %>% 
      dplyr::filter(Actual < 30) %>% 
      dplyr::select(Actual) %>% 
      as.vector(), 
    raster_op_adf %>% 
      dplyr::filter(Actual < 30) %>% 
      dplyr::select(Predicted) %>% 
      as.vector())

postResample(raster_op_adf$Predicted, raster_op_adf$Actual)
postResample(raster_op_adf %>% dplyr::filter(Actual < 30) %>% dplyr::select(Actual),
             raster_op_adf %>% dplyr::filter(Actual < 30) %>% dplyr::select(Predicted))
(postResample(raster_op_adf %>% dplyr::filter(Actual < 30) %>% dplyr::select(Actual),
             raster_op_adf %>% dplyr::filter(Actual < 30) %>% dplyr::select(Predicted))[1]/mean(raster_op_adf$Actual))*100
(postResample(raster_op_adf$Predicted, raster_op_adf$Actual)[1]/mean(raster_op_adf$Actual))*100

ggplot(data = raster_op_adf, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  coord_fixed()


raster_op <- rbind(raster_op_n, raster_op_adf)

png(filename="./plots/boxplot_adf_n_act_vs_pred.png", type="cairo",
    width = 3300, height = 3300, res = 600)
ggplot(data = reshape2::melt(raster_op), aes(x = variable, y = value, fill = Q)) +
  geom_boxplot() +
  facet_grid(rows = vars(Q), scales="free") +
  theme_bw(base_size = 12, base_family = "Helvetica")+
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                     name = "Forage quality parameter", 
                     labels = c("ADF (%DM)", "Nitrogen (%DM)"))+
  labs(x = "Data type",
       y = "Quality (%DM)",
       title = "Predicted vs Actual") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom")
dev.off()

A <- ggplot(data = raster_op %>% dplyr::filter(Q == "N", Actual < 3.7), 
            aes(x = Predicted, y = Actual)) +
  geom_point(col = "#E48F1B", alpha = 0.8, size = 5, shape = 20) +
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  coord_fixed(ratio = 1) +
  labs(x = "Predicted N (%DM)",
       y = "Actual N (%DM)",
       title = "N (%DM) - Predicted vs Actual") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom")

B <- ggplot(data = raster_op %>% dplyr::filter(Q == "ADF", Actual < 30), 
            aes(x = Predicted, y = Actual)) +
  geom_point(col = "#42858C", alpha = 0.8, size = 5, shape = 20) +
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  coord_fixed(ratio = 1) +
  labs(x = "Predicted ADF (%DM)",
       y = "Actual ADF (%DM)",
       title = "ADF (%DM) - Predicted vs Actual") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom")

png(filename="./plots/scatter_adf_n_act_vs_pred.png", type="cairo",
    width = 6600, height = 6600, res = 600)
cowplot::plot_grid(A, B, ncol = 2, nrow = 1)
dev.off()

#############################################################################

# Check with interpolated raster

# create spatial pixel df
n_sp <- as(pred_n_svm_raster, 'SpatialPixels')

# merge to gis file
g4_points@data$fp_id <- seq(401, 420, 1)
projection(g4_n) <- projection(pred_n_svm_raster)
g4_n <- merge(g4_points, g4_n_data)
colnames(g4_n@coords) <- colnames(n_sp@coords)


# Krigging

# Variogram

# Define the 1st order polynomial equation
f1 <- as.formula(n ~ x + y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f1, g4_n, cloud = FALSE, cutoff=50, width=5)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(model="Sph"))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit)

# Krigging Interpolation

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f1, g4_n, n_sp, dat.fit)
krig_r <- raster(dat.krg)
plot(krig_r)
persp(krig_r)

# Compare interpolated raster and predicted raster
cor(values(krig_r), values(pred_n_svm_raster), use = "complete.obs")

# Compare interpolated and predict values

inter_op <- data.frame("Actual" = g4_n_data$n, 
                       "KrigR" = extract(krig_r, g4_sub, fun = mean), 
                       "PredictR" = extract(pred_n_svm_raster, g4_sub, fun = mean))
par(mfrow=c(1,3))
boxplot(inter_op$Actual, main = "Actual")
boxplot(inter_op$KrigR, main = "KrigR")
boxplot(inter_op$PredictR, main = "PredictR")
par(mfrow=c(1,1))

ggplot(data = reshape2::melt(inter_op), aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_bw()

summary(lm(KrigR ~ PredictR, data = inter_op))
cor.test(inter_op$KrigR, inter_op$PredictR)

summary(lm(KrigR ~ Actual, data = inter_op))
cor.test(inter_op$KrigR, inter_op$Actual)

ggplot(data = inter_op, aes(x = Actual, y = PredictR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  coord_fixed()

ggplot(data = inter_op, aes(x = Actual, y = KrigR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  coord_fixed()
