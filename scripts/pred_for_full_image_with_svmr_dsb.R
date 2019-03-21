################################################################################
# 08.03.2018
# Forage quality estimation map generation with SVMR
# JW
################################################################################


# Loading required packages
rm(list=ls())

library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(dplyr)
library(ggplot2)
library(caret)
library(ppls)
library(data.table)

# 1. Read aggregated DOM

agdom <- brick("D:/Shared_Processing/DSB_data_share/ortho_100/ghl_180711_ortho_100.tif")
names(agdom) <- c("PAN", paste("WV_", seq(450, 998, 4), sep=""))
agdom

## Drop unwanted layers
spdom <- dropLayer(agdom, c(1:9, 128:139))
spdom

## Convert to SpatialPixelDataFrame
spdom_px <- as(spdom, 'SpatialPixels')
head(spdom_px)

## Convert to matrix
spdom_org_mat <- as.matrix(as.data.frame(spdom))
head(spdom_org_mat)

## Normalise each pixel
spdom_nrm_mat <- t(apply(spdom_org_mat, 1, normalize.vector))
head(spdom_nrm_mat)

## Convert back to data frame
spdom_nrm_df <- data.frame(spdom_nrm_mat)
head(spdom_nrm_df)

## Drop NAs
spdom_nrm_df_na <- tidyr::drop_na(spdom_nrm_df)
head(spdom_nrm_df_na)

# 2. Predicting for whole image

## Load models
load("./output/models/100_times_fixed_seed/trained_svm_models_with_all.RData")

# Predict N and ADF
pred_n_svm <- predict(svm_n_all, spdom_nrm_df_na)
pred_n_svm_df <- data.frame(cbind(spdom_px@coords, pred_n_svm))
head(pred_n_svm_df)

pred_adf_svm <- predict(svm_adf_all, spdom_nrm_df_na)
pred_adf_svm_df <- data.frame(cbind(spdom_px@coords, pred_adf_svm))
head(pred_adf_svm_df)

## Convert to SpatialGrid
pred_n_svm_spg <- pred_n_svm_df
coordinates(pred_n_svm_spg) <- ~ x + y

pred_adf_svm_spg <- pred_adf_svm_df
coordinates(pred_adf_svm_spg) <- ~ x + y

## coerce to SpatialPixelsDataFrame
gridded(pred_n_svm_spg) <- TRUE

gridded(pred_adf_svm_spg) <- TRUE

## coerce to raster
pred_n_svm_raster <- raster(pred_n_svm_spg)
projection(pred_n_svm_raster) <- projection(agdom)
writeRaster(pred_n_svm_raster, 
            filename = "./output/predictRasters/ghl_20180711_pred_n.tif", 
            overwrite = TRUE)

pred_adf_svm_raster <- raster(pred_adf_svm_spg)
projection(pred_adf_svm_raster) <- projection(agdom)
writeRaster(pred_adf_svm_raster, 
            filename = "./output/predictRasters/ghl_20180711_pred_adf.tif", 
            overwrite = TRUE)

## Plotting
par(mfrow=c(1,2))
plot(pred_n_svm_raster, sub = "GHL - H3", main ="N (%DM)",
     font.main=2, font.lab=4, font.sub=4)


plot(pred_adf_svm_raster, sub = "GHL - H3", main = "ADF (%DM)",
     font.main=2, font.lab=4, font.sub=4)
par(mfrow=c(1,1))
