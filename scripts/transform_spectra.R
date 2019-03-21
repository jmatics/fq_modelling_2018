###############################################
# 19.03.2018
# Forage qualitz correlation with spectral data
# Original, Continumm removal, Derivatives
# JW
###############################################


# Loading required packages
rm(list=ls())

library(data.table)
library(dplyr)
library(hsdar)
library(ggplot2)
library(tibble)


### Function to convert normalised reflectance back to a dataframe
source("./funs/spc_2df.R")

# 1. Reading data tables

## Read WIZ data

wiz_df <- read.csv("./output_wiz/hs_all_nrm_df_wiz.csv", header = TRUE)
wiz_df$source <- rep("Witzenhausen", dim(wiz_df)[1])

## Read Rhön data

dsb_df <- read.csv("./output_dsb/hs_all_nrm_df_dsb.csv", header = TRUE)
dsb_df$source <- rep("Rhön", dim(dsb_df)[1])

## merging two data sets

all_df <- rbind(wiz_df, dsb_df)
all_df$doy <- factor(all_df$doy)

# 2. Create speclib class objects

wv <- seq(482, 950, 4)
spec_df <- all_df[c(5:122)] # Select only spectral data
spec_nrm_mat <- as.matrix(spec_df) # Convert df to a matrix
spec_nrm_data <- speclib(spec_nrm_mat, wv)
SI(spec_nrm_data) <- all_df[c(1:4, 123:135)]
spec_nrm_data


# 3. Create derivative spectra

spec_1d_data <- derivative.speclib(spec_nrm_data, m = 1)
spec_2d_data <- derivative.speclib(spec_nrm_data, m = 2)

par(mfrow=c(1,3))
plot(spec_nrm_data, FUN = 1, main = "Original normalised")
plot(spec_1d_data, FUN = 1, main = "First derivation")
plot(spec_2d_data, FUN = 1, main = "Second Derivation")
par(mfrow=c(1,1))

## Convert derivatives back to a dataframe
spec_1d_df<- spc_2df(spec_1d_data)
spec_1d_df <- spec_1d_df[c(1:4, 18:135, 5:17)] # arrange columns
names(spec_1d_df) <- names(all_df)
write.csv(spec_1d_df, "./output/TransformSpectra/hs_all_1d_df.csv", row.names = FALSE)

spec_2d_df<- spc_2df(spec_2d_data)
spec_2d_df <- spec_2d_df[c(1:4, 18:135, 5:17)] # arrange columns
names(spec_2d_df) <- names(all_df)
write.csv(spec_2d_df, "./output/TransformSpectra/hs_all_2d_df.csv", row.names = FALSE)

# 4. Create continuum removal spectra

## segmented hull:

spec_shLine_data <- transformSpeclib(spec_nrm_data, method = "sh", out = "raw")
spec_shRatio_data <- transformSpeclib(spec_nrm_data, method = "sh", out = "ratio")
spec_shBD_data <- transformSpeclib(spec_nrm_data, method = "sh", out = "bd")

par(mfrow=c(2,2))
plot(spec_nrm_data, ispec = 100, main = "Original normalised")
plot(spec_shLine_data, ispec = 100, numeratepoints = FALSE, 
     main = "Segmented hull - Continuum line")
plot(spec_shRatio_data, ispec = 100, main = "Segmented hull - Ratio")
plot(spec_shBD_data, ispec = 100, main = "Segmented hull - Band depth")
par(mfrow=c(1,1))

## Convert continuum removal spectra back to a dataframe
spec_shBD_df<- spc_2df(spec_shBD_data)
spec_shBD_df <- spec_shBD_df[c(1:4, 18:135, 5:17)] # arrange columns
names(spec_shBD_df) <- names(all_df)
write.csv(spec_shBD_df, "./output/TransformSpectra/hs_all_shBD_df.csv", row.names = FALSE)

## Continumm removal for absorbption feature
### 470 - 518 nm --> Abs1
### 550 - 750 nm --> Abs2

abs_features_shBD <- specfeat(spec_shBD_data, c(482, 518, 550, 750))
par(mfrow=c(2,2))
plot(abs_features_shBD, main = "Abs - BD", stylebysubset = "field_id")

# BDR

abs_features_shBDR <- bdri(abs_features_shBD, 1, index = "bdr")
plot(abs_features_shBDR, main = "Abs - BDR", stylebysubset = "field_id")

# NBDI
abs_features_shNDBI <- bdri(abs_features_shBD, 1, index = "ndbi")
plot(abs_features_shNDBI, main = "Abs - NDBI", stylebysubset = "field_id")

# BNa
abs_features_shBNa <- bdri(abs_features_shBD, 1, index = "bna")
plot(abs_features_shBNa, main = "Abs - BNa", stylebysubset = "field_id")
par(mfrow=c(1,1))


## Derivatives of the CR

spec_1d_cr_data <- derivative.speclib(spec_shRatio_data, m = 1)

## Convert continuum removal spectra back to a dataframe
spec_1d_cr_df<- spc_2df(spec_1d_cr_data)
spec_1d_cr_df <- spec_1d_cr_df[c(1:4, 18:135, 5:17)] # arrange columns
names(spec_1d_cr_df) <- names(all_df)
write.csv(spec_1d_cr_df, "./output/TransformSpectra/hs_all_1d_cr_df.csv", row.names = FALSE)