########################################################
# 21.02.2018
# Forage qualitz modelling with RFR (100 permutations)
# JW
########################################################


# Loading required packages
rm(list=ls())

library(data.table)
library(dplyr)
library(hsdar)
library(ggplot2)
library(caret)
library(purrr)
library(readr)
library(ppls)
library(tibble)
library(MASS)
library(cowplot)

################################################

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


## Visualise data

boxplot_n <- ggplot(all_df, aes (x = doy, y = n, fill = field_id)) + 
  geom_boxplot(alpha = 0.75) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Grassland ID", 
                              labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL")) +
  labs(x = "Julian date (DOY)",
       y = "N (%) per dry matter",
       caption = "Nitrogen (N) concentration data (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

boxplot_adf <- ggplot(all_df, aes (x = doy, y = adf, fill = field_id)) + 
  geom_boxplot(alpha = 0.9) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Grassland ID", 
                              labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL")) +
  labs(x = "Julian date (DOY)",
       y = "ADF (%) per dry matter",
       caption = "Acid detergent fiber (ADF) concentration data (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

plot_grid(boxplot_n, boxplot_adf, nrow = 2, ncol = 1)


# Model calibration / validation

### Define list variables

rf_seedVec <- sample(c(1:99999), 100, replace=FALSE)

rf_train_list <- vector("list", 100) 
rf_test_list <- vector("list", 100)

rf_fit_n <- vector("list", 100)
rf_op_n <- vector("list", 100)

rf_fit_adf <- vector("list", 100)
rf_op_adf <- vector("list", 100)

### Define necessray variables for cal/val

## Coulmn index for estimators (independent variables)
estimators <- c(5:122)

## Coulmn index for each targets (dependent variables)
targetN <- 131
targetADF <- 134

## Define control parameters
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  returnResamp = "all",
  allowParallel = TRUE
)

metric <- "RMSE"
rf_tunegrid <- expand.grid(.mtry=c(5:15))

### Cal/Val process 100 times

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

tictoc::tic("forLoop")
for (k in 1:length(rf_seedVec)) {
  set.seed(rf_seedVec[k])
  
  # Data spliting
  split_sample <-
    groupdata2::partition(
      all_df,
      p = 0.8,
      cat_col = "field_id",
      force_equal = FALSE,
      list_out = F
    ) %>% data.frame()
  
  rf_train_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) 
  
  rf_test_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2)
  
  rf_train_list[[k]]$seed <- rep(rf_seedVec[k], dim(rf_train_list[[k]])[1])
  rf_test_list[[k]]$seed <- rep(rf_seedVec[k], dim(rf_test_list[[k]])[1])
  
  # Train RFR model for N %
  rf_fit_n[[k]] <-
    train(
      n ~ .,
      data = rf_train_list[[k]][, c(targetN, estimators)],
      method = "rf",
      metric = metric,
      tuneGrid = rf_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale"),
      importance = T
    )
  
  # Train RFR model for ADF %
  rf_fit_adf[[k]] <-
    train(
      adf ~ .,
      data = rf_train_list[[k]][, c(targetADF, estimators)],
      method = "rf",
      metric = metric,
      tuneGrid = rf_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale"),
      importance = T
    )
  
  # Test RFR model for N %
  rf_op_n[[k]] <- postResample(rf_test_list[[k]][, targetN],
                                predict(rf_fit_n[[k]], 
                                        newdata =rf_test_list[[k]][, estimators]))
  
  # Test RFR model for ADF %
  rf_op_adf[[k]] <- postResample(rf_test_list[[k]][, targetADF],
                                  predict(rf_fit_adf[[k]], 
                                          newdata = rf_test_list[[k]][, estimators]))
  
}
tictoc::toc()


### Prepartion for visualise cal/val outputs

# Compute mean N (%) for each test data subset to calculate rRMSEP

rf_mean_n <- numeric()
for(i in 1:100){
  rf_mean_n[i] <- mean(rf_test_list[[i]][, targetN])
}

# Compute mean ADF (%) for each test data subset to calculate rRMSEP

rf_mean_adf <- numeric()
for (i in 1:100) {
  rf_mean_adf[i] <- mean(rf_test_list[[i]][, targetADF])
}

### Model cal/val output plots

# Plots for best tune parameter (No. of components)
rf_best_tune_df <- data.frame("comp" = c(unlist(lapply(rf_fit_n,"[[","bestTune")),
                                          unlist(lapply(rf_fit_adf,"[[","bestTune"))),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = rf_best_tune_df, aes(x = model, y = comp, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Mtry",
       x = "Model variable",
       caption = "Best tune parameter (Mtry) for RFR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

# rRMSEP plots
rf_best_rmse_df <- data.frame("rmse" = c((unlist(lapply(rf_op_n,"[[","RMSE"))/rf_mean_n)*100,
                                          (unlist(lapply(rf_op_adf,"[[","RMSE"))/rf_mean_adf)*100),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = rf_best_rmse_df, aes(x = model, y = rmse, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "rRMSEP (%)",
       x = "Model variable",
       caption = "relative RMSE (%) for RFR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

# Best models

### Identify best train/test subset

rf_best_k_for_n <- which.min(unlist(lapply(rf_op_n,"[[","RMSE")))
rf_best_k_for_adf <- which.min(unlist(lapply(rf_op_adf,"[[","RMSE")))

set.seed(rf_seedVec[rf_best_k_for_n])

rf_best_fit_n <- rf_fit_n[[rf_best_k_for_n]]

print(rf_best_fit_n)
plot(rf_best_fit_n)

plot(varImp(rf_best_fit_n), 10, 
     main = "RFR for N (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(rf_best_fit_n, rf_test_list[[rf_best_k_for_n]], 
              estimators, targetN, plot = TRUE, 
              title = "N (%) estimation model with RFR")

### Best RFR for ADF estimation with best train/test subset

set.seed(rf_seedVec[rf_best_k_for_adf])

rf_best_fit_adf <- rf_fit_adf[[rf_best_k_for_adf]]

print(rf_best_fit_adf)
plot(rf_best_fit_adf)

plot(varImp(rf_best_fit_adf), 10, 
     main = "RFR for ADF (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(rf_best_fit_adf, rf_test_list[[rf_best_k_for_adf]], 
              estimators, targetADF, plot = TRUE, 
              title = "ADF (%) estimation model with RFR")

save.image(file = "./output/models/100_times/rfr_100.RData")
