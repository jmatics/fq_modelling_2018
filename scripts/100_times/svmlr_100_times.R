########################################################
# 25.02.2018
# Forage qualitz modelling with SVMLR (100 permutations)
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

svml_seedVec <- sample(c(1:99999), 100, replace=FALSE)

svml_train_list <- vector("list", 100) 
svml_test_list <- vector("list", 100)

svml_fit_n <- vector("list", 100)
svml_op_n <- vector("list", 100)

svml_fit_adf <- vector("list", 100)
svml_op_adf <- vector("list", 100)

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
svml_tunegrid <- expand.grid(C = seq(1, 30, 1))

### Cal/Val process 100 times

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

tictoc::tic("forLoop")
for (k in 1:length(svml_seedVec)) {
  set.seed(svml_seedVec[k])
  
  # Data spliting
  split_sample <-
    groupdata2::partition(
      all_df,
      p = 0.8,
      cat_col = "field_id",
      force_equal = FALSE,
      list_out = F
    ) %>% data.frame()
  
  svml_train_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) 
  
  svml_test_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2)
  
  svml_train_list[[k]]$seed <- rep(svml_seedVec[k], dim(svml_train_list[[k]])[1])
  svml_test_list[[k]]$seed <- rep(svml_seedVec[k], dim(svml_test_list[[k]])[1])
  
  # Train SVMLR model for N %
  svml_fit_n[[k]] <-
    train(
      n ~ .,
      data = svml_train_list[[k]][, c(targetN, estimators)],
      method = "svmLinear",
      metric = metric,
      tuneGrid = svml_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale"),
      importance = T
    )
  
  # Train SVMLR model for ADF %
  svml_fit_adf[[k]] <-
    train(
      adf ~ .,
      data = svml_train_list[[k]][, c(targetADF, estimators)],
      method = "svmLinear",
      metric = metric,
      tuneGrid = svml_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale"),
      importance = T
    )
  
  # Test SVMLR model for N %
  svml_op_n[[k]] <- postResample(svml_test_list[[k]][, targetN],
                                predict(svml_fit_n[[k]], 
                                        newdata =svml_test_list[[k]][, estimators]))
  
  # Test SVMLR model for ADF %
  svml_op_adf[[k]] <- postResample(svml_test_list[[k]][, targetADF],
                                  predict(svml_fit_adf[[k]], 
                                          newdata = svml_test_list[[k]][, estimators]))
  
}
tictoc::toc()


### Prepartion for visualise cal/val outputs

# Compute mean N (%) for each test data subset to calculate rRMSEP

svml_mean_n <- numeric()
for(i in 1:100){
  svml_mean_n[i] <- mean(svml_test_list[[i]][, targetN])
}

# Compute mean ADF (%) for each test data subset to calculate rRMSEP

svml_mean_adf <- numeric()
for (i in 1:100) {
  svml_mean_adf[i] <- mean(svml_test_list[[i]][, targetADF])
}

### Model cal/val output plots

# Plots for best tune parameter (No. of components)
svml_best_tune_df <- data.frame("comp_sigma" = c(unlist(lapply(svml_fit_n,"[[","bestTune"))[seq(1,100,2)], unlist(lapply(svml_fit_adf,"[[","bestTune"))[seq(1,100,2)]),
                               "comp_cost" = c(unlist(lapply(svml_fit_n,"[[","bestTune"))[seq(2,100,2)], unlist(lapply(svml_fit_adf,"[[","bestTune"))[seq(2,100,2)]),
                               "model" = c(rep("N", 100), rep("ADF", 100),
                                           rep("N", 100), rep("ADF", 100)),
                               "tune" = c(rep("Sigma", 200), rep("Cost", 200))) 

sigma_plot <- ggplot(data = svml_best_tune_df, aes(x = model, y = comp_sigma, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Sigma",
       x = "Model variable",
       caption = "Best tune parameter (Sigma) for SVMLR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cost_plot <- ggplot(data = svml_best_tune_df, aes(x = model, y = comp_cost, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Cost",
       x = "Model variable",
       caption = "Best tune parameter (Cost) for SVMLR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(sigma_plot, cost_plot, nrow = 1, ncol = 2)

# rRMSEP plots
svml_best_rmse_df <- data.frame("rmse" = c((unlist(lapply(svml_op_n,"[[","RMSE"))/svml_mean_n)*100,
                                          (unlist(lapply(svml_op_adf,"[[","RMSE"))/svml_mean_adf)*100),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = svml_best_rmse_df, aes(x = model, y = rmse, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "rRMSEP (%)",
       x = "Model variable",
       caption = "relative RMSE (%) for SVMLR models") +
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

svml_best_k_for_n <- which.min(unlist(lapply(svml_op_n,"[[","RMSE")))
svml_best_k_for_adf <- which.min(unlist(lapply(svml_op_adf,"[[","RMSE")))

set.seed(svml_seedVec[svml_best_k_for_n])

svml_best_fit_n <- svml_fit_n[[svml_best_k_for_n]]

print(svml_best_fit_n)
plot(svml_best_fit_n)

plot(varImp(svml_best_fit_n), 10, 
     main = "SVMLR for N (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(svml_best_fit_n, svml_test_list[[svml_best_k_for_n]], 
              estimators, targetN, plot = TRUE, 
              title = "N (%) estimation model with SVMLR")

### Best SVMLR for ADF estimation with best train/test subset

set.seed(svml_seedVec[svml_best_k_for_adf])

svml_best_fit_adf <- svml_fit_adf[[svml_best_k_for_adf]]

print(svml_best_fit_adf)
plot(svml_best_fit_adf)

plot(varImp(svml_best_fit_adf), 10, 
     main = "SVMLR for ADF (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(svml_best_fit_adf, svml_test_list[[svml_best_k_for_adf]], 
              estimators, targetADF, plot = TRUE, 
              title = "ADF (%) estimation model with SVMLR")

save.image(file = "./output/models/100_times/svmlr_100.RData")
