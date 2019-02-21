########################################################
# 21.02.2018
# Forage qualitz modelling with PLSR (100 permutations)
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

pls_seedVec <- sample(c(1:99999), 100, replace=FALSE)

pls_train_list <- vector("list", 100) 
pls_test_list <- vector("list", 100)

pls_fit_n <- vector("list", 100)
pls_op_n <- vector("list", 100)

pls_fit_adf <- vector("list", 100)
pls_op_adf <- vector("list", 100)

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
pls_tunegrid <- expand.grid(.ncomp = c(1:20))

### Cal/Val process 100 times

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

tictoc::tic("forLoop")
for (k in 1:length(pls_seedVec)) {
  set.seed(pls_seedVec[k])
  
  # Data spliting
  split_sample <-
    groupdata2::partition(
      all_df,
      p = 0.8,
      cat_col = "field_id",
      force_equal = FALSE,
      list_out = F
    ) %>% data.frame()
  
  pls_train_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) 
  
  pls_test_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2)
  
  pls_train_list[[k]]$seed <- rep(pls_seedVec[k], dim(pls_train_list[[k]])[1])
  pls_test_list[[k]]$seed <- rep(pls_seedVec[k], dim(pls_test_list[[k]])[1])
  
  # Train PLSR model for N %
  pls_fit_n[[k]] <-
    train(
      n ~ .,
      data = pls_train_list[[k]][, c(targetN, estimators)],
      method = "pls",
      metric = metric,
      tuneGrid = pls_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale")
    )
  
  # Train PLSR model for ADF %
  pls_fit_adf[[k]] <-
    train(
      adf ~ .,
      data = pls_train_list[[k]][, c(targetADF, estimators)],
      method = "pls",
      metric = metric,
      tuneGrid = pls_tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale")
    )
  
  # Test PLSR model for N %
  pls_op_n[[k]] <- postResample(pls_test_list[[k]][, targetN],
                                predict(pls_fit_n[[k]], 
                                        newdata =pls_test_list[[k]][, estimators]))
  
  # Test PLSR model for ADF %
  pls_op_adf[[k]] <- postResample(pls_test_list[[k]][, targetADF],
                                  predict(pls_fit_adf[[k]], 
                                          newdata = pls_test_list[[k]][, estimators]))
  
}
tictoc::toc()


### Prepartion for visualise cal/val outputs

# Compute mean N (%) for each test data subset to calculate rRMSEP

pls_mean_n <- numeric()
for(i in 1:100){
  pls_mean_n[i] <- mean(pls_test_list[[i]][, targetN])
}

# Compute mean ADF (%) for each test data subset to calculate rRMSEP

pls_mean_adf <- numeric()
for (i in 1:100) {
  pls_mean_adf[i] <- mean(pls_test_list[[i]][, targetADF])
}

### Model cal/val output plots

# Plots for best tune parameter (No. of components)
pls_best_tune_df <- data.frame("comp" = c(unlist(lapply(pls_fit_n,"[[","bestTune")),
                                          unlist(lapply(pls_fit_adf,"[[","bestTune"))),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = pls_best_tune_df, aes(x = model, y = comp, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "No. of components",
       x = "Model variable",
       caption = "Best tune parameter (No. of components) for PLSR models") +
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
pls_best_rmse_df <- data.frame("rmse" = c((unlist(lapply(pls_op_n,"[[","RMSE"))/pls_mean_n)*100,
                                          (unlist(lapply(pls_op_adf,"[[","RMSE"))/pls_mean_adf)*100),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = pls_best_rmse_df, aes(x = model, y = rmse, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "rRMSEP (%)",
       x = "Model variable",
       caption = "relative RMSE (%) for PLSR models") +
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

pls_best_k_for_n <- which.min(unlist(lapply(pls_op_n,"[[","RMSE")))
pls_best_k_for_adf <- which.min(unlist(lapply(pls_op_adf,"[[","RMSE")))

set.seed(pls_seedVec[pls_best_k_for_n])

pls_best_fit_n <- pls_fit_n[[pls_best_k_for_n]]

print(pls_best_fit_n)
plot(pls_best_fit_n)

plot(varImp(pls_best_fit_n), 10, 
     main = "PLSR for N (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(pls_best_fit_n, pls_test_list[[pls_best_k_for_n]], 
              estimators, targetN, plot = TRUE, 
              title = "N (%) estimation model with PLSR")

### Best PLSR for ADF estimation with best train/test subset

set.seed(pls_seedVec[pls_best_k_for_adf])

pls_best_fit_adf <- pls_fit_adf[[pls_best_k_for_adf]]

print(pls_best_fit_adf)
plot(pls_best_fit_adf)

plot(varImp(pls_best_fit_adf), 10, 
     main = "PLSR for ADF (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(pls_best_fit_adf, pls_test_list[[pls_best_k_for_adf]], 
              estimators, targetADF, plot = TRUE, 
              title = "ADF (%) estimation model with PLSR")

save.image(file = "./output/models/100_times/plsr_100.RData")