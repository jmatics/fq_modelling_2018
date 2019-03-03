########################################################
# 02.03.2018
# Forage qualitz modelling with NNR (100 permutations)
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

nn_seedVec <- sample(c(1:99999), 100, replace=FALSE)

nn_train_list <- vector("list", 100) 
nn_test_list <- vector("list", 100)

nn_fit_n <- vector("list", 100)
nn_op_n <- vector("list", 100)

nn_fit_adf <- vector("list", 100)
nn_op_adf <- vector("list", 100)

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
nn_tunegrid <- expand.grid(.decay = c(0.5, 0.2, 0.1), .size = c(5, 6, 7))

### Cal/Val process 100 times

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

tictoc::tic("forLoop")
for (k in 1:length(nn_seedVec)) {
  set.seed(nn_seedVec[k])
  
  # Data spliting
  split_sample <-
    groupdata2::partition(
      all_df,
      p = 0.8,
      cat_col = "field_id",
      force_equal = FALSE,
      list_out = F
    ) %>% data.frame()
  
  nn_train_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) 
  
  nn_test_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2)
  
  nn_train_list[[k]]$seed <- rep(nn_seedVec[k], dim(nn_train_list[[k]])[1])
  nn_test_list[[k]]$seed <- rep(nn_seedVec[k], dim(nn_test_list[[k]])[1])
  
  # Train NNR model for N %
  nn_fit_n[[k]] <-
    train(
      n ~ .,
      data = nn_train_list[[k]][, c(targetN, estimators)],
      method = "nnet",
      metric = metric,
      tuneGrid = nn_tunegrid,
      preProcess = c("center", "scale"),
      maxit = 5000,
      trace = F,
      linout = 1
    )
  
  # Train NNR model for ADF %
  nn_fit_adf[[k]] <-
    train(
      adf ~ .,
      data = nn_train_list[[k]][, c(targetADF, estimators)],
      method = "nnet",
      metric = metric,
      tuneGrid = nn_tunegrid,
      preProcess = c("center", "scale"),
      maxit = 5000,
      trace = F,
      linout = 1
    )
  
  # Test NNR model for N %
  nn_op_n[[k]] <- postResample(nn_test_list[[k]][, targetN],
                                predict(nn_fit_n[[k]], 
                                        newdata =nn_test_list[[k]][, estimators]))
  
  # Test NNR model for ADF %
  nn_op_adf[[k]] <- postResample(nn_test_list[[k]][, targetADF],
                                  predict(nn_fit_adf[[k]], 
                                          newdata = nn_test_list[[k]][, estimators]))
  
}
tictoc::toc()


### Prepartion for visualise cal/val outputs

# Compute mean N (%) for each test data subset to calculate rRMSEP

nn_mean_n <- numeric()
for(i in 1:100){
  nn_mean_n[i] <- mean(nn_test_list[[i]][, targetN])
}

# Compute mean ADF (%) for each test data subset to calculate rRMSEP

nn_mean_adf <- numeric()
for (i in 1:100) {
  nn_mean_adf[i] <- mean(nn_test_list[[i]][, targetADF])
}

### Model cal/val output plots

# Plots for best tune parameter (No. of components)
nn_best_tune_df <- data.frame("comp_size" = c(unlist(lapply(nn_fit_n,"[[","bestTune"))[seq(1,100,2)], unlist(lapply(nn_fit_adf,"[[","bestTune"))[seq(1,100,2)]),
                               "comp_decay" = c(unlist(lapply(nn_fit_n,"[[","bestTune"))[seq(2,100,2)], unlist(lapply(nn_fit_adf,"[[","bestTune"))[seq(2,100,2)]),
                               "model" = c(rep("N", 100), rep("ADF", 100),
                                           rep("N", 100), rep("ADF", 100)),
                               "tune" = c(rep("size", 200), rep("decay", 200))) 

size_plot <- ggplot(data = nn_best_tune_df, aes(x = model, y = comp_size, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "size",
       x = "Model variable",
       caption = "Best tune parameter (size) for NNR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

decay_plot <- ggplot(data = nn_best_tune_df, aes(x = model, y = comp_decay, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "decay",
       x = "Model variable",
       caption = "Best tune parameter (decay) for NNR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%)", "Nitrogen (%)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(size_plot, decay_plot, nrow = 1, ncol = 2)

# rRMSEP plots
nn_best_rmse_df <- data.frame("rmse" = c((unlist(lapply(nn_op_n,"[[","RMSE"))/nn_mean_n)*100,
                                          (unlist(lapply(nn_op_adf,"[[","RMSE"))/nn_mean_adf)*100),
                               "model" = c(rep("N", 100), rep("ADF", 100)))

ggplot(data = nn_best_rmse_df, aes(x = model, y = rmse, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "rRMSEP (%)",
       x = "Model variable",
       caption = "relative RMSE (%) for NNR models") +
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

nn_best_k_for_n <- which.min(unlist(lapply(nn_op_n,"[[","RMSE")))
nn_best_k_for_adf <- which.min(unlist(lapply(nn_op_adf,"[[","RMSE")))

set.seed(nn_seedVec[nn_best_k_for_n])

nn_best_fit_n <- nn_fit_n[[nn_best_k_for_n]]

print(nn_best_fit_n)
plot(nn_best_fit_n)

plot(varImp(nn_best_fit_n), 10, 
     main = "NNR for N (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(nn_best_fit_n, nn_test_list[[nn_best_k_for_n]], 
              estimators, targetN, plot = TRUE, 
              title = "N (%) estimation model with NNR")

### Best NNR for ADF estimation with best train/test subset

set.seed(nn_seedVec[nn_best_k_for_adf])

nn_best_fit_adf <- nn_fit_adf[[nn_best_k_for_adf]]

print(nn_best_fit_adf)
plot(nn_best_fit_adf)

plot(varImp(nn_best_fit_adf), 10, 
     main = "NNR for ADF (%) estimation - Important variables for prediction")

source("./funs/obs_pred_plot.R")

obs_pred_plot(nn_best_fit_adf, nn_test_list[[nn_best_k_for_adf]], 
              estimators, targetADF, plot = TRUE, 
              title = "ADF (%) estimation model with NNR")

save.image(file = "./output/models/100_times/nnr_100.RData")
