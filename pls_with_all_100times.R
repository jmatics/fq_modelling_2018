###############################################
# 15.02.2018
# PLSR with 100 times
# JW
###############################################


# Loading required packages
rm(list = ls())

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

################################################

# 1. Reading data tables

## Read WIZ data

wiz_df <-
  read.csv("./output_wiz/hs_all_nrm_df_wiz.csv", header = TRUE)
wiz_df$source <- rep("Witzenhausen", dim(wiz_df)[1])

## Read Rhön data

dsb_df <-
  read.csv("./output_dsb/hs_all_nrm_df_dsb.csv", header = TRUE)
dsb_df$source <- rep("Rhön", dim(dsb_df)[1])

## merging two data sets

all_df <- rbind(wiz_df, dsb_df)

# Define seed values
# 100 numbers generated between 1 and 100000

seedVec <-
  c(
    47999,
    86664,
    9896,
    81577,
    87574,
    60874,
    8445,
    61601,
    12542,
    84331,
    92156,
    42532,
    54205,
    77947,
    35020,
    47384,
    96268,
    99501,
    13234,
    30640,
    47115,
    51293,
    25621,
    69429,
    89133,
    50920,
    36120,
    52283,
    50955,
    28713,
    13129,
    73411,
    55214,
    3118,
    15432,
    96425,
    93839,
    67354,
    27001,
    46678,
    7002,
    78738,
    9028,
    24213,
    54594,
    67920,
    67009,
    18928,
    59971,
    85079,
    27368,
    70443,
    24754,
    91252,
    26147,
    22559,
    86933,
    7910,
    95300,
    19866,
    48144,
    52590,
    35932,
    67085,
    10149,
    18486,
    95006,
    445,
    83360,
    66332,
    38214,
    74489,
    53402,
    1486,
    5843,
    83213,
    17498,
    13772,
    3995,
    20366,
    36259,
    87131,
    30907,
    19117,
    77909,
    11248,
    24335,
    99879,
    21860,
    82767,
    51555,
    90820,
    16708,
    35086,
    32838,
    30931,
    68453,
    57428,
    61707,
    89342
  )

# set empty list for outputs

# 100 gives the number of repetitions of model building and dataset creation
train_list <- vector("list", 100) 
test_list <- vector("list", 100)

plsFit_n <- vector("list", 100)
op_n <- vector("list", 100)

plsFit_adf <- vector("list", 100)
op_adf <- vector("list", 100)



# Model calibrtion and validation 100 times

## Coulmn index for estimators (independent variables)
estimators <- c(5:122)

## Coulmn index for each targets (dependent variables)
targetN <- 130
targetADF <- 133

timestamp()
library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

## Define control parameters
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  returnResamp = "all",
  allowParallel = TRUE
)

metric <- "RMSE"
tunegrid <- expand.grid(.ncomp = c(1:20))

tictoc::tic("forLoop")
for (k in 1:length(seedVec)) {
  set.seed(seedVec[k])
  
  # Data spliting
  split_sample <-
    groupdata2::partition(
      all_df,
      p = 0.8,
      cat_col = "field_id",
      num_col = NULL,
      id_col = NULL,
      id_aggregation_fn = sum,
      force_equal = FALSE,
      list_out = F
    )
  
  train_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) %>% data.frame()
  
  test_list[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2) %>% data.frame()
  
  test_list[[k]]$seed <- rep(seedVec[k], dim(test_list[[k]])[1])
  
  # Train PLSR model for N %
  plsFit_n[[k]] <-
    train(
      n ~ .,
      data = train_list[[k]][, c(targetN, estimators)],
      method = "pls",
      metric = metric,
      tuneGrid = tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale")
    )
  
  # Train PLSR model for ADF %
  plsFit_adf[[k]] <-
    train(
      adf ~ .,
      data = train_list[[k]][, c(targetADF, estimators)],
      method = "pls",
      metric = metric,
      tuneGrid = tunegrid,
      trControl = myControl,
      preProcess = c("center", "scale")
    )
  
  # Test PLSR model for N %
  op_n[[k]] <- postResample(test_list[[k]][,targetN],
                                predict(plsFit_n[[k]], newdata=test_list[[k]][, estimators]))
  
  # Test PLSR model for ADF %
  op_adf[[k]] <- postResample(test_list[[k]][,targetADF],
                            predict(plsFit_adf[[k]], newdata=test_list[[k]][, estimators]))
  
}
tictoc::toc()


# Results for N
boxplot(unlist(lapply(op_n,"[[","RMSE"))) 
# plot all calculated RMSE values between predicted and observed values of the validation dataset
boxplot(unlist(lapply(plsFit_n,"[[","bestTune"))) 
# plot to show the best tuning parameter for all models (for PLSR it is the number of latent variables for other models more than one parameter might be optimized) 

mean_n <- numeric()
for(i in 1:100){
  mean_n[i] <- mean(test_list[[i]][,targetN])
}


boxplot((unlist(lapply(op_n,"[[","RMSE"))/mean_n)*100)
# Results for ADF

boxplot(unlist(lapply(op_adf,"[[","RMSE"))) 
boxplot(unlist(lapply(plsFit_adf,"[[","bestTune"))) 

mean_adf <- numeric()
for(i in 1:100){
  mean_adf[i] <- mean(test_list[[i]][,targetADF])
}

boxplot((unlist(lapply(op_adf,"[[","RMSE"))/mean_adf)*100)


