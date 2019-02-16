###############################################
# 13.02.2018
# Forage qualitz modelling with ML
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
all_df$row <- c(1:nrow(all_df))


# 2. Data partioning and variable identification

set.seed(2147483647)

table(all_df$field_id) # Check exisisting data rows based on field id

## Separate data set in to two groups with same propotion from each group (60%)
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

## Subset to train data (1)
#train_df  <- split_sample[[1]]
train_df <-
  split_sample %>% dplyr::filter(.partitions == 1) %>% data.frame()
table(train_df$field_id)

## Subset to train data (2)
test_df <-
  split_sample %>% dplyr::filter(.partitions == 2) %>% data.frame()
table(test_df$field_id)

## Coulmn index for estimators (independent variables)
estimators <- c(5:122)

## Coulmn index for each targets (dependent variables)
targetN <- 130
targetC <- 131
targetADF <- 133


# 3. Model calibration (training)

## Preperation for parallel processing
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

set.seed(777)

## 3.1 PLSR
metric <- "RMSE"
tunegrid <- expand.grid(.ncomp = c(1:12))


# RFE
library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

set.seed(1)
x <- train_df[, estimators]
y <- train_df[, targetN]
plsrProfile <- rfe(
  x,
  y,
  sizes = c(10:118),
  rfeControl = rfeControl(functions = caretFuncs,
                          number = 5),
  method = "pls",
  trControl = myControl
)


# summarize the results
print(plsrProfile)
# list the chosen features
predictors(plsrProfile)
# plot the results
plot(plsrProfile, type = c("g", "o"))
