###########################################################
# 22.02.2018
# Forage quality estimation with PLSR, RFR, GPR, and SVMR
# JW
###########################################################


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


# 2. Data partioning and variable identification

set.seed(777)

table(all_df$field_id) # Check exisisting data rows based on field id

## Separate data set in to two groups with same propotion from each group (60%)
split_sample <-
  groupdata2::partition(
    all_df %>% dplyr::filter(source == "Witzenhausen"),
    p = 0.8,
    cat_col = "field_id",
    force_equal = FALSE,
    list_out = F
  )  %>%
  data.frame()

## Subset to train data (1)
#train_df  <- split_sample[[1]]
train_df <- split_sample %>% dplyr::filter(.partitions == 1) %>% dplyr::select(-.partitions)
table(train_df$field_id)

## Subset to train data (2)
test_df <- split_sample %>% dplyr::filter(.partitions == 2)
table(test_df$field_id)

## Coulmn index for estimators (independent variables)
estimators <- c(5:122)

## Coulmn index for each targets (dependent variables)
targetN <- 131
targetADF <- 134

# 3. Model calibration (training)

## Define control parameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp = "all",
                          allowParallel = TRUE)

metric <- "RMSE"

pls_tunegrid_n <- expand.grid(.ncomp=c(7))
pls_tunegrid_adf <- expand.grid(.ncomp=c(7))

rf_tunegrid_n <- expand.grid(.mtry=c(14))
rf_tunegrid_adf <- expand.grid(.mtry=c(14))

gp_tunegrid_n <- expand.grid(sigma=c(0.045))
gp_tunegrid_adf <- expand.grid(sigma=c(0.040))

svm_tunegrid_n <- expand.grid(sigma=c(0.03), C=c(12))
svm_tunegrid_adf <- expand.grid(sigma=c(0.03), C=c(12))

## Run calibration (training)

tictoc::tic("training")
library(doParallel)
cls = makeCluster(detectCores()-1) 
registerDoParallel(cls)

### PLSR

pls_n <- train(
  n ~ .,
  data = train_df[, c(targetN, estimators)],
  method = "pls",
  metric = metric,
  tuneGrid = pls_tunegrid_n,
  trControl = myControl,
  preProcess = c("center", "scale")
)

pls_adf <- train(
  adf ~ .,
  data = train_df[, c(targetADF, estimators)],
  method = "pls",
  metric = metric,
  tuneGrid = pls_tunegrid_adf,
  trControl = myControl,
  preProcess = c("center", "scale")
)

### RFR

rf_n <- train(
  n ~ .,
  data = train_df[, c(targetN, estimators)],
  method = "rf",
  metric = metric,
  tuneGrid = rf_tunegrid_n,
  trControl = myControl,
  preProcess = c("center", "scale"),
  importance = T
)

rf_adf <- train(
  adf ~ .,
  data = train_df[, c(targetADF, estimators)],
  method = "rf",
  metric = metric,
  tuneGrid = rf_tunegrid_adf,
  trControl = myControl,
  preProcess = c("center", "scale"),
  importance = T
)

### GPR

gp_n <-
  train(
    n ~ .,
    data = train_df[, c(targetN, estimators)],
    method = "gaussprRadial",
    metric = metric,
    tuneGrid = gp_tunegrid_n,
    trControl = myControl,
    preProcess = c("center", "scale"),
    importance = T
  )

gp_adf <-
  train(
    adf ~ .,
    data = train_df[, c(targetADF, estimators)],
    method = "gaussprRadial",
    metric = metric,
    tuneGrid = gp_tunegrid_adf,
    trControl = myControl,
    preProcess = c("center", "scale"),
    importance = T
  )

### SVMR

svm_n <-
  train(
    n ~ .,
    data = train_df[, c(targetN, estimators)],
    method = "svmRadial",
    metric = metric,
    tuneGrid = svm_tunegrid_n,
    trControl = myControl,
    preProcess = c("center", "scale"),
    importance = T
  )

svm_adf <-
  train(
    adf ~ .,
    data = train_df[, c(targetADF, estimators)],
    method = "svmRadial",
    metric = metric,
    tuneGrid = svm_tunegrid_adf,
    trControl = myControl,
    preProcess = c("center", "scale"),
    importance = T
  )

stopCluster(cls)
tictoc::toc()

# 4. Model resampling

## N
models_for_n <- resamples(list("PLSR-N" = pls_n,
                               "RFR-N" = rf_n,
                               "GPR-N" = gp_n,
                               "SVMR-N" = svm_n))
summary(models_for_n)
bwplot(models_for_n, scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

## ADF
models_for_adf <- resamples(list("PLSR-ADF" = pls_adf,
                                 "RFR-ADF" = rf_adf,
                                 "GPR-ADF" = gp_adf,
                                 "SVMR-ADF" = svm_adf))
summary(models_for_adf)
bwplot(models_for_adf, scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

save(pls_n, pls_adf, rf_n,rf_adf, gp_n, gp_adf, svm_n, svm_adf, file = "./output/models/pls_rfr_gpr_svmr_for_n_adf_wiz.RData")

# 5. Model validation

## Predict with test data

### PLSR
pls_n_pred <- predict(pls_n , test_df[, estimators])
postResample(pred = pls_n_pred, obs = test_df$n)
pls_adf_pred <- predict(pls_adf , test_df[, estimators])
postResample(pred = pls_adf_pred, obs = test_df$adf)

### RFR
rf_n_pred <- predict(rf_n , test_df[, estimators])
postResample(pred = rf_n_pred, obs = test_df$n)
rf_adf_pred <- predict(rf_adf , test_df[, estimators])
postResample(pred = rf_adf_pred, obs = test_df$adf)

### GPR
gp_n_pred <- predict(gp_n , test_df[, estimators])
postResample(pred = gp_n_pred, obs = test_df$n)
gp_adf_pred <- predict(gp_adf , test_df[, estimators])
postResample(pred = gp_adf_pred, obs = test_df$adf)

### SVMR
svm_n_pred <- predict(svm_n , test_df[, estimators])
postResample(pred = svm_n_pred, obs = test_df$n)
svm_adf_pred <- predict(svm_adf , test_df[, estimators])
postResample(pred = svm_adf_pred, obs = test_df$adf)

### Observed, Predicted dataframe

op_df <- cbind(test_df[-(estimators)], pls_n_pred, pls_adf_pred,
               rf_n_pred, rf_adf_pred,
               gp_n_pred, gp_adf_pred,
               svm_n_pred, svm_adf_pred)

write.csv(op_df, "./output/models/plsr_rfr_gpr_svmr_op_wiz.csv", row.names = FALSE)
