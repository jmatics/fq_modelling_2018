###############################################
# 19.02.2018
# RFE for PLSR with N (%)
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
#all_df$row <- c(1:nrow(all_df))


# 2. Data partioning and variable identification

set.seed(777)

table(all_df$field_id) # Check exisisting data rows based on field id

## Coulmn index for estimators (independent variables)
estimators <- c(5:122)

## Coulmn index for each targets (dependent variables)
targetN <- 131
targetADF <- 134


# 3. RFE

## Define control parameters
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  returnResamp = "all",
  allowParallel = TRUE
)

metric <- "RMSE"
tunegrid <- expand.grid(.ncomp = c(1:12))


## with N

tictoc::tic("rfe")

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

set.seed(777)
x <- all_df[, estimators]
y <- all_df[, targetN]
plsr_n_profile <- rfe(
  x,
  y,
  sizes = c(5:118),
  rfeControl = rfeControl(functions = caretFuncs,
                          number = 5),
  method = "pls",
  trControl = myControl,
  preProcess = c("center", "scale")
)
tictoc::toc()

save(plsr_n_profile, file = "./output/models/plsr_n_profile.RData")


# summarize the results
print(plsr_n_profile)
# list the chosen features
predictors(plsr_n_profile)
# plot the results
plot(plsr_n_profile, type = c("g", "o"))

plsr_n_df <- data.frame("rmse" = plsr_n_profile$results$RMSE,
                        "var" = plsr_n_profile$results$Variables)

ggplot(data = plsr_n_df, aes(x = var, y = (rmse/mean(all_df$n))*100)) +
  geom_line(col = "#E48F1B") +
  geom_point(col = "#E48F1B") +
  theme_bw(base_size = 12, base_family = "Lucida") +
  labs(x = "Variables",
       y = "rRMSEP (%) (Bootstrap)",
       caption = "Recursive feature elimination (RFE) for PLSR model with N (%)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )


###########


## with ADF

tictoc::tic("rfe")

library(doParallel)
cls = makeCluster(detectCores() - 1)
registerDoParallel(cls)

set.seed(777)
x <- all_df[, estimators]
y <- all_df[, targetADF]
plsr_adf_profile <- rfe(
  x,
  y,
  sizes = c(5:118),
  rfeControl = rfeControl(functions = caretFuncs,
                          number = 5),
  method = "pls",
  trControl = myControl,
  preProcess = c("center", "scale")
)
tictoc::toc()

save(plsr_adf_profile, file = "./output/models/plsr_adf_profile.RData")


# summarize the results
print(plsr_adf_profile)
# list the chosen features
predictors(plsr_adf_profile)
# plot the results
plot(plsr_adf_profile, type = c("g", "o"))

plsr_adf_df <- data.frame("rmse" = plsr_adf_profile$results$RMSE,
                        "var" = plsr_adf_profile$results$Variables)

ggplot(data = plsr_adf_df, aes(x = var, y = (rmse/mean(all_df$adf))*100)) +
  geom_line(col = "#42858C") +
  geom_point(col = "#42858C") +
  theme_bw(base_size = 12, base_family = "Lucida") +
  labs(x = "Variables",
       y = "rRMSEP (%) (Bootstrap)",
       caption = "Recursive feature elimination (RFE) for PLSR model with ADF (%)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )
