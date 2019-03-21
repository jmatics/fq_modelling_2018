###############################################
# 20.03.2018
# Forage qualitz modelling with CUBISTR
# JW
###############################################


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


### Histogram plot for N concentration data
ggplot(all_df, aes (x = doy, y = n, fill = field_id)) + 
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

### Histogram plot for N concentration data
ggplot(all_df, aes (x = doy, y = adf, fill = field_id)) + 
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


# 2. Data partioning and variable identification

set.seed(777)

table(all_df$field_id) # Check exisisting data rows based on field id

## Separate data set in to two groups with same propotion from each group (60%)
split_sample <-
  groupdata2::partition(
    all_df,
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

## Preperation for parallel processing
tictoc::tic("n")
library(doParallel)
cls = makeCluster(detectCores()-1) 
registerDoParallel(cls)

## Define control parameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp = "all",
                          allowParallel = TRUE)

set.seed(777)

## 3.1 CUBISTR
metric <- "RMSE"
tunegrid <- expand.grid(committees  = c(seq(1, 100, 10)),
                        neighbors  = c(1, 3, 5, 7, 9))

### CUBIST for N
cub_all_n <- train(n ~., data = train_df[, c(targetN, estimators)], 
                   method = "cubist", metric = metric, 
                   tuneGrid = tunegrid, 
                   trControl = myControl, 
                   preProcess = c("center", "scale"), 
                   importance = T)
stopCluster(cls)
tictoc::toc()
cub_all_n
plot(cub_all_n)

### CUBIST for ADF
tictoc::tic("adf")
cls = makeCluster(detectCores()-1)
registerDoParallel(cls)
cub_all_adf <- train(adf ~., data = train_df[, c(targetADF, estimators)], 
                     method = "cubist", metric = metric, 
                     tuneGrid = tunegrid, 
                     trControl = myControl,
                     preProcess = c("center", "scale"), 
                     importance = T)
stopCluster(cls)
tictoc::toc()

cub_all_adf
plot(cub_all_adf)

save(cub_all_n, cub_all_adf, file = "./output/models/cubist_model_for_N_ADF.RData")
timestamp()
### Visulaise calibrated models

plot(varImp(cub_all_n), 10, main = "N_CUBISTR")
plot(varImp(cub_all_adf), 10, main = "ADF_CUBISTR")


cub_all_compare <- resamples(list("N_CUBISTR" = cub_all_n,
                                  "ADF_CUBISTR" = cub_all_adf))
summary(cub_all_compare)


bwplot(cub_all_compare, scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))
dotplot(cub_all_compare, scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))


# 4. Model validation

source("./funs/obs_pred_plot.R")

obs_pred_plot(cub_all_n, test_df, 
              estimators, targetN, plot = TRUE, 
              title = "N (%) estimation model with CUBISTR")


obs_pred_plot(cub_all_adf, test_df, 
              estimators, targetADF, plot = TRUE, 
              title = "ADF (%) estimation model with CUBISTR")
# densityplot(gp_all_n, comps = 1:3)
#  scoreplot(gp_all_adf$finalModel, comps = 1:3)  
#  loadingplot(gp_all_n$finalModel, comps = 1:3, legendpos = "bottomright")
#  corrplot(gp_all_n$finalModel, comps = 1:3, legendpos = "bottomright")
#  coefplot(gp_all_n$finalModel, comps = 1:3, legendpos = "bottomright")
