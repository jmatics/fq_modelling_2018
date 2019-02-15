###############################################
# 28.01.2018
# PLSR with Caret for N modelling
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

# 1. Reading data tables separtely

## biomass data

bm_list <- dir(path = "./input_wiz/bm/", pattern = "*.csv", full.names = TRUE)

bm_data <- bm_list %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind)     # reduce with rbind into one dataframe

## nitrogen data

cn_data <- read.csv("./input_wiz/cn/cn_all.csv", header = TRUE)

## hyperspectral data

hs_list <- dir(path = "./input_wiz/hs/", pattern = "*.csv", full.names = TRUE)

hs_read <- hs_list %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind)     # reduce with rbind into one dataframe
hs_data <- hs_read[-c(5:12, 119:142)]
# 2. Merging and removing NAs

## hyperspectral and biomass

hs_bm_data <- merge(bm_data, hs_data) %>% tidyr::drop_na()

## hyperspectral and nitrogen

hs_cn_data <- merge(cn_data, hs_data) %>% tidyr::drop_na()
#hs_cn_data$n_log <- sqrt(hs_cn_data$n)

## all three tables

hs_bm_cn_data <- merge(merge(bm_data, cn_data), hs_data) %>% tidyr::drop_na()


# 3. Data split

## Without considering number of data for each grassland

set.seed(1205)
trn_samples <- hs_cn_data$n %>% 
  createDataPartition(p = 0.65, list = FALSE)
train_hs_cn  <- hs_cn_data[trn_samples, ]
test_hs_cn <- hs_cn_data[-trn_samples, ]

col_spec <- c(9:114)
col_N <- 5
col_C <- 6

## PLS regression model calibration

set.seed(1205)

control <- trainControl(method="repeatedcv", 
                        number=11, 
                        repeats=9,
                        returnResamp = "all")

tunegrid <- expand.grid(.ncomp=c(1:10))
metric <- "RMSE"

mod_pls_N <- train(n~., 
                   data = train_hs_cn[,c(col_N, col_spec)], 
                   method = "pls", metric = metric, 
                   tuneGrid = tunegrid, 
                   tunegrid=control, 
                   preProcess = c("center", "scale"))

mod_pls_N
plot(mod_pls_N)
mod_pls_N$bestTune
summary(mod_pls_N)
plot(varImp(mod_pls_N), 20)

predictions <- mod_pls_N %>% predict(test_hs_cn[,c(col_N, col_spec)])
data.frame(
  RMSE = caret::RMSE(predictions, test_hs_cn$n),
  Rsquare = caret::R2(predictions, test_hs_cn$n)
)
plot(test_hs_cn$n_log, predictions)
abline(0,1,lty=2,lwd=1.3)

my.function.testdata <- function(model,data,testX,testY,plot=TRUE,title=FALSE){
  yhat = predict(model , data[,testX])
  out <- postResample(yhat,data[,testY])
  if(plot==TRUE){
    plot(as.matrix(data[,testY]),yhat,main=title,pch=16,cex=1.2,ylab="predicted",xlab="observed",
         sub=paste("R² = ",round(out[2],2),",  rRMSEP = ",round(abs(out[1]/mean(test_df[,testY]))*100,1),"%",sep=""))
    abline(0,1,lty=2,lwd=1.3)
  }
  print(out)
}

my.function.testdata(mod_pls_N, test_hs_cn, 
                     col_spec, col_N,
                     plot=TRUE,title="PLS")


obs_pred_plot <- function(model, df, estimators, target, plot = TRUE, title = FALSE){
  op_df <- data.frame("field_id" = df$field_id,
                      "source" = df$source,
                      "Observed" = df[, target],
                      "Predicted" = predict(model , df[, estimators]),
                      "Residuals" = df[, target] - predict(model , df[, estimators]))
  valid_out <- postResample(op_df$Predicted, op_df$Observed)
  if(plot==TRUE){
    opPlot <- ggplot(op_df, 
           aes(x=Predicted, y=Observed)) +
      geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
      stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
      geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
                  linetype = "twodash", color = "black") +
      theme_few(base_family = "Helvetica") + 
      coord_fixed(ratio = 1) +
      jcolors::scale_color_jcolors(palette = "pal3", name = "Location") +
      labs(x="Predicted", 
           y="Observed",
           caption=paste("R² = ", round(valid_out[2], 2), ",   ",
                         "rRMSEP = ", round(abs(valid_out[1]/mean(op_df$Observed)), 2), 
                         " %", "\n", "Predicted vs observed plot", sep = "")) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=13),
            legend.title = element_text(size=12, face="bold"),
            legend.text = element_text(size=11),
            plot.caption=element_text(size=11, face="italic", hjust=1),
            legend.position = "top")
    
    prPlot <- ggplot(op_df, 
           aes(x=Predicted, y=Residuals)) +
      geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
      geom_abline(mapping = NULL, data = NULL, slope = 0, intercept = 0, 
                  linetype = "twodash", color = "black") +
      ggthemes::theme_few(base_family = "Helvetica") + 
      coord_fixed(ratio = 1) +
      jcolors::scale_color_jcolors(palette = "pal3", name = "Location") +
      labs(x="Predicted", 
           y="Residuals",
           caption=paste("\n", "Predicted vs residual plot", sep = "")) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=13),
            legend.title = element_text(size=12, face="bold"),
            legend.text = element_text(size=11),
            plot.caption=element_text(size=11, face="italic", hjust=1),
            legend.position = "top")
    
    bothPlots <- cowplot::plot_grid(opPlot, prPlot, ncol = 2, nrow = 1,
                       align="hv")
    ggpubr::annotate_figure(bothPlots,
                    top = ggpubr::text_grob(title, family="Helvetica", face = "bold", size = 16))
  }
}
  function(model,data,testX,testY,plot=TRUE,title=FALSE){
  yhat = predict(model , data[,testX])
  out <- postResample(yhat,data[,testY])
  if(plot==TRUE){
    plot(as.matrix(data[,testY]),yhat,main=title,pch=16,cex=1.2,ylab="predicted",xlab="observed",
         sub=paste("R² = ",round(out[2],2),",  rRMSEP = ",round(abs(out[1]/mean(test_df[,testY]))*100,1),"%",sep=""))
    abline(0,1,lty=2,lwd=1.3)
  }
  print(out)
}
