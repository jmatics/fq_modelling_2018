### Edited script by JW (jayan.wijesingha@uni-kassel.de) #######
### First version by Thomas Astor (thastor@uni-kassel.de) ######

# Repeated model building (using caret) for 100 randomly selected training and validation datasets
# Build calibration (60%) and validation (40%) dataset

library(caret)
library(dplyr)

trainctrl<-trainControl(method="repeatedcv",number=5,repeats=3) # define validation procedure for model calibration (optimization based on tuning parameter)

set.seed(777)

# dataset containing all explanatory and all dependent variables
spec2 <- dr_all 

# delete all unnecessary variables
spec2$fp_id <- NULL
spec2$db_lup <- NULL

# 100 gives the number of repetitions of model building and dataset creation
spec2.train <- vector("list",100) 
spec2.test <- vector("list",100)

plsFit <- vector("list",100)
pred.obs <- vector("list",100)

# generate random 100 seed values between 1 and 99999 to select random portion of the train / test data
seed_vec <- sample(c(1:99999),100, replace=FALSE) 

# For each `k` different 'seed value' will be set and sampling data based on the given seed
for (k in 1:length(seed_vec)){
  set.seed(seed_vec[k])
  
  # Split balanced samples based on your group variable
  # Here, wanted to keep similar propotion of the each field in both train & test data
  # Feed a column name that have your group data to `cat_col`
  # If you don't have `groupdata2` library, please install it first
  # For different partion sizes change `p` value (Ex: 0.6 for 60%, 0.65 for 65%)
  
  split_sample <-
    groupdata2::partition(
      spec2,
      p = 0.6,
      cat_col = "field_id",
      force_equal = FALSE,
      list_out = F
    ) %>% data.frame()
  
  # Train subset
  spec2.train[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 1) %>% 
    data.frame() %>% 
    dplyr::select(-.partitions)
  
  # Test subset
  spec2.test[[k]] <-
    split_sample %>% dplyr::filter(.partitions == 2) %>% 
    data.frame() %>% 
    dplyr::select(-.partitions)
  
  # Removed code from original version
  # spec2$row <- c(1:nrow(spec2))
  # 
  # spec.list <- split(spec2,rep(c(1:6),times=15))
  # spec.train <- unlist(lapply(spec.list, function(x) sample(x$row,floor((nrow(x)/10)*6)))) #the sample commant defines that 60% of the samples(rows) are selected for training. If you want to change the proportion you have to change the 6 (e.g. 6 to 7.5 for getting 75% of all samples)
  # 
  # spec2$row <- NULL
  # 
  # spec2.train[[i]] <- spec2[spec.train,]
  # spec2.test[[i]] <- spec2[-spec.train,]
  # End 
  
  # Model calibration
  plsFit[[k]] <- train(
    fb_lup ~ .,
    data = spec2.train[[k]],
    method = "pls",
    preProc = c("center", "scale"),
    tuneLength = 10,
    trControl = trainctrl
  )
  # using train you can select various model types but be aware about the correct tuning parameter (see: http://topepo.github.io/caret/using-your-own-model-in-train.html)
  
  pred.obs[[k]] <- postResample(spec2.test[[k]][, 1],
                                predict(plsFit[[k]], 
                                        newdata = spec2.test[[k]][, -1]))
  
}


# plot all calculated RMSE values between predicted and observed values of the validation dataset
boxplot(unlist(lapply(pred.obs,"[[","RMSE"))) 

# plot to show the best tuning parameter for all models (for PLSR it is the number of latent variables for other models more than one parameter might be optimized) 
boxplot(unlist(lapply(plsFit,"[[","bestTune"))) 

