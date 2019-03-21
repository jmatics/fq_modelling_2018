library(caret)
load("./output/models/plsr_model_for_N_ADF.RData")
load("./output/models/rfr_model_for_N_ADF.RData")
load("./output/models/gpr_model_for_N_ADF.RData")
load("./output/models/svmr_model_for_N_ADF.RData")
load("./output/models/mars_model_for_N_ADF.RData")
load("./output/models/sgbr_model_for_N_ADF.RData")
load("./output/models/cubist_model_for_N_ADF.RData")

n_models <- resamples(list("N_PLSR" = pls_all_n,
                           "N_RFR" = rf_all_n, 
                           "N_GPR" = gp_all_n, 
                           "N_SVMR" = svm_all_n, 
                           "N_MARS" = mars_all_n, 
                           "N_SGBR" = sgb_all_n, 
                           "N_CUBIST" = cub_all_n))

summary(n_models)
bwplot(n_models)
densityplot(n_models, metric = "RMSE")


adf_models <- resamples(list("ADF_PLSR" = pls_all_adf,
                             "ADF_RFR" = rf_all_adf, 
                             "ADF_GPR" = gp_all_adf,  
                           "ADF_SVMR" = svm_all_adf, 
                           "ADF_MARS" = mars_all_adf, 
                           "ADF_SGBR" = sgb_all_adf, 
                           "ADF_CUBIST" = cub_all_adf))

summary(adf_models)
bwplot(adf_models)
densityplot(adf_models, metric = "RMSE")