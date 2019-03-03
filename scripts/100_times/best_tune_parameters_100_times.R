library(dplyr)

load("./output/models/100_times/plsr_100.RData")
pls_best_tune_df %>% group_by(model) %>% summarise(median(comp))

load("./output/models/100_times/rfr_100.RData")
rf_best_tune_df %>% group_by(model) %>% summarise(median(comp))

load("./output/models/100_times/gpr_100.RData")
gp_best_tune_df %>% group_by(model) %>% summarise(median(comp))

load("./output/models/100_times/svmr_100.RData")
svm_best_tune_df %>% group_by(model) %>% summarise(median(comp_sigma))
svm_best_tune_df %>% group_by(model) %>% summarise(median(comp_cost))

load("./output/models/100_times/nnr_100.RData")
nn_best_tune_df %>% group_by(model) %>% summarise(median(comp_size))
nn_best_tune_df %>% group_by(model) %>% summarise(median(comp_decay))