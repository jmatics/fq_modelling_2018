load("./output/models/pls_rfr_gpr_svmr_for_n_adf.RData")
library(caret)

imp_n_df <- data.frame("wv" = seq(482, 950, 4),
                     "pls_n_imp" = (varImp(pls_n)$importance)$Overall,
                     "rf_n_imp" = (varImp(rf_n)$importance)$Overall,
                     "gp_n_imp" = (varImp(gp_n)$importance)$Overall,
                     "svm_n_imp" = (varImp(svm_n)$importance)$Overall
                     )


imp_n_df_melt <- reshape2::melt(imp_n_df, id = "wv")

model_n_label <- labeller(variable = c("pls_n_imp" = "PLSR",
                                       "rf_n_imp" = "RFR",
                                       "gp_n_imp" = "GPR",
                                       "svm_n_imp" = "SVMR"))

plot_n_imp <- ggplot(imp_n_df_melt %>% dplyr::filter(value > 60), 
       aes(x = wv, y = value)) +
  geom_bar(stat="identity", color="#E48F1B", fill = "gray99") + 
  #coord_flip() +
  facet_grid(~ variable, labeller = model_n_label) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(x = "Wavelength (nm)",
       y = "Overall score for importance of prediction",
       caption = "Important wavelengths for N (%DM) prediction with more than 60 score") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )


imp_adf_df <- data.frame("wv" = seq(482, 950, 4),
                       "pls_adf_imp" = (varImp(pls_adf)$importance)$Overall,
                       "rf_adf_imp" = (varImp(rf_adf)$importance)$Overall,
                       "gp_adf_imp" = (varImp(gp_adf)$importance)$Overall,
                       "svm_adf_imp" = (varImp(svm_adf)$importance)$Overall
)


imp_adf_df_melt <- reshape2::melt(imp_adf_df, id = "wv")

model_adf_label <- labeller(variable = c("pls_adf_imp" = "PLSR",
                                       "rf_adf_imp" = "RFR",
                                       "gp_adf_imp" = "GPR",
                                       "svm_adf_imp" = "SVMR"))

plot_adf_imp <- ggplot(imp_adf_df_melt %>% dplyr::filter(value > 60), 
       aes(x = wv, y = value)) +
  geom_bar(stat="identity", color="#42858C", fill = "gray99") + 
  #coord_flip() +
  facet_grid(~ variable, labeller = model_adf_label) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(x = "Wavelength (nm)",
       y = "Overall score for importance of prediction",
       caption = "Important wavelengths for ADF (%DM) prediction with more than 60 score") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(plot_n_imp, plot_adf_imp, nrow = 2, ncol = 1)