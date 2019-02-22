load("./output/models/100_times/plsr_100.RData")
load("./output/models/100_times/rfr_100.RData")
load("./output/models/100_times/gpr_100.RData")
load("./output/models/100_times/svmr_100.RData")

library(ggplot2)
library(dplyr)

pls_plot <- ggplot(data = pls_best_tune_df, aes(x = model, y = comp, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "No. of components",
       x = "Model variable",
       title = "PLSR",
       caption = "Best tune parameter (No. of components) for PLSR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%DM)", "Nitrogen (%DM)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

rf_plot <- ggplot(data = rf_best_tune_df, aes(x = model, y = comp, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Mtry",
       x = "Model variable",
       title = "RFR",
       caption = "Best tune parameter (Mtry) for RFR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%DM)", "Nitrogen (%DM)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

gp_plot <- ggplot(data = gp_best_tune_df, aes(x = model, y = comp, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Sigma",
       x = "Model variable",
       title = "GPR",
       caption = "Best tune parameter (Sigma) for GPR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%DM)", "Nitrogen (%DM)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

sigma_plot <- ggplot(data = svm_best_tune_df, aes(x = model, y = comp_sigma, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Sigma",
       x = "Model variable",
       title = "SVMR",
       caption = "Best tune parameter (Sigma) for SVMR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%DM)", "Nitrogen (%DM)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cost_plot <- ggplot(data = svm_best_tune_df, aes(x = model, y = comp_cost, fill = model)) + 
  geom_boxplot() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(y = "Cost",
       x = "Model variable",
       title = "SVMR",
       caption = "Best tune parameter (Cost) for SVMR models") +
  scale_fill_manual(values = c("#42858C", "#E48F1B"),
                    name = "Forage quality parameter", 
                    labels = c("ADF (%DM)", "Nitrogen (%DM)")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

svm_plot <- cowplot::plot_grid(sigma_plot, cost_plot, nrow = 1, ncol = 2)

cowplot::plot_grid(pls_plot, rf_plot, gp_plot, svm_plot, nrow = 2, ncol = 2)
