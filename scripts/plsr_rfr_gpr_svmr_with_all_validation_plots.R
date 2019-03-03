########################################
# Viusalise validation plots 
# 22.02.2019
#######################################

library(ggplot2)
library(caret)
library(dplyr)

op_df <- read.csv("./output/models/plsr_rfr_gpr_svmr_op.csv")

post_resample_pls_n <- postResample(op_df$pls_n_pred, op_df$n)
post_resample_pls_adf <- postResample(op_df$pls_adf_pred, op_df$adf)

post_resample_rf_n <- postResample(op_df$rf_n_pred, op_df$n)
post_resample_rf_adf <- postResample(op_df$rf_adf_pred, op_df$adf)

post_resample_gp_n <- postResample(op_df$gp_n_pred, op_df$n)
post_resample_gp_adf <- postResample(op_df$gp_adf_pred, op_df$adf)

post_resample_svm_n <- postResample(op_df$svm_n_pred, op_df$n)
post_resample_svm_adf <- postResample(op_df$svm_adf_pred, op_df$adf)

mean_n <- mean(op_df$n)
mean_adf <- mean(op_df$adf)


### OP plot for N
pls_n_plot <- ggplot(op_df, 
                     aes(x=pls_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "PLSR",
       caption=paste("R² = ", round(post_resample_pls_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_pls_n[1]/mean_n, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

rf_n_plot <- ggplot(op_df, 
                     aes(x=rf_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "RFR",
       caption=paste("R² = ", round(post_resample_rf_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_rf_n[1]/mean_n, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

gp_n_plot <- ggplot(op_df, 
                    aes(x=gp_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "GPR",
       caption=paste("R² = ", round(post_resample_gp_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_gp_n[1]/mean_n, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

svm_n_plot <- ggplot(op_df, 
                    aes(x=svm_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "SVMR",
       caption=paste("R² = ", round(post_resample_svm_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_svm_n[1]/mean_n, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

n_plots <- cowplot::plot_grid(pls_n_plot, rf_n_plot, gp_n_plot, svm_n_plot, 
                   nrow = 2, ncol = 2, align="hv")
ggpubr::annotate_figure(n_plots,
                        top = ggpubr::text_grob("N (%DM) Estimation (Model Testing)", 
                                                family="Helvetica", face = "bold", size = 17))
png(filename="./plots/n_model_validation.png", type="cairo",
    width = 3300, height = 3300, res = 300)
ggpubr::annotate_figure(n_plots,
                        top = ggpubr::text_grob("N (%DM) Estimation (Model Testing)", 
                                                family="Helvetica", face = "bold", size = 16))
dev.off()

### OP plot for ADF
pls_adf_plot <- ggplot(op_df, 
                       aes(x=pls_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "PLSR",
       caption=paste("R² = ", round(post_resample_pls_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_pls_adf[1]/mean_adf, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

rf_adf_plot <- ggplot(op_df, 
                      aes(x=rf_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "RFR",
       caption=paste("R² = ", round(post_resample_rf_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_rf_adf[1]/mean_adf, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

gp_adf_plot <- ggplot(op_df, 
                      aes(x=gp_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "GPR",
       caption=paste("R² = ", round(post_resample_gp_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_gp_adf[1]/mean_adf, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

svm_adf_plot <- ggplot(op_df, 
                       aes(x=svm_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "",
                               labels = c("Rhön", "WIZ")) +
  labs(x="Predicted", 
       y="Observed",
       title = "SVMR",
       caption=paste("R² = ", round(post_resample_svm_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_svm_adf[1]/mean_adf, 2)*100, 
                     " %", sep = "")) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=11),
        plot.caption = element_text(size=13, face="italic", hjust=1),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "top")

adf_plots <- cowplot::plot_grid(pls_adf_plot, rf_adf_plot, gp_adf_plot, svm_adf_plot, 
                   nrow = 2, ncol = 2)


ggpubr::annotate_figure(adf_plots,
                        top = ggpubr::text_grob("ADF (%DM) Estimation (Model Testing)", 
                                                family="Helvetica", face = "bold", size = 17))

png(filename="./plots/adf_model_validation.png", type="cairo",
    width = 3300, height = 3300, res = 300)
ggpubr::annotate_figure(adf_plots,
                        top = ggpubr::text_grob("ADF (%DM) Estimation (Model Testing)", 
                                                family="Helvetica", face = "bold", size = 17))
dev.off()
# Taylor diagram

library(plotrix)

png(filename="./plots/n_model_taylor.png", type="cairo",
    width = 1100, height = 1100, res = 100)
oldpar <- taylor.diagram(op_df$n, op_df$n, pch = 19, main = "N (%DM) estimation", pcex = 2)
taylor.diagram(op_df$n, op_df$pls_n_pred, add = TRUE, col="#9449d2", pch = 19, pcex = 2)
taylor.diagram(op_df$n, op_df$rf_n_pred, add = TRUE, col="#B1740F", pch = 19, pcex = 2)
taylor.diagram(op_df$n, op_df$gp_n_pred, add = TRUE, col="blue", pch = 19, pcex = 2)
taylor.diagram(op_df$n, op_df$svm_n_pred, add = TRUE, col="#08585A", pch = 19, pcex = 2)
lpos <- 1.5*sd(op_df$n)
legend(0.85, 1.0, legend=c("Observed","PLSR","RFR","GPR","SVMR"),
       pch = 19, cex = 1.5, col = c("red", "#9449d2", "#B1740F", "blue", "#08585A"))
par(oldpar)
dev.off()

png(filename="./plots/adf_model_taylor.png", type="cairo",
    width = 1100, height = 1100, res = 100)
oldpar <- taylor.diagram(op_df$adf, op_df$adf, pch = 19, main = "ADF (%DM) estimation", pcex = 2)
taylor.diagram(op_df$adf, op_df$pls_adf_pred, add = TRUE, col="#9449d2", pch = 19, pcex = 2)
taylor.diagram(op_df$adf, op_df$rf_adf_pred, add = TRUE, col="#B1740F", pch = 19, pcex = 2)
taylor.diagram(op_df$adf, op_df$gp_adf_pred, add = TRUE, col="blue", pch = 19, pcex = 2)
taylor.diagram(op_df$adf, op_df$svm_adf_pred, add = TRUE, col="#08585A", pch = 19, pcex = 2)
lpos <- 1.5*sd(op_df$adf)
legend(5.2, 5.3, legend=c("Observed","PLSR","RFR","GPR","SVMR"),
       pch = 19, cex = 1.5, col = c("red", "#9449d2", "#B1740F", "blue", "#08585A"))
par(oldpar)
dev.off()


## Residual check

op_df$pls_n_delta <- op_df$n - op_df$pls_n_pred
op_df$pls_adf_delta <- op_df$adf - op_df$pls_adf_pred

op_df$rf_n_delta <- op_df$n - op_df$rf_n_pred
op_df$rf_adf_delta <- op_df$adf - op_df$rf_adf_pred

op_df$gp_n_delta <- op_df$n - op_df$gp_n_pred
op_df$gp_adf_delta <- op_df$adf - op_df$gp_adf_pred

op_df$svm_n_delta <- op_df$n - op_df$svm_n_pred
op_df$svm_adf_delta <- op_df$adf - op_df$svm_adf_pred

### Boxplots for residuals

### Residuals vs field id
op_df$field_id <- factor(op_df$field_id, 
                         levels = c("g1", "g2", "g3", "g4", "BG", "BGL", "GH", "GHL"),
                         labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL"))

res_vs_fid_n <- ggplot(data = op_df %>% 
         dplyr::select(field_id, names(op_df)[grep("*n_delta", names(op_df))]) %>% 
         reshape2::melt(), aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid(~ field_id) +
  theme_bw(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E48F1B") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Model", 
                              labels = c("PLSR", "RFR", "GPR", "SVMR")) +
  labs(y = "Residuals  - N (%DM)",
       x = "Model",
       caption = "Residuals for N (%DM) estimation") +
  scale_x_discrete(labels = c("pls_n_delta" = "PLSR", "rf_n_delta" = "RFR",
                              "gp_n_delta" = "GPR", "svm_n_delta" = "SVMR")) + 
  theme( 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

res_vs_fid_adf <- ggplot(data = op_df %>% 
         dplyr::select(field_id, names(op_df)[grep("*adf_delta", names(op_df))]) %>% 
         reshape2::melt(), aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid(~ field_id) +
  theme_bw(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#42858C") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Model", 
                              labels = c("PLSR", "RFR", "GPR", "SVMR")) +
  labs(y = "Residuals - ADF (%DM)",
       x = "Model",
       caption = "Residuals for ADF (%DM) estimation") +
  scale_x_discrete(labels = c("pls_adf_delta" = "PLSR", "rf_adf_delta" = "RFR",
                              "gp_adf_delta" = "GPR", "svm_adf_delta" = "SVMR")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(res_vs_fid_n, res_vs_fid_adf, nrow = 2, ncol = 1)

png(filename="./plots/model_residual_n_adf.png", type="cairo",
    width = 6600, height = 3300, res = 400)
cowplot::plot_grid(res_vs_fid_n, res_vs_fid_adf, nrow = 2, ncol = 1)
dev.off()
