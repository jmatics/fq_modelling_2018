########################################
# Viusalise validation plots 
# 22.02.2019
#######################################

library(ggplot2)
library(caret)

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
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_pls_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_pls_n[1]/mean_n, 2)*100, 
                     " %", "\n", "PLSR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

rf_n_plot <- ggplot(op_df, 
                     aes(x=rf_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_rf_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_rf_n[1]/mean_n, 2)*100, 
                     " %", "\n", "RFR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

gp_n_plot <- ggplot(op_df, 
                    aes(x=gp_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_gp_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_gp_n[1]/mean_n, 2)*100, 
                     " %", "\n", "GPR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

svm_n_plot <- ggplot(op_df, 
                    aes(x=svm_n_pred, y=n)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_svm_n[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_svm_n[1]/mean_n, 2)*100, 
                     " %", "\n", "SVMR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

cowplot::plot_grid(pls_n_plot, rf_n_plot, gp_n_plot, svm_n_plot, 
                   nrow = 2, ncol = 2)

### OP plot for ADF
pls_adf_plot <- ggplot(op_df, 
                       aes(x=pls_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_pls_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_pls_adf[1]/mean_adf, 2)*100, 
                     " %", "\n", "PLSR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

rf_adf_plot <- ggplot(op_df, 
                      aes(x=rf_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_rf_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_rf_adf[1]/mean_adf, 2)*100, 
                     " %", "\n", "RFR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

gp_adf_plot <- ggplot(op_df, 
                      aes(x=gp_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_gp_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_gp_adf[1]/mean_adf, 2)*100, 
                     " %", "\n", "GPR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

svm_adf_plot <- ggplot(op_df, 
                       aes(x=svm_adf_pred, y=adf)) +
  geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
              linetype = "twodash", color = "black") +
  ggthemes::theme_few(base_family = "Helvetica") + 
  coord_fixed(ratio = 1) +
  jcolors::scale_color_jcolors(palette = "pal3") +
  labs(x="Predicted", 
       y="Observed",
       caption=paste("R² = ", round(post_resample_svm_adf[2], 2), ",   ",
                     "rRMSEP = ", round(post_resample_svm_adf[1]/mean_adf, 2)*100, 
                     " %", "\n", "SVMR", sep = "")) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=11),
        plot.caption=element_text(size=11, face="italic", hjust=1),
        legend.position = "top")

cowplot::plot_grid(pls_adf_plot, rf_adf_plot, gp_adf_plot, svm_adf_plot, 
                   nrow = 2, ncol = 2)

# Taylor diagram

library(plotrix)

oldpar <- taylor.diagram(op_df$n, op_df$n, pch = 19, main = "N (%) estimation")
taylor.diagram(op_df$n, op_df$pls_n_pred, add = TRUE, col="#9449d2", pch = 19)
taylor.diagram(op_df$n, op_df$rf_n_pred, add = TRUE, col="#B1740F", pch = 19)
taylor.diagram(op_df$n, op_df$gp_n_pred, add = TRUE, col="blue", pch = 19)
taylor.diagram(op_df$n, op_df$svm_n_pred, add = TRUE, col="#08585A", pch = 19)
lpos <- 1.5*sd(op_df$n)
legend(0.9, 1.3, legend=c("Observed","PLSR","RFR","GPR","SVMR"),
       pch = 19, col = c("red", "#9449d2", "#B1740F", "blue", "#08585A"))
par(oldpar)

oldpar <- taylor.diagram(op_df$adf, op_df$adf, pch = 19, main = "ADF (%) estimation")
taylor.diagram(op_df$adf, op_df$pls_adf_pred, add = TRUE, col="#9449d2", pch = 19)
taylor.diagram(op_df$adf, op_df$rf_adf_pred, add = TRUE, col="#B1740F", pch = 19)
taylor.diagram(op_df$adf, op_df$gp_adf_pred, add = TRUE, col="blue", pch = 19)
taylor.diagram(op_df$adf, op_df$svm_adf_pred, add = TRUE, col="#08585A", pch = 19)
lpos <- 1.5*sd(op_df$adf)
legend(5.7, 7.3, legend=c("Observed","PLSR","RFR","GPR","SVMR"),
       pch = 19, col = c("red", "#9449d2", "#B1740F", "blue", "#08585A"))
par(oldpar)
