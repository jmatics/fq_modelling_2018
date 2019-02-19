#####################################################################################################
# Function to plot "observed vs predicted" and "predicted vs residuals" plots for calibrated ML model
# Script by Jayan Wijesingha (jayan.wijesingha@uni-kassel.de)
# 2019-02-10
#####################################################################################################

obs_pred_plot <- function(model, df, estimators, target, plot = TRUE, title = FALSE){
  op_df <- data.frame("field_id" = df$field_id,
                      "fp_id" = df$fp_id,
                      "source" = df$source,
                      "Observed" = df[, target],
                      "Predicted" = predict(model , df[, estimators]),
                      "Residuals" = df[, target] - predict(model , df[, estimators]))
  valid_out <- postResample(op_df$Predicted, op_df$Observed)
    opPlot <- ggplot(op_df, 
                     aes(x=Predicted, y=Observed)) +
      geom_point(size = 2, alpha  = 0.8, aes(col=source))  + 
      stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
      geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, 
                  linetype = "twodash", color = "black") +
      ggthemes::theme_few(base_family = "Helvetica") + 
      coord_fixed(ratio = 1) +
      jcolors::scale_color_jcolors(palette = "pal3", name = "Location") +
      labs(x="Predicted", 
           y="Observed",
           caption=paste("R.Sq = ", round(valid_out[2], 2), ",   ",
                         "rRMSEP = ", round(abs(valid_out[1]/mean(op_df$Observed)), 2)*100, 
                         " %", "\n", "Predicted vs observed plot", sep = "")) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=13),
            legend.title = element_text(size=12, face="bold"),
            legend.text = element_text(size=11),
            plot.caption=element_text(size=11, face="italic", hjust=1),
            legend.position = "top")
    
    prPlot <- ggplot(op_df, 
                     aes(x=Predicted, y=Residuals, fill=source)) +
      #geom_point()  + 
      geom_label(label=op_df$fp_id, size=3, color="white", check_overlap = T) +
      geom_abline(mapping = NULL, data = NULL, slope = 0, intercept = 0, 
                  linetype = "twodash", color = "black") +
      ggthemes::theme_few(base_family = "Helvetica") + 
      coord_fixed(ratio = 1) +
      jcolors::scale_fill_jcolors(palette = "pal3", name = "Location") +
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
    outPlot <- ggpubr::annotate_figure(bothPlots,
                            top = ggpubr::text_grob(title, family="Helvetica", face = "bold", size = 16))
    
  print(valid_out)
  return(outPlot)
}
