###############################################
# 13.02.2018
# Forage qualitz modelling with PLSR
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
library(Cairo)
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
png(filename="./plots/all_n_boxplot.png", type="cairo",
    width = 6600, height = 3300, res = 600)
ggplot(all_df, aes (x = doy, y = n, fill = field_id)) + 
  geom_boxplot(alpha = 0.75) +
  theme_light(base_size = 12, base_family = "Lucida") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Grassland ID", 
                              labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL")) +
  labs(x = "Julian date (DOY)",
       y = "N (%DM) per dry matter",
       caption = "Nitrogen (N) concentration data (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )
dev.off()

### Histogram plot for N concentration data
png(filename="./plots/all_adf_boxplot.png", type="cairo",
    width = 6600, height = 3300, res = 600)
ggplot(all_df, aes (x = doy, y = adf, fill = field_id)) + 
  geom_boxplot(alpha = 0.9) +
  theme_light(base_size = 12, base_family = "Lucida") +
  jcolors::scale_fill_jcolors("pal7",
                              name = "Grassland ID", 
                              labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL")) +
  labs(x = "Julian date (DOY)",
       y = "ADF (%DM) per dry matter",
       caption = "Acid detergent fiber (ADF) concentration data (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )
dev.off()

png(filename="./plots/all_n_vs_adf_plot.png", type="cairo",
    width = 3300, height = 3300, res = 600)
ggplot(all_df, 
       aes (x = n, y = adf)) + 
  geom_point(aes(col = field_id), alpha = 0.8, size = 5, shape = 20) +
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") + 
  #facet_grid(. ~ harvest) + 
  coord_fixed(ratio = 0.15) +
  theme_light(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors("pal7",
                              name = "Grassland ID", 
                              labels = c("G1a", "G1b", "G2", "G3", "BG", "BGL", "GH", "GHL")) +
  labs(x = "N (%DM) per dry matter",
       y = "ADF (%DM) per dry matter",
       caption = "Nitrogen (N) vs Acid detergent fiber (ADF) data (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )
dev.off()


## Correlation between reflectance vs N
wv <- seq(482, 950, 4)

cor_n_df <- all_df[c(5:122, 131)] %>%
  corrr::correlate() %>% 
  corrr::focus(n)

cor_n_df$wv <- wv


## Correlation between reflectance vs ADF
cor_adf_df <- all_df[c(5:122, 134)] %>%
  corrr::correlate() %>% 
  corrr::focus(adf)

cor_adf_df$wv <- wv


cor_df <- merge(cor_n_df[-1], cor_adf_df[-1])
cor_df_melt <- reshape2::melt(cor_df, id = "wv")
names(cor_df_melt) <- c("Wavelength", "Quality", "Correlation")

png(filename="./plots/all_n_and_adf_vs_hs.png", type="cairo",
    width = 6600, height = 3300, res = 600)
ggplot(cor_df_melt, aes(x = Wavelength, y = Correlation, group = Quality)) +
  geom_line(aes(color = Quality))+
  geom_point(aes(color = Quality)) + ylim(c(-0.8, 0.8)) +
  theme_light(base_size = 12, base_family = "Helvetica") +
  scale_color_manual(values = c("#E48F1B", "#42858C"),
                     name = "Forage quality parameter", 
                     labels = c("Nitrogen (%DM)", "ADF (%DM)")) +
  labs(x = "Wavelength (nm)",
       y = "Correlation coefficient (r)",
       caption = "Correlation coefficient with normalised spectral reflectance vs N (%DM) and ADF (%DM) (all)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )
dev.off()

### HS

wv <- seq(482, 950, 4)
spec_df <- as.matrix(all_df[c(5:122)]) # Select only spectral data

spec_org_data <- speclib(spec_df, wv)
SI(spec_org_data) <- all_df[c(1:4, 123:135)]

nri_data <- nri(spec_org_data, recursive = TRUE)

#lm_n_nri <- lm.nri(nri_data ~ n, preddata = spec_org_data)
#tictoc::tic("LM_NRI")
#lm_adf_nri <- lm.nri(nri_data ~ adf, preddata = spec_org_data)
#tictoc::toc()


save(lm_n_nri, lm_adf_nri, file = "./output/lm_nri_all.RData")
png(filename="./plots/all_n_vs_ndsi.png", type="cairo",
    width = 3300, height = 3300, res = 600)
plot(lm_n_nri, coefficient = "r.squared", main = "NDSI vs N (%DM)",
     constraint = "p.value<0.05", colspace = "rgb", col = jcolors::jcolors("pal12"))
lines(c(480,950),c(480,950))
dev.off()

png(filename="./plots/all_adf_vs_ndsi.png", type="cairo",
    width = 3300, height = 3300, res = 600)
plot(lm_adf_nri, coefficient = "r.squared", main = "NDSI vs ADF (%DM)",
     constraint = "p.value<0.05", colspace = "rgb", col = jcolors::jcolors("pal12"))
lines(c(480,950),c(480,950))
dev.off()