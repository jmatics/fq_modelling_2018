###############################################
# 19.03.2018
# Correlation between N, ADF and Transformed spectra
# JW
###############################################


# Loading required packages
rm(list=ls())

library(data.table)
library(dplyr)
library(hsdar)
library(ggplot2)
windowsFonts(Helvetica=windowsFont("Helvetica-Regular"))

# 1. Load original data frames

## Read WIZ data

wiz_df <- read.csv("./output_wiz/hs_all_nrm_df_wiz.csv", header = TRUE)
wiz_df$source <- rep("Witzenhausen", dim(wiz_df)[1])

## Read Rhön data

dsb_df <- read.csv("./output_dsb/hs_all_nrm_df_dsb.csv", header = TRUE)
dsb_df$source <- rep("Rhön", dim(dsb_df)[1])

## merging two data sets

all_df <- rbind(wiz_df, dsb_df)
all_df$doy <- factor(all_df$doy)

## Calculate N and ADF density
all_df$n_dens <- (all_df$n/100)*all_df$db
all_df$adf_dens <- (all_df$adf/100)*all_df$db

# 2. Load derivatives data frames

all_1d_df <- read.csv("./output/TransformSpectra/hs_all_1d_df.csv")
all_2d_df <- read.csv("./output/TransformSpectra/hs_all_2d_df.csv")

# 3. Load continuum removal data frames

all_shBD_df <- read.csv("./output/TransformSpectra/hs_all_shBD_df.csv")
all_1d_cr_df <- read.csv("./output/TransformSpectra/hs_all_1d_cr_df.csv")

# 4. Correaltion analysis

spec_col <- c(5:122)
n_col <- 131
adf_col <- 134
ndens_col <- 136
adfdens_col <- 137
#fb_col <- 125
#db_col <- 129


### reflectance vs N

cor_n_df <- data.frame("wv" = seq(482, 950, 4), 
                       "org_n" = all_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n,
                       "d1_n" = all_1d_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n,
                       "d2_n" = all_2d_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n,
                       "shBD_n" = all_shBD_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n,
                       "d1CR_n" = all_1d_cr_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n)

cor_n_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(sum_cor = sum(abs(correlation)))

cor_n_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(max_cor = max(abs(correlation)))


cor_n_plot <- ggplot(data = cor_n_df %>% 
         tidyr::gather(spectra, correlation, -wv),
       aes(x = wv, y = correlation, color = spectra, group = spectra)) +
  geom_line() +
  geom_point() +ylim(c(-0.7, 0.7)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "", 
                               labels = c("First derivative", "Second derivative", 
                                          "Original", "Continumm removal band depth",
                                          "FD of CR")) +
  labs(x = "Wavelength (nm)", 
       y = "Correlation coefficient with N (%DM)", 
       caption = "Correlation coefficient with spectral reflectance vs N (%DM)" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )




### reflectance vs ADF

cor_adf_df <- data.frame("wv" = seq(482, 950, 4), 
                       "org_adf" = all_df[c(spec_col, adf_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(adf) %>% .$adf,
                       "d1_adf" = all_1d_df[c(spec_col, adf_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(adf) %>% .$adf,
                       "d2_adf" = all_2d_df[c(spec_col, adf_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(adf) %>% .$adf,
                       "shBD_adf" = all_shBD_df[c(spec_col, adf_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(adf) %>% .$adf,
                       "d1CR_adf" = all_1d_cr_df[c(spec_col, adf_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(adf) %>% .$adf)

cor_adf_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(sum_cor = sum(abs(correlation)))

cor_adf_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(max_cor = max(abs(correlation)))


cor_adf_plot <- ggplot(data = cor_adf_df %>% 
         tidyr::gather(spectra, correlation, -wv),
       aes(x = wv, y = correlation, color = spectra, group = spectra)) +
  geom_line() +
  geom_point() + ylim(c(-0.7, 0.7)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "", 
                               labels = c("First derivative", "Second derivative", 
                                          "Original", "Continumm removal band depth",
                                          "FD of CR")) +
  labs(x = "Wavelength (nm)", 
       y = "Correlation coefficient with ADF (%DM)", 
       caption = "Correlation coefficient with spectral reflectance vs ADF (%DM)" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )


cowplot::plot_grid(cor_n_plot, cor_adf_plot, 
                   nrow = 1, ncol = 2, align = "hv")

### reflectance vs fb

cor_fb_df <- data.frame("wv" = seq(482, 950, 4), 
                       "org_fb" = all_df[c(spec_col, fb_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(fb) %>% .$fb,
                       "d1_fb" = all_1d_df[c(spec_col, fb_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(fb) %>% .$fb,
                       "d2_fb" = all_2d_df[c(spec_col, fb_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(fb) %>% .$fb,
                       "shBD_fb" = all_shBD_df[c(spec_col, fb_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(fb) %>% .$fb,
                       "d1CR_fb" = all_1d_cr_df[c(spec_col, fb_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(fb) %>% .$fb)


cor_fb_plot <- ggplot(data = cor_fb_df %>% 
                       tidyr::gather(spectra, correlation, -wv),
                     aes(x = wv, y = correlation, color = spectra, group = spectra)) +
  geom_line() +
  geom_point() + ylim(c(-0.8, 0.8)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "", 
                               labels = c("First derivative", "Second derivative", 
                                          "Original", "Continumm removal band depth",
                                          "FD of CR")) +
  labs(x = "Wavelength (nm)", 
       y = "Correlation coefficient with FB (t/ha)", 
       caption = "Correlation coefficient with spectral reflectance vs FB (t/ha)" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

### reflectance vs db

cor_db_df <- data.frame("wv" = seq(482, 950, 4), 
                        "org_db" = all_df[c(spec_col, db_col)] %>%
                          corrr::correlate() %>% 
                          corrr::focus(db) %>% .$db,
                        "d1_db" = all_1d_df[c(spec_col, db_col)] %>%
                          corrr::correlate() %>% 
                          corrr::focus(db) %>% .$db,
                        "d2_db" = all_2d_df[c(spec_col, db_col)] %>%
                          corrr::correlate() %>% 
                          corrr::focus(db) %>% .$db,
                        "shBD_db" = all_shBD_df[c(spec_col, db_col)] %>%
                          corrr::correlate() %>% 
                          corrr::focus(db) %>% .$db,
                        "d1CR_db" = all_1d_cr_df[c(spec_col, db_col)] %>%
                          corrr::correlate() %>% 
                          corrr::focus(db) %>% .$db)


cor_db_plot <- ggplot(data = cor_db_df %>% 
                        tidyr::gather(spectra, correlation, -wv),
                      aes(x = wv, y = correlation, color = spectra, group = spectra)) +
  geom_line() +
  geom_point() + ylim(c(-0.8, 0.8)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "", 
                               labels = c("First derivative", "Second derivative", 
                                          "Original", "Continumm removal band depth",
                                          "FD of CR")) +
  labs(x = "Wavelength (nm)", 
       y = "Correlation coefficient with DB (t/ha)", 
       caption = "Correlation coefficient with spectral reflectance vs DB (t/ha)" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(cor_fb_plot, cor_db_plot, 
                   nrow = 1, ncol = 2, align = "hv")



## Simple linear regression with N and ADF vs Reflectance
### N

all_df[c(spec_col, n_col)] %>% 
  dplyr::select(-n) %>% 
  map(~lm(all_df$n ~ .x, data = mtcars)) %>% 
  map(glance) %>% 
  do.call(rbind.data.frame, .) %>% 
  rownames_to_column %>% 
  as_tibble 

lm_n_plot <- all_1d_cr_df[c(spec_col, n_col)] %>% 
  dplyr::select(-n) %>% 
  map(~lm(all_1d_cr_df$n ~ .x, data = mtcars)) %>% 
  map(glance) %>% 
  do.call(rbind.data.frame, .) %>% 
  rownames_to_column %>% 
  as_tibble %>% 
  mutate(wv = seq(482, 950, 4)) %>%
  ggplot(aes(x = wv, y = adj.r.squared)) +
  geom_line() +
  geom_point() +
  ylim(c(0, 0.4)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  labs(x = "Wavelength (nm)", 
       y = "Adjusted R squared ~ N (%DM)", 
       caption = "Simple linear regression with spectral reflectance vs N (%DM)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )


### ADF

all_df[c(spec_col, adf_col)] %>% 
  dplyr::select(-adf) %>% 
  map(~lm(all_df$adf ~ .x, data = mtcars)) %>% 
  map(glance) %>% 
  do.call(rbind.data.frame, .) %>% 
  rownames_to_column %>% 
  as_tibble 

lm_adf_plot <- all_1d_cr_df[c(spec_col, adf_col)] %>% 
  dplyr::select(-adf) %>% 
  map(~lm(all_1d_cr_df$adf ~ .x, data = mtcars)) %>% 
  map(glance) %>% 
  do.call(rbind.data.frame, .) %>% 
  rownames_to_column %>% 
  as_tibble %>% 
  mutate(wv = seq(482, 950, 4)) %>%
  ggplot(aes(x = wv, y = adj.r.squared)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  ylim(c(0, 0.4)) +
  labs(x = "Wavelength (nm)", 
       y = "Adjusted R squared ~ ADF (%DM)", 
       caption = "Simple linear regression with spectral reflectance vs ADF (%DM)" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(lm_n_plot, lm_adf_plot, 
                   nrow = 1, ncol = 2, align = "hv")


# N % vs N Dens

cor_nd_df <- data.frame("wv" = seq(482, 950, 4), 
                       "org_nc" = all_df[c(spec_col, n_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n) %>% .$n,
                       "org_nd" = all_df[c(spec_col, ndens_col)] %>%
                         corrr::correlate() %>% 
                         corrr::focus(n_dens) %>% .$n_dens)

cor_nd_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(sum_cor = sum(abs(correlation)))

cor_nd_df %>% tidyr::gather(spectra, correlation, -wv) %>% 
  group_by(spectra) %>% 
  summarise(max_cor = max(abs(correlation)))

ggplot(data = cor_nd_df %>% 
         tidyr::gather(spectra, correlation, -wv),
       aes(x = wv, y = correlation, color = spectra, group = spectra)) +
  geom_line() +
  geom_point() +ylim(c(-0.8, 0.8)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  jcolors::scale_color_jcolors(palette = "pal3", 
                               name = "", 
                               labels = c("N (%DM)", "N density (kg/m²)")) +
  labs(x = "Wavelength (nm)", 
       y = "Correlation coefficient with N", 
       caption = "Correlation coefficient with spectral reflectance vs N" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

all_df[c(spec_col, ndens_col)] %>% 
  dplyr::select(-n_dens) %>% 
  map(~lm(all_df$adf ~ .x, data = mtcars)) %>% 
  map(glance) %>% 
  do.call(rbind.data.frame, .) %>% 
  rownames_to_column %>% 
  as_tibble %>% 
  mutate(wv = seq(482, 950, 4)) %>%
  ggplot(aes(x = wv, y = adj.r.squared)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  ylim(c(0, 0.4)) +
  labs(x = "Wavelength (nm)", 
       y = "Adjusted R squared ~ N density (kg/m²)", 
       caption = "Simple linear regression with spectral reflectance vs N density" ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

cowplot::plot_grid(lm_n_plot, lm_adf_plot, 
                   nrow = 1, ncol = 2, align = "hv")

