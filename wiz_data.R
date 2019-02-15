###############################################
# 12.02.2018
# Spectral data and N, ADF relationship (WIZ Data)
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


# 1. Reading data tables separtely

## Biomass data

bm_list <- dir(path = "./input_wiz/bm/", pattern = "*.csv", full.names = TRUE)

bm_data <- bm_list %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind)     # reduce with rbind into one dataframe


bm_data$logdb <- log(bm_data$db) # Standardise dry biomass with log transformation

### Histogram plot for dry biomass data
ggplot(bm_data, aes (x = db, fill= field_id)) + 
  geom_density(alpha = 0.4) +
  facet_grid(. ~ harvest) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_fill_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "Dry biomass (kg/m²)",
       y = "Density",
       caption = "Dry biomass data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

### Histogram plot for log transformed dry biomass data
ggplot(bm_data, aes (x = logdb, fill= field_id)) + 
  geom_density(alpha = 0.4) +
  facet_grid(. ~ harvest) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_fill_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "Log transformed dry biomass (kg/m²)",
       y = "Density",
       caption = "Log transformed dry biomass data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

## Nitrogen data

cn_data <- read.csv("./input_wiz/cn/grassland_cn.csv", header = TRUE)

### Histogram plot for N concentration data
ggplot(cn_data, aes (x = n, fill= field_id)) + 
  geom_density(alpha = 0.4) +
  facet_grid(. ~ harvest) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_fill_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "N (%) per dry matter",
       y = "Density",
       caption = "Nitrogen (N) concentration data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 0),
    legend.position = "bottom"
  )

### Histogram plot for C concentration data
ggplot(cn_data, aes (x = c, fill= field_id)) + 
  geom_density(alpha = 0.4) +
  facet_grid(. ~ harvest) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_fill_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "C (%) per dry matter",
       y = "Density",
       caption = "Carbon (C) concentration data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 0),
    legend.position = "bottom"
  )

## ADF data

adf_data <- read.csv("./input_wiz/adf/grassland_adf2.csv", header = TRUE)

### Histogram plot for ADF concentration data
ggplot(adf_data, aes (x = adf, fill= field_id)) + 
  geom_density(alpha = 0.4) +
  facet_grid(. ~ harvest) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_fill_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "ADF (%) per dry matter",
       y = "Density",
       caption = "Acid detergent fiber (ADF) concentration data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 0),
    legend.position = "bottom"
  )


## N vs ADF

ggplot(merge(adf_data, cn_data) %>% tidyr::drop_na(), 
       aes (x = n, y = adf)) +
  geom_point(aes(col = field_id), alpha = 0.8, size = 5, shape = 20) +
  stat_smooth(method=lm, se = FALSE, linetype = "twodash") +
  #facet_grid(. ~ harvest) + 
  coord_fixed(ratio = 0.15) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_color_manual(values = c("#3CA437", "#B1740F", "#D5006A", "#08585A"),
                    name = "Grassland ID", 
                    labels = c("G1", "G2", "G3", "G4")) +
  labs(x = "N (%) per dry matter",
       y = "ADF (%) per dry matter",
       caption = "Nitrogen (N) vs Acid detergent fiber (ADF) data (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )

## Hyperspectral data

hs_list <- dir(path = "./input_wiz/hs/", pattern = "*.csv", full.names = TRUE)

hs_read <- hs_list %>%
  map(read_csv) %>%    
  reduce(rbind) 

### Remove unwanted wavelength bands (450 - 478 nm & 950 - 998 nm)
hs_data <- hs_read[-c(5:12, 131:142)]

# 2. Merging and removing NAs

## hyperspectral and biomass

hs_bm_df <- merge(bm_data, hs_data) %>% tidyr::drop_na()

## hyperspectral and nitrogen

hs_cn_df <- merge(cn_data, hs_data) %>% tidyr::drop_na()

## hyperspectral and adf

hs_adf_df <- merge(adf_data, hs_data) %>% tidyr::drop_na()

## all three tables

hs_all_df <- merge(hs_data, merge(bm_data, merge(cn_data, adf_data))) %>% tidyr::drop_na()
hs_all_df$field_id <- factor(hs_all_df$field_id)
hs_all_df$harvest <- factor(hs_all_df$harvest)

# 3. Create speclib class objects and plotting

wv <- seq(482, 950, 4)
spec_df <- hs_all_df[c(5:122)] # Select only spectral data
spec_org_mat <- as.matrix(spec_df)/10000 # Convert df to a matrix
spec_nrm_mat <- t(apply(spec_org_mat, 1, normalize.vector)) # Normalise each row


## Speclib data for original reflectance
spec_org_data <- speclib(spec_org_mat, wv)
SI(spec_org_data) <- hs_all_df[c(1:4, 123:133)]
spec_org_data

## Speclib data for normalised reflectance
spec_nrm_data <- speclib(spec_nrm_mat, wv)
SI(spec_nrm_data) <- hs_all_df[c(1:4, 123:133)]
spec_nrm_data

### Convert normalised reflectance back to a dataframe
source("./spc_2df.R")
hs_all_nrm_df <- spc_2df(spec_nrm_data)
hs_all_nrm_df <- hs_all_nrm_df[c(1:4, 16:133, 5:15)] # arrange columns
names(hs_all_nrm_df) <- names(hs_all_df)

write.csv(hs_all_df, "./output_wiz/hs_all_org_df_wiz.csv", row.names = FALSE)
write.csv(hs_all_nrm_df, "./output_wiz/hs_all_nrm_df_wiz.csv", row.names = FALSE)

## Plotting mean spectral curves for Harvest level and grassland level

### First Harvest
par(mfrow=c(3,2))

plot(subset(spec_org_data, field_id == "g1" & harvest == "H1"), FUN = "mean", col = "#3CA437", 
     main = "Original Reflectance - Grassland Level (H1)", ylim=c(0,0.7))
plot(subset(spec_org_data, field_id == "g2" & harvest == "H1"), FUN = "mean", col = "#B1740F", new = FALSE)
plot(subset(spec_org_data, field_id == "g3" & harvest == "H1"), FUN = "mean", col = "#D5006A", new = FALSE)
plot(subset(spec_org_data, field_id == "g4" & harvest == "H1"), FUN = "mean", col = "#08585A", new = FALSE)
legend("topleft", legend = c("G1", "G2", "G3", "G4"),
       col = c("#3CA437", "#B1740F", "#D5006A", "#08585A"), lty=1)

par(bg="gray97")
plot(subset(spec_nrm_data, field_id == "g1" & harvest == "H1"), FUN = "mean", col = "#3CA437", 
     main = "Normalised Reflectance - Grassland Level (H1)", ylim=c(0,0.2))
plot(subset(spec_nrm_data, field_id == "g2" & harvest == "H1"), FUN = "mean", col = "#B1740F", new = FALSE)
plot(subset(spec_nrm_data, field_id == "g3" & harvest == "H1"), FUN = "mean", col = "#D5006A", new = FALSE)
plot(subset(spec_nrm_data, field_id == "g4" & harvest == "H1"), FUN = "mean", col = "#08585A", new = FALSE)
legend("topleft", legend = c("G1", "G2", "G3", "G4"),
       col = c("#3CA437", "#B1740F", "#D5006A", "#08585A"), lty=1)


### Two cuts system

plot(subset(spec_org_data, field_id == "g3" & harvest == "H1"), FUN = "mean", col = "darkgreen", 
     main = "Original Reflectance - G3", ylim=c(0,0.7))
plot(subset(spec_org_data, field_id == "g3" & harvest == "H2"), FUN = "mean", col = "lightgreen", new = FALSE)
legend("topleft", legend = c("H1", "H2"),
       col = c("darkgreen", "lightgreen"), lty=1)

par(bg="gray97")
plot(subset(spec_nrm_data, field_id == "g3" & harvest == "H1"), FUN = "mean", col = "darkgreen", 
     main = "Normalised Reflectance - G3", ylim=c(0,0.2))
plot(subset(spec_nrm_data, field_id == "g3" & harvest == "H2"), FUN = "mean", col = "lightgreen", new = FALSE)
legend("topleft", legend = c("H1", "H2"),
       col = c("darkgreen", "lightgreen"), lty=1)


### Three cuts system

plot(subset(spec_org_data, field_id == "g4" & harvest == "H1"), FUN = "mean", col = "orangered2", 
     main = "Original Reflectance - G4", ylim=c(0,0.7))
plot(subset(spec_org_data, field_id == "g4" & harvest == "H2"), FUN = "mean", col = "orange2", new = FALSE)
plot(subset(spec_org_data, field_id == "g4" & harvest == "H3"), FUN = "mean", col = "orange4", new = FALSE)
legend("topleft", legend = c("H1", "H2", "H3"),
       col = c("orangered2", "orange2", "orange4"), lty=1)


plot(subset(spec_nrm_data, field_id == "g4" & harvest == "H1"), FUN = "mean", col = "orangered2", 
     main = "Normalised Reflectance - G4", ylim=c(0,0.2))
plot(subset(spec_nrm_data, field_id == "g4" & harvest == "H2"), FUN = "mean", col = "orange2", new = FALSE)
plot(subset(spec_nrm_data, field_id == "g4" & harvest == "H3"), FUN = "mean", col = "orange4", new = FALSE)
legend("topleft", legend = c("H1", "H2", "H3"),
       col = c("orangered2", "orange2", "orange4"), lty=1)

par(mfrow=c(1,1))

# 4. Correlation with single bands


## Correlation between reflectance vs N
cor_n_df <- hs_all_nrm_df[c(5:122, 130)] %>%
  corrr::correlate() %>% 
  corrr::focus(n)

cor_n_df$wv <- wv


## Correlation between reflectance vs ADF
cor_adf_df <- hs_all_nrm_df[c(5:122, 133)] %>%
  corrr::correlate() %>% 
  corrr::focus(adf)

cor_adf_df$wv <- wv


cor_df <- merge(cor_n_df[-1], cor_adf_df[-1])
cor_df_melt <- reshape2::melt(cor_df, id = "wv")
names(cor_df_melt) <- c("Wavelength", "Quality", "Correlation")

ggplot(cor_df_melt, aes(x = Wavelength, y = Correlation, group = Quality)) +
  geom_line(aes(color = Quality))+
  geom_point(aes(color = Quality)) +
  theme_bw(base_size = 12, base_family = "Lucida") +
  scale_color_manual(values = c("#E48F1B", "#42858C"),
                    name = "Forage quality parameter", 
                    labels = c("Nitrogen (%)", "ADF (%)")) +
  labs(x = "Wavelength (nm)",
       y = "Correlation coefficient",
       caption = "Correlation coefficient with normalised spectral reflectance vs N (%) and ADF (%) (WIZ)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "bottom"
  )


# 5. Correlation with normalised difference spectral indices (NDSI)

## Create NDSI for all possible bands

nri_data <- nri(spec_nrm_data, recursive = TRUE)
nri_data

## Linear regression with NDSI vs N%
par(mfrow=c(2,1))
load("./lm_x_nri_wiz.RData")
#lm_n_nri <- lm.nri(nri_data ~ n, preddata = spec_nrm_data)
plot(lm_n_nri, coefficient = "r.squared", main = "NDSI vs N %",
     constraint = "p.value<0.05", colspace = "rgb", col = jcolors::jcolors("pal12"))
lines(c(480,950),c(480,950))

## Linear regression with NDSI vs ADF%

#lm_adf_nri <- lm.nri(nri_data ~ adf, preddata = spec_nrm_data)
plot(lm_adf_nri, coefficient = "r.squared", main = "NDSI vs ADF %",
     constraint = "p.value<0.05", colspace = "rgb", col = jcolors::jcolors("pal12"))
lines(c(480,950),c(480,950))
par(mfrow=c(1,1))

#save(lm_n_nri, lm_adf_nri, file = "./lm_x_nri_wiz.RData")
