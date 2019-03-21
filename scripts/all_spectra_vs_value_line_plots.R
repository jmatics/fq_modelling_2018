spec_n_plot <- ggplot(data = all_df[c(5:122, 131)] %>% 
         tidyr::gather(wv, ref, -n) %>%
         mutate(wv = as.numeric(substr(as.character(wv), 4, 6))), 
       aes(x = wv, y = ref, color = n, group = n)) +
  geom_line() +
  labs(x = "Wavelength (nm)", 
       y = "Normalised spectral reflectance", 
       caption = "Normalised spectral reflectance and N (%DM)") +
  scale_color_viridis(
    option = "D",
    limits = c(0, 4),
    name = "N (%DM)") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "right"
  )

spec_adf_plot <- ggplot(data = all_df[c(5:122, 134)] %>% 
         tidyr::gather(wv, ref, -adf) %>%
         mutate(wv = as.numeric(substr(as.character(wv), 4, 6))), 
                aes(x = wv, y = ref, color = adf, group = adf)) +
  geom_line() +
  labs(x = "Wavelength (nm)", 
       y = "Normalised spectral reflectance", 
       caption = "Normalised spectral reflectance and ADF (%DM)") +
  scale_color_viridis(
    option = "B",
    limits = c(20, 40),
    name = "ADF (%DM)") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "right"
  )


cowplot::plot_grid(spec_n_plot, spec_adf_plot, 
                   nrow = 1, ncol = 2, align = "hv")


spec_fb_plot <- ggplot(data = all_df[c(5:122, 125)] %>% 
                         rownames_to_column %>%
                         tidyr::gather(wv, ref, -fb, -rowname) %>%
                         mutate(wv = as.numeric(substr(as.character(wv), 4, 6))), 
                       aes(x = wv, y = ref,  color = fb, group = rowname)) +
  geom_line() +
  labs(x = "Wavelength (nm)", 
       y = "Normalised spectral reflectance", 
       caption = "Normalised spectral reflectance and fresh biomass (kg/m²)") +
  scale_color_viridis(
    option = "D",
    limits = c(0, 4),
    name = "Fresh biomass (kg/m²)") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "right"
  )

spec_db_plot <- ggplot(data = all_df[c(5:122, 129)] %>% 
                         rownames_to_column %>%
                         tidyr::gather(wv, ref, -db, -rowname) %>%
                         mutate(wv = as.numeric(substr(as.character(wv), 4, 6))), 
                       aes(x = wv, y = ref,  color = db, group = rowname)) +
  geom_line() +
  labs(x = "Wavelength (nm)", 
       y = "Normalised spectral reflectance", 
       caption = "Normalised spectral reflectance and dry biomass (kg/m²)")+
  scale_color_viridis(
    option = "B",
    limits = c(0, 0.8),
    name = "Dry biomass (kg/m²)") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "right"
  )


cowplot::plot_grid(spec_fb_plot, spec_db_plot, 
                   nrow = 1, ncol = 2, align = "hv")


ggplot(data = all_df[c(5:122, 129)] %>% 
         rownames_to_column %>%
         tidyr::gather(wv, ref, -db, -rowname) %>%
         mutate(wv = as.numeric(substr(as.character(wv), 4, 6))), 
       aes(x = wv, y = ref,  color = db, group = rowname)) +
  geom_line() +
  labs(x = "Wavelength (nm)", 
       y = "Normalised spectral reflectance", 
       caption = "Normalised spectral reflectance and dry biomass (kg/m²)")+
  scale_color_viridis(
    option = "B",
    name = "Dry biomass (kg/m²)") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 11, face = "italic", hjust = 1),
    legend.position = "right"
  )
