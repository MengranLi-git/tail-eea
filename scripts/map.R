build_grouped_map <- function(data, model_name,
                              value_col = "AR_50",
                              alpha_col = "AR_50_alpha",
                              use_jitter_alpha = TRUE) {
  
  q_val <- data[[value_col]]
  alpha_val <- data[[alpha_col]]
  
  q_breaks <- quantile(q_val, probs = seq(0, 1, 0.2), na.rm = TRUE)
  if (use_jitter_alpha || any(duplicated(quantile(alpha_val, probs = seq(0, 1, 0.2), na.rm = TRUE)))) {
    alpha_val <- jitter(alpha_val, factor = 0.01)
  }
  alpha_breaks <- quantile(alpha_val, probs = seq(0, 1, 0.2), na.rm = TRUE)
  
  dplyr::mutate(data,
                model = model_name,
                q_cat = cut(q_val, breaks = q_breaks, labels = FALSE, include.lowest = TRUE),
                alpha_cat = as.character(cut(alpha_val, breaks = alpha_breaks, labels = FALSE, include.lowest = TRUE))
  )
}

plot_facet_ar_map <- function(data_combined,
                              title = "Attribution Ratio (Grouped)",
                              facet_order = c("MGPD", "HW", "eFCM")) {
  
  # facet order
  data_combined$model <- factor(data_combined$model, levels = facet_order)
  
  # blue to red
  quantile_gradient_colors <- c("#2c7fb8", "#99cfe2", "#ffffcc", "#fca082", "#f03b20")
  
  # alpha 
  alpha_mapping <- c("1" = 1.0, "2" = 0.8, "3" = 0.6, "4" = 0.4, "5" = 0.2)
  
  # legend
  theme_legend <- theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(20, 'points'),
    legend.position = "right",
    legend.direction = "vertical"
  )
  
  # main plot
  p <- ggplot(data = europe) +
    geom_sf(fill = NA) +
    geom_point(data = data_combined,
               aes(x = lon, y = lat,
                   color = q_cat,
                   alpha = alpha_cat),
               shape = 15, size = 3) +
    
    # color
    scale_color_gradientn(
      colors = quantile_gradient_colors,
      name = "AR Quantile",
      limits = c(1, 5),
      breaks = 1:5,
      labels = c("Very Low", "Low", "Mid", "High", "Very High"),
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 0.6,
        barheight = 10
      )
    ) +
    
    # alpha legend
    scale_alpha_manual(
      values = alpha_mapping,
      guide = "none"
    ) +
    scale_x_continuous(breaks = seq(-10, 40, by = 10), expand = c(0, 0))+
    coord_sf(xlim = c(-10, 40), ylim = c(36.5, 58.5), expand = FALSE) +
    scale_x_continuous(breaks = seq(-5, 40, by = 10),
                       labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(breaks = seq(40, 56, by = 5),
                       labels = function(y) paste0(y, "°N"))+
    facet_wrap(~model, ncol = 3) +
    theme_bw() + theme_legend +
    ggtitle(title)
  
  return(p)
}


theme_common <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(20, 'points'),
    legend.position = "right",
    legend.direction = "vertical"
  )


load("script/euro_map.Rdata")

map_mgpd <- build_grouped_map(map_AR_MGPD, model_name = "MGPD")
map_hw   <- build_grouped_map(map_AR_HW, model_name = "HW")
map_fcm  <- build_grouped_map(map_AR_eFCM, model_name = "eFCM")

all_map <- dplyr::bind_rows(map_fcm, map_hw, map_mgpd)

qqpoint = cf$coord[c(102, 128), ]

europe_plot <- plot_facet_ar_map(
  data_combined = all_map,
  title = "(a) 50-year Attribution Ratio (Europe)"
) + geom_point(data = qqpoint[1:2,],
               aes(x = lon, y = lat),
               color = "black", fill = "yellow",
               shape = 21, size = 3, stroke = 0)


library(rnaturalearth)
library(rnaturalearthdata)

usa_map_sf <- ne_states(country = "United States of America", returnclass = "sf")

usa_plot <- ggplot(grid_long_AR50) +
  geom_sf(data = usa_map_sf, fill = NA, color = "gray80", size = 0.2) +  
  geom_point(aes(x = lon, y = lat, color = mean, alpha = alpha),
             shape = 15, size = 2) +
  color_manual +
  alpha_theme +
  labs(color = "AR") +
  facet_wrap(~ method, ncol = 3) +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE) +
  scale_x_continuous(breaks = seq(-120, -70, by = 15),
                     labels = function(x) paste0(abs(x), "°W")) +
  scale_y_continuous(breaks = seq(30, 50, by = 10),
                     labels = function(y) paste0(y, "°N"))+
  guides(alpha = "none") +
  ggtitle("(b) 50-year return level (USA)") +
  theme_common


europe_plot_adjusted <- europe_plot +
  theme(
    legend.position.inside = c(1.1, 12.7),  # ：x = 1.1 right，y = 0.7  top
    legend.justification = "left"
  )

ggarrange(europe_plot_adjusted, usa_plot, nrow = 2)

setwd("E:/R/EEA")
ggsave(filename="poster_map.pdf", width = 10, height = 6)
