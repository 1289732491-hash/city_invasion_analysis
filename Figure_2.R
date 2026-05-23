library(ggplot2)
library(sf)
library(dplyr)

raw.res.data <- read.csv(file = "res_data_path")
raw.continent.shp <- read_sf(dsn = "continent_shp_path") #global land
raw.centroid.shp <- read_sf(dsn = "city_centroid_path")
raw.hotspot.data <- read.csv(file = "data_path")
controid.shp <- subset(raw.centroid.shp, ORIG_FID %in% raw.res.data$ORIG_FID)
controid.shp <- left_join(controid.shp, raw.res.data, by = "ORIG_FID")
controid.shp <- st_transform(controid.shp, crs = 4326)
continent.shp <- raw.continent.shp[, c("CONTINENT", "geometry")]
continent.shp <- st_transform(continent.shp, crs = 4326)
#plot(continent.shp)
taxa.list <- c("all","bird","mammal","herps")
my.color <- c("#7f3c86", "#b78cbe", "#efdef6","#E3EAF1","#d7f7ed","#65b894", "#007942")
for (i in 1:length(taxa.list)) {
  #i = 1
  sel.taxa <- taxa.list[i]
  sel.hotspot <- subset(raw.hotspot.data, taxa == sel.taxa)
  sel.centroid.shp <- controid.shp[,c("ORIG_FID", paste0(sel.taxa,"_count_residual"), "geometry")]
  sel.centroid.shp <- left_join(sel.centroid.shp, sel.hotspot, by = "ORIG_FID")
  colnames(sel.centroid.shp) <- c("ORIG_FID","residual", "geometry","category", "taxa")
  sel.centroid.shp$rescale <- rescale(sel.centroid.shp$residual) 
  sel.rescale <- sel.centroid.shp$rescale
  color.value <- c(max(sel.rescale), 
                   quantile(sel.rescale, 0.9), 
                   quantile(sel.rescale, 0.75), 
                   median(sel.rescale), 
                   quantile(sel.rescale, 0.25), 
                   quantile(sel.rescale, 0.1), 
                   min(sel.rescale))
  global.plot <- ggplot() + 
    #geom_sf(data = graticule,  fill = NA, size = 0.5, color = "gray80")+
    geom_sf(data = continent.shp, fill = NA, size = 0.5, color = "#808080") +
    geom_sf(data = subset(sel.centroid.shp, category %in% c("top_c", "low_a", "low_b", "low_c")), 
            aes(fill = residual, color = category, stroke  = 0.5), size = 2.5, alpha = 0.8, shape = 21) +
    geom_sf(data =subset(sel.centroid.shp, category %in% c("top_a", "top_b")), 
            aes(fill = residual, color = category, stroke  = 1), size = 2.5, alpha = 0.8, shape = 21) +
    scale_color_manual(values = c("top_a" = "#ff0505", "top_b" = "#2c5050", "top_c"= "gray80",
                                  "low_a"= "gray80", "low_b"= "gray80", "low_c"= "gray80")) + 
    scale_fill_gradientn(colours = my.color, values = color.value) + 
    #annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.4, "cm"),
    #                      height = unit(0.6, "cm"),width = unit(0.4, "cm"), style = north_arrow_minimal()) +
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          panel.border = element_rect(color = "#808080",fill = NA, linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(color = "gray70", linewidth = 0.3, linetype = "solid"),
          #title = element_text(size = 7.5, vjust=0.5),
          legend.position = "none",
          axis.text = element_text(size = unit(12, "mm"), vjust=0.5),
          axis.ticks = element_blank())
  #global.plot
  
  fl.plot <- global.plot +
    #annotation_scale(location = "bl", width_hint = 0.3, unit = "km", text_cex = 0.5) +
    coord_sf(xlim = c(-86, -80), ylim = c(24.5, 31), expand = FALSE) +
    scale_x_continuous(breaks = seq(-86, -80, by = 3)) +
    scale_y_continuous(breaks = seq(24.5, 31, by = 3))
  #fl.plot
  
  eur.plot <-  global.plot +
    #annotation_scale(location = "bl", width_hint = 0.3, unit = "km", text_cex = 0.5) +
    coord_sf(xlim = c(-11, 38), ylim = c(34.5, 61), expand = FALSE) +
    scale_x_continuous(breaks = seq(-11, 40, by = 15)) +
    scale_y_continuous(breaks = seq(34.5, 61, by = 10))
  #eur.plot
  
  aus.plot <-  global.plot +
    #annotation_scale(location = "bl", width_hint = 0.3, unit = "km", text_cex = 0.5) +
    coord_sf(xlim = c(137.5, 154), ylim = c(-39.5, -15), expand = FALSE)+
    scale_x_continuous(breaks = seq(137, 156, by = 6)) +
    scale_y_continuous(breaks = seq(-38, -15, by = 8))
  #aus.plot
  
  row_plot <- (fl.plot | eur.plot | aus.plot)
  combined <- (global.plot)/row_plot + 
    plot_annotation(tag_levels = 'A') + 
    plot_layout(heights = c(1.55, 1))
  
  ggsave(global.plot, file = paste0(output.dir, "/global_residual.png"),
         width = 12, height = 6, dpi = 600, units = "in")
  ggsave(row_plot, file = paste0(output.dir,  "/row_residual.png"),
         width = 12, height = 3, dpi = 600, units = "in")
  ggsave(fl.plot, file = paste0(output.dir,  "/fl_residual.png"),
         width = 3.5, height = 3, dpi = 600, units = "in")
  ggsave(eur.plot, file = paste0(output.dir, "/eur_residual.png"),
         width = 4.5, height = 3, dpi = 600, units = "in")
  ggsave(aus.plot, file = paste0(output.dir,  "/AUS_residual.png"),
         width = 4, height = 3, dpi = 600, units = "in")
  ggsave(combined, file = paste0(output.dir,  "/merged_residual.png"),
         width = 12, height = 10, dpi = 600, units = "in")
}
