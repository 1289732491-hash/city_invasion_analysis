library(circlize)
library(dplyr)
library(magrittr)

raw.data <- read.csv(file = "data_path" )
sel.taxa <- "all"
link.data <- raw.data %>% subset(., taxa == sel.taxa)
link.data <- link.data[,-which(colnames(link.data) == "taxa")]

mycolor <- c(Africa = "#440154FF", Asia = "#31688EFF", Europe = "#21908CFF", Oceania = "#FDE725FF",
             "LAC" = "#35B779FF", "North America"  = "#8FD744FF")
order <- c("Europe", "Northern America", "Asia", "Africa", "LAC","Oceania")

png(filename = "output_path", width = 11, height = 11, units = "cm", res = 600)
adj_matrix <- link.data %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = "drop") %>% 
  tidyr::spread(key = target, value = value, fill = 0)  
adj_matrix <- as.matrix(adj_matrix[, -1]) 
rownames(adj_matrix) <- link.data$source[!duplicated(link.data$source)]  

circos.clear()
circos.par(start.degree = 90, gap.degree = 5, track.margin = c(0.01, 0.01))

chordDiagram(adj_matrix, 
             order = order,
             directional = 1, 
             direction.type = c("arrows", "diffHeight"),
             diffHeight = 0.05,
             transparency = 0.5,
             annotationTrack = "grid",
             link.arr.type = "big.arrow",
             grid.col = mycolor,
             annotationTrackHeight = c(0.05))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sector.name = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  theta = mean(xlim)
  circos.text(x = theta,
              y = 2,
              labels = sector.name,
              facing = "bending",   
              niceFacing = TRUE,
              cex = 1)
}, bg.border = NA)
dev.off()

