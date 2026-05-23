library(ggplot2)
library(rstatix)
library(dplyr)
library(patchwork)
##########set plot function##########
plot.fun <- function(model){
  plot_model(model, type = "eff", terms = as.character(group)) +
    theme_bw() +
    theme(plot.title = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line( linewidth = 0.25),
          axis.title.x = element_blank(),
          #axis.title.y = element_text(color = "black",face="plain",size = 8),
          axis.title.y = element_blank(),
          #axis.text.x = element_text(color = "black",face="plain",size = 8),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = "black",face="plain",size = 8),
          panel.grid.major = element_line(color="#CCCCCC", linewidth = 0.15),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.15, fill = NA),
          panel.background = element_blank(),
          plot.margin = margin(10, 10, 10, 10))
}
##############load data##########
compare.data <- read.csv(file = "res_data_path") #change to path on your computer
colnames(compare.data) <- c("ORIG_FID", "country", "island", "advanced", "continent", 
                            "All", "Aves", "Mammal", "Herpetofauna")
levels <- c("Oceania", "Europe", "LAC", "Asia", "Africa","North America")
compare.data$continent <- factor(compare.data$continent, levels = levels)
vars.list <- c("All", "Aves", "Mammal", "Herpetofauna")
group.list <- c("continent", "island", "advanced")
comb.list <- expand.grid(vars.list, group.list)
##############plot##########
plot.list <- list()
plot.text.list <- list()
for (i in 1:nrow(comb.list)) {
  #i = 1
  var <- comb.list[i, "Var1"]
  group <-  comb.list[i, "Var2"]
  formula <- paste0(var, "~", group)
  model <- lme(as.formula(formula), random = ~ 1 | country, data = compare.data)
  p <- plot.fun(model)
  plot.list[[i]] <- p
  plot.text.list[[i]] <- p.text
}

comb.plot <- patchwork::wrap_plots(plot.list, ncol = 3, byrow = FALSE) 
ggsave(comb.plot, filename = paste0(output.dir, "comb_plot.pdf"),
       width = 20, height = 20, units = "cm", dpi = 600)
