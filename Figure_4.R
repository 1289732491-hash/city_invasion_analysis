######this script is used to plot model result on three category of color#########
library(dplyr)
library(ggplot2)
library(patchwork)

model.path <- "model_path"
source(file = "function_path")
model.data <- read.csv(file = "data_path")

levels <- c("Intercept", "Seaports & airports(n)", "GDP", "Trade", "Migrant", "HMI", 
            "Biodiversity", "Mean tavg", "Mean prec", "Island", "Sample effort")
model.data$vars <- factor(model.data$vars, levels = rev(levels))
model.data <- subset(model.data, vars != "Intercept")
colnames(model.data) <- c("vars", "taxa", "formula", "estimate", "se", "z_value", "p_value")
model.data <-  model.data %>%
  mutate (shape = case_when(
    p_value <= 0.05 ~ vars,
    p_value >0.05 ~ "white"
  ))
color.mapping <- c("Road density" = "#007942", "Seaports & airports(n)" = "#007942", "GDP" = "#007942", 
                   "Trade" = "#007942", "Migrant" = "#007942", "HMI"="#7f3c86", "Built-up surface" = "#7f3c86", 
                   "Biodiversity" = "#BD5B4B", "Tavg" ="#beb78c", "Prec" ="#beb78c", 
                   "Island" ="#6DB7BD", "Sample effort" ="#6DB7BD","white" = "white")
#row 1
p1.data <-  subset(model.data, taxa == "All groups" & formula == 1)
p2.data <-  subset(model.data, taxa == "All groups" & formula == 2)
p3.data <-  subset(model.data, taxa == "All groups" & formula == 3)
row1 <- model.plot(p1.data) + model.plot(p2.data) + model.plot(p3.data)
#row 2
p4.data <-  subset(model.data, taxa == "Aves" & formula == 1)
p5.data <-  subset(model.data, taxa == "Aves" & formula == 2)
p6.data <-  subset(model.data, taxa == "Aves" & formula == 3)
row2 <- model.plot(p4.data) + model.plot(p5.data) + model.plot(p6.data)
#row 3
p7.data <-  subset(model.data, taxa == "Mammalia" & formula == 1)
p8.data <-  subset(model.data, taxa == "Mammalia" & formula == 2)
p9.data <-  subset(model.data, taxa == "Mammalia" & formula == 3)
row3 <- model.plot(p7.data) + model.plot(p8.data) + model.plot(p9.data)
#row 4
p10.data <-  subset(model.data, taxa == "Herpetofauna" & formula == 1)
p11.data <-  subset(model.data, taxa == "Herpetofauna" & formula == 2)
p12.data <-  subset(model.data, taxa == "Herpetofauna" & formula == 3)
row4 <- model.plot(p10.data) + model.plot(p11.data) + model.plot(p12.data)
  
final_plot <- row1 / row2 / row3/ row4 
final_plot + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold"))
ggsave(filename = "plot_path",
       dpi = 600, width = 18, height = 16)



