model.plot <- function(data){
  up_limits <- ceiling(max(data$estimate + 1.96*data$se))
  low_limits <- floor(min(data$estimate - 1.96*data$se))
  ggplot(data, aes(x = estimate, y = vars, color = vars, fill = shape, shape = p_judg)) +
    geom_vline(xintercept = 0, color = 'black',linetype = "dotted") +
    geom_errorbarh(aes(xmin = estimate-1.96*se, xmax = estimate+1.96*se), 
                   height=0, position = ggstance::position_dodgev(.6),
                   size = 2) +
    geom_point(position = ggstance::position_dodgev(.6), size = 6, stroke = 2.5) +
    scale_shape_manual(values =c("sign" = 16, "no_sign" = 21)) +
    ylab('') +
    xlab('Estimate') +
    #xlab('Standardized effect size') +
    scale_x_continuous(limits = c(low_limits, up_limits), 
                       expand = c(0, 0)) +
    theme_model() +
    ggplot2::scale_color_manual(values = color.mapping) + 
    ggplot2::scale_fill_manual(values = color.mapping)
}