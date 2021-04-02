## round y axis label to 1 decimal place
scaleFUN <- function(x) {c(0, round(max(x)/0.05)*0.05)}
breakFUN <- function(x) {c(0, max(x))}

group.colors = c(atrisk_incub5_mean = '#C73E1D', pt_infectpa_binary_incub5_comm_mean = '#83BCA9', 
                 pt_infectpa_binary_incub5_noso_mean = '#F2BAC9', staff_infectpa_binary_incub5_mean = '#38618C')

labs = c('Patients with new nosocomial infections',
         'Patients with community-acquired SARS-CoV-2\non the same ward who are infectious', 
         'Patients with hospital-acquired SARS-CoV-2\non the same ward who are infectious', 
         'Staff on the same ward who are infectious')

plot_perday_nodate <- function(d) {
  
  #create dummy points to adjust y scale when y = 0 
  d.point = list()
  for (var in unique(d$variable)){
    if (all(d$value[which(d$variable == var)] == 0)) {
      d.point[[p]][[var]] = data.frame(week = as.Date('2020-01-20'), 
                                       WardName = p, 
                                       variable = var, 
                                       value = max(d$value))
    } else {
      d.point[[p]][[var]] = data.frame(week = as.Date('2020-01-20'), 
                                       WardName = p, 
                                       variable = var, 
                                       value = 0)
    }
  }
  d.point = do.call('rbind.data.frame', unlist(d.point, recursive = F))
  d.point$variable = as.factor(d.point$variable)
  d.point$variable = factor(d.point$variable, levels = d.point$variable[c(grep('risk', d.point$variable), 
                                                                          grep('comm',d.point$variable),
                                                                          grep('noso',d.point$variable),
                                                                          grep('staff',d.point$variable))])
  
  ggplot(data = d, aes(x = week, y = value, group = variable, colour = variable)) +
    geom_point(data = d.point, aes(x = week, y = value, group = variable, color = NA), show.legend = FALSE) +
    geom_line() +
    ylab('') + 
    xlab('') +
    scale_x_date(date_breaks = 'month') +
    scale_y_continuous(breaks = breakFUN, labels = scaleFUN) +
    facet_grid(rows = vars(variable), cols = vars(WardName), scale = "free_y") + 
    scale_color_manual(values = group.colors, name = '', 
                       labels = labs) +
    theme_minimal() +
    guides(alpha=FALSE, size = FALSE, 
           color=guide_legend(nrow=2,byrow=TRUE)) +
    theme(panel.spacing.y = unit(6, "mm"),
          legend.position = 'bottom', 
          axis.text.y = element_text(size = font.size),
          axis.text.x = element_blank(), 
          strip.text.y = element_blank(), 
          strip.text.x = element_blank(), 
          text = element_text(size=font.size+10), 
          plot.margin = unit(c(1,0,0,0), "cm")) 
  
}

plot_perday_yesdate <- function(d) {
  
  #create dummy points to adjust y scale when y = 0 
  d.point = list()
  for (var in unique(d$variable)){
    if (all(d$value[which(d$variable == var)] == 0)) {
      d.point[[p]][[var]] = data.frame(week = as.Date('2020-01-20'), 
                                       WardName = p, 
                                       variable = var, 
                                       value = max(d$value))
    } else {
      d.point[[p]][[var]] = data.frame(week = as.Date('2020-01-20'), 
                                       WardName = p, 
                                       variable = var, 
                                       value = 0)
    }
  }
  d.point = do.call('rbind.data.frame', unlist(d.point, recursive = F))
  d.point$variable = as.factor(d.point$variable)
  d.point$variable = factor(d.point$variable, levels = d.point$variable[c(grep('risk', d.point$variable), 
                                                                          grep('comm',d.point$variable),
                                                                          grep('noso',d.point$variable),
                                                                          grep('staff',d.point$variable))])
  
  ggplot(data = d, aes(x = week, y = value, group = variable, colour = variable)) +
    geom_point(data = d.point, aes(x = week, y = value, group = variable, color = NA), show.legend = FALSE) +
    geom_line() +
    ylab('') + 
    xlab('') +
    scale_x_date(date_breaks = 'month') +
    scale_y_continuous(breaks = breakFUN, labels = scaleFUN) +
    facet_grid(rows = vars(variable), cols = vars(WardName), scale = "free_y") +
    scale_color_manual(values = group.colors, name = '', 
                       labels = labs) +
    theme_minimal() +
    guides(alpha = FALSE, size = FALSE, 
           color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 2))) +
    theme(panel.spacing.y = unit(6, "mm"),
          legend.position = 'bottom', 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          axis.text.y = element_text(size = font.size),
          strip.text.y = element_blank(), 
          strip.text.x = element_blank(), 
          text=element_text(size = font.size+10), 
          plot.margin = unit(c(1,0,0,0), "cm"), 
          legend.key.width = unit(2,"cm"), 
          legend.text=element_text(size=font.size+11))
  
}
