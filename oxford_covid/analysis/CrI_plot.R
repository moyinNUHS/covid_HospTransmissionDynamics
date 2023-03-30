################################################
########### Plot credible intervals  ###########
################################################

# set working directory 
setwd('~/oxford_covid/analysis/')

# load libraries 
library(ggplot2); library(ggpubr) # plot graph

# prepare data
filelist = list.files('output/')
mcmcsummary.filelist = filelist[grep('mcmcsummary', filelist)]
mainplot.filelist = c(mcmcsummary.filelist[grep('incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02', mcmcsummary.filelist)], 
                      mcmcsummary.filelist[grep('incub3_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02', mcmcsummary.filelist)], 
                      mcmcsummary.filelist[grep('incub7_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02', mcmcsummary.filelist)],
                      mcmcsummary.filelist[grep('incub5_wardTRUE_intFALSE_binary_phase12TRUE_0.002_0.02', mcmcsummary.filelist)])

get.plot.dat <- function(infected.population, filelist = mainplot.filelist) {
  
  filelist.pop = filelist[grep(infected.population, filelist)]
  
  df.list = lapply(filelist.pop, function(filename) {
    
    load(paste0('output/', filename))
    if (length(grep('log', rownames(mcmc.summary)) > 0)) {
      summ.red = as.data.frame(mcmc.summary[-grep('log', rownames(mcmc.summary)),])
    } else {
      summ.red = as.data.frame(mcmc.summary)
    }
    summ.red = summ.red*100
    summ.red$type = substr(rownames(summ.red), 1, 1)
    summ.mean = list()
    for (var in unique(summ.red$type)){
      summ.mean[[var]] = t(quantile(summ.red$mean[which(summ.red$type == var)], prob = c(0.025, 0.5, 0.975)))
    }
    summ.df = do.call('rbind.data.frame', summ.mean)
    summ.df$var = rownames(summ.df)
    summ.df$mod.type = filename
    return(summ.df)
    
  })
  
  df = do.call('rbind.data.frame', df.list)
  df$mod.type = as.factor(df$mod.type)
  df$mod.type = factor(df$mod.type, 
                       levels = rev(c(mainplot.filelist[grep(paste0(infected.population, '_incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02'), mainplot.filelist)],
                                      mainplot.filelist[grep(paste0(infected.population, '_incub3'), mainplot.filelist)],
                                      mainplot.filelist[grep(paste0(infected.population, '_incub7'), mainplot.filelist)],
                                      mainplot.filelist[grep(paste0(infected.population, '_incub5_wardTRUE_intFALSE_binary_phase12T'), mainplot.filelist)])), 
                       labels = rev(c('Assumed 5-day incubation period',
                                      'Assumed 3-day incubation period',
                                      'Assumed 7-day incubation period',
                                      'Only including data from phases 1 and 2')))
  
  return(df)
}

plotCrI <- function(plot.dat) { 
  
  scaleFUN <- function(x) sprintf("%.2f", x) # x axis 2 decimal places
  
  xmax = max(plot.dat$`97.5%`)
  
  ggplot(plot.dat, aes(x = `50%`, y = var, group = mod.type, color = mod.type)) +
    geom_errorbarh(aes(xmin = `2.5%`, xmax=`97.5%`), position = position_dodge(width = 0.3), size=3, height=0)+
    geom_point(size = 2, colour = 'grey10', shape = 4, position = position_dodge(width = 0.3))+
    scale_y_discrete(limits = unique(rev(plot.dat$var)),
                     labels = rev(c('Background infection risk\nincluding undetected cases' ,
                                    'Infectious patients with\ncommunity-acquired SARS-CoV-2',
                                    'Infectious patients with\nhospital-acquired SARS-CoV-2',
                                    'Infectious health care worker'))) +
    scale_color_manual(values = c("#B8E2C8", '#68A691', '#99C24D', '#4dc297', '#3c735f'), name = '') +
    xlab("Additional absolute risk per day (%)")+
    ylab("")+
    scale_x_continuous(lim = c(0, xmax), breaks = seq(0, xmax, by = 0.05), labels = scaleFUN)+
    theme_minimal() + 
    theme(legend.position = 'bottom', 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 20)) + 
    guides(color = guide_legend(nrow = 2, reverse = TRUE))
}


plot.dat.pt = get.plot.dat(infected.population = 'PT')
pdf("graphs/pt_main_additive.pdf", onefile=FALSE, width = 15, height =8) 
plotCrI(plot.dat.pt)
dev.off()

plot.dat.staff = get.plot.dat(infected.population = 'STAFF')
pdf("graphs/staff_main_additive.pdf", onefile=FALSE, width = 15, height =8) 
plotCrI(plot.dat.staff)
dev.off()

# scp moyin@163.1.213.159:oxford_covid/analysis/graphs/*additive.pdf ~/Documents/nBox/COVID/HA_COVID/Oxford/manuscript/lancetID/figs


