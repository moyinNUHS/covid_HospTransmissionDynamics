###########################################
########### Plot Trace plots  #############
###########################################

# set working directory 
setwd('~/oxford_covid/analysis/')

# load libraries 
library(MCMCvis)                                   # summaries of jags output 
library(ggmcmc); library(ggplot2); library(ggpubr) # plot graph

theme_set(theme_minimal())

# function to plot trace plots
plot.trace <- function(mod.fit.ggs){
  
  a = mod.fit.ggs[which(mod.fit.ggs$Parameter == 'a[1]'),]
  b = mod.fit.ggs[which(mod.fit.ggs$Parameter == 'b[1]'),]
  c = mod.fit.ggs[which(mod.fit.ggs$Parameter == 'c[1]'),]
  d = mod.fit.ggs[which(mod.fit.ggs$Parameter == 'd[1]'),]
  
  plot.a = ggs_traceplot(a) + labs(y = 'Value') 
  plot.b = ggs_traceplot(b) + labs(y = 'Value') 
  plot.c = ggs_traceplot(c) + labs(y = 'Value') 
  plot.d = ggs_traceplot(d) + labs(y = 'Value') 
  
  ggarrange(plot.a, plot.b, plot.c, plot.d, ncol = 2, nrow = 2, common.legend = T)
  
}

load('output/PT_incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02_mcmcggs.Rdata')
pdf("graphs/pt_main_trace.pdf", onefile=FALSE) 
plot.trace(mcmc.ggs)
dev.off()

load('output/PT_incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02_mcmcggs.Rdata')
pdf("graphs/staff_main_trace.pdf", onefile=FALSE) 
plot.trace(mcmc.ggs)
dev.off()

