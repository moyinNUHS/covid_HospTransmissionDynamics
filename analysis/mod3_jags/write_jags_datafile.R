################################################
######## WRITE DATA AS TEXT FILES ##############
################################################

# clean environment 
rm(list = ls())

## load data 
### for JR cluster:
setwd('~/oxford_covid/analysis/')
load('~/oxford_covid/data/metadata/pt.df190221.Rdata')
load('~/oxford_covid/data/metadata/staff.df190221.Rdata')

### for local machine 
setwd('~/Documents/nBox/COVID/HA COVID/Oxford JR/analysis code/jags_commandline/')
load('~/Documents/nBox/COVID/HA COVID/Oxford JR/metadata/pt.df190221.Rdata')
load('~/Documents/nBox/COVID/HA COVID/Oxford JR/metadata/staff.df190221.Rdata')

clean.dat.jags.commandline <- function(dat, 
                                       incub, 
                                       phase12 = F, 
                                       ward = F,
                                       int = F,
                                       infectiousness = 'binary', 
                                       mean_prior = 0.002, 
                                       sd_prior = 0.02) {
  
  # clean data for JAGS
  
  # add dummy phase variables 
  dat$phase1 = 0
  dat$phase1[which(dat$phase == 1)] = 1
  dat$phase2 = 0
  dat$phase2[which(dat$phase == 2)] = 1
  dat$phase3 = 0
  dat$phase3[which(dat$phase == 3)] = 1
  
  # cut uneeded data
  # for sensitivity analysis - excluding phase 3 
  if (phase12 == T){ dat = dat[which(dat$phase != 3),]}
  
  # get the appropriate rows and columns for the incubation period 
  if (length(grep('staff', dat$id[1])) > 0) {
    dat.incub = dat[dat[paste0('atrisk_incub', incub)] == 1,]
    pop = 'STAFF'
  } else {
    dat.incub = dat[which(dat[paste0('atrisk_incub', incub)] == 1 & dat$admitted == 1),]
    pop = 'PT'
  }
  infectiousness.col = paste0('_infectpa_', infectiousness, '_incub')
  dat.red = dat.incub[c(paste0('infection_day_incub', incub, '_outcome'), 
                        'ward',
                        'phase2', 
                        'phase3',
                        paste0('pt', infectiousness.col, incub, '_comm'),
                        paste0('pt', infectiousness.col, incub, '_noso'),
                        paste0('staff', infectiousness.col, incub))]
  
  
  # get just the rows with complete entries 
  dat.complete = dat.red[complete.cases(dat.red),]
  
  # pos.rows = which(dat.complete[, 1] == 1)
  # neg.rows = which(dat.complete[, 1] == 0)
  # dat.complete = rbind.data.frame(dat.complete[sample(pos.rows, 20), ], 
  #                                 dat.complete[sample(pos.rows, 50), ])
  
  # ge the data to dump
  N = as.numeric(nrow(dat.complete))
  outcome = as.numeric(as.character(dat.complete[, paste0('infection_day_incub', incub, '_outcome')]))
  foi_comm = dat.complete[,grep('comm', colnames(dat.complete))]
  foi_noso = dat.complete[,grep('noso', colnames(dat.complete))]
  foi_staff = dat.complete[,grep('staff', colnames(dat.complete))]
  
  if (ward == T) {admission_ward_index = as.numeric(as.factor(dat.complete$ward)); N_ward = length(unique(admission_ward_index))}
  if (int == T) {phase2 = dat.complete$phase2; phase3 = dat.complete$phase3}
  
  # dump data 
  fileName = paste0("data/", pop, "_incub", incub, "_ward", ward, "_int", int, "_", infectiousness, "_", "phase12", phase12, "_", mean_prior, "_", sd_prior)
  dat.to.save = c('N', 'outcome', 'foi_comm', 'foi_noso', 'foi_staff', 'mean_prior', 'sd_prior')
  if (ward == T & int == F) {dat.to.save = c(dat.to.save, 'admission_ward_index', 'N_ward')}
  if (ward == F & int == T) {dat.to.save = c(dat.to.save, 'phase2', 'phase3')}
  
  dump(dat.to.save, file = paste0(fileName, "_data.R"))
  
  ## save initial values 
  if (ward == T) {
    aprimed = rep(0.0001, N_ward); bprimed = rep(0.0001, N_ward); cprimed = rep(0.0001, N_ward); dprimed = rep(0.0001, N_ward); 
    a0 = 0.001; b0 = 0.001; c0 = 0.001; d0 = 0.001; sigma.a = 0.001; sigma.b = 0.001; sigma.c = 0.001; sigma.d = 0.001
    dump(c('aprimed', 'bprimed', 'cprimed', 'dprimed', 
           'a0', 'b0', 'c0', 'd0', 
           'sigma.a', 'sigma.b', 'sigma.c', 'sigma.d'), 
         file = paste0(fileName, "_init.R"))
    
  } else if (int == T) {
    a0 = 0.001; a_p2 = 0.001; a_p3 = 0.001; b_comm = 0.001; b_noso = 0.001; b_staff = 0.001; b_comm_p2 = 0.001;
    b_noso_p2 = 0.001;  b_staff_p2 = 0.001; b_comm_p3 = 0.001; b_noso_p3 = 0.001; b_staff_p3 = 0.001
    dump(c('a0', 'a_p2', 'a_p3', 
           'b_comm', 'b_noso', 'b_staff','b_comm_p2', 'b_noso_p2', 'b_staff_p2',
           'b_comm_p3', 'b_noso_p3', 'b_staff_p3'), file = paste0(fileName, "_init.R"))
    
  } else {
    a = 0.001; b = 0.001; c = 0.001; d = 0.001
    dump(c('a', 'b', 'c', 'd'), file = paste0(fileName, "_init.R"))
  }
  
}
###############################################
### Save data
###############################################

# Compare interaction and no interaction 
clean.dat.jags.commandline(dat = pt.df, incub = 5)
clean.dat.jags.commandline(dat = pt.df, incub = 5, int = T,)
clean.dat.jags.commandline(dat = staff.df, incub = 5)
clean.dat.jags.commandline(dat = staff.df, incub = 5, int = T)

# Main analysis 
clean.dat.jags.commandline(dat = pt.df, incub = 3, ward = T) 
clean.dat.jags.commandline(dat = pt.df, incub = 5, ward = T)
clean.dat.jags.commandline(dat = pt.df, incub = 7, ward = T)
clean.dat.jags.commandline(dat = pt.df, incub = 5, ward = T, phase12 = T)
clean.dat.jags.commandline(dat = pt.df, incub = 5, ward = T, infectiousness = 'scaled')
clean.dat.jags.commandline(dat = pt.df, incub = 5, ward = T, mean_prior = 0.01, sd_prior = 0.1)

clean.dat.jags.commandline(dat = staff.df, incub = 3, ward = T)
clean.dat.jags.commandline(dat = staff.df, incub = 5, ward = T)
clean.dat.jags.commandline(dat = staff.df, incub = 7, ward = T)
clean.dat.jags.commandline(dat = staff.df, incub = 5, ward = T, phase12 = T)
clean.dat.jags.commandline(dat = staff.df, incub = 5, ward = T, infectiousness = 'scaled')
clean.dat.jags.commandline(dat = staff.df, incub = 5, ward = T, mean_prior = 0.01, sd_prior = 0.1)

