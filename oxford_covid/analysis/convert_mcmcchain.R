###########################################################
############## CONVERT RAW OUTPUT TO MCMC.LIST ############ 
###########################################################

# set working directory 
setwd('~/oxford_covid/analysis/')

# load libraries 
library(coda)                                      # read jags output
library(MCMCvis)                                   # summaries of jags output 
library(vroom)                                     # read delim more quickly 
library(ggmcmc)

# function to read all output
read.output <- function(mod){
  
  dir = 'output_raw/'
  index.file = paste0(dir, mod, 'index.txt')
  
  # check for NAs 
  print(paste0('Checking missing data: ', mod, " index file ongoing....."))
  index.df = vroom(index.file, delim = ' ')
  missing = unlist(apply(index.df[,2:ncol(index.df)], 2, anyNA))
  if (any(missing) == T) {
    print(paste0("Missing data found..."))
    index.df.complete = index.df[complete.cases(index.df),]
  } else {
    print(paste0("No missing data found..."))
  }
  
  chains = list()
  for (i in 1:3){ # for each chain  
    
    chain.file = paste0(dir, mod, 'chain', i, '.txt')
    
    # check for NAs 
    chain.df = vroom(chain.file, delim = '  ')
    print(paste0('Checking missing data: ', mod, " ", i, " out of 3 chains ongoing....."))
    missing = unlist(apply(chain.df[,2:ncol(chain.df)], 2, anyNA))
    
    if (any(missing) == T) {
      print(paste0("Missing data found..."))
      chain.df.complete = chain.df[complete.cases(chain.df),]
    } else {
      print(paste0("No missing data found..."))
    }
    
    # read coda
    print(paste0('Reading raw data: ', mod, " ", i, " out of 3 chains ongoing....."))
    
    #start = 1
    #end = unlist(index.df[min(grep('log', unlist(index.df[, 1] )))-1, 3])
    
    chains[[i]] = read.coda(chain.file, index.file, quiet = T)
  }
  
  print(paste0('Saving mcmc.list: ', mod, "....."))
  mcmc.chains = as.mcmc.list(chains) # in the format of mcmc list for the MCMCvis package 
  save(mcmc.chains, file = paste0('output/', mod, '_mcmclist.Rdata'))
 
  mcmc.summary = MCMCsummary(mcmc.chains)
  save(mcmc.summary, file = paste0('output/', mod, '_mcmcsummary.Rdata'))
  
  print(paste0('Saving ggs: ', mod, "....."))
  mcmc.ggs = ggs(mcmc.chains) # in the format of ggs for the ggmcmc package
  save(mcmc.ggs, file = paste0('output/', mod, '_mcmcggs.Rdata'))
  
}

# load output files 
mod.list = c('STAFF_incub3_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02'
             #, 
             #'STAFF_incub5_wardFALSE_intFALSE_binary_phase12FALSE_0.002_0.02',
             # 'STAFF_incub5_wardFALSE_intTRUE_binary_phase12FALSE_0.002_0.02',
             # 'STAFF_incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.01_0.1',
             # 'STAFF_incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02',
             # 'STAFF_incub5_wardTRUE_intFALSE_binary_phase12TRUE_0.002_0.02',
             # 'STAFF_incub5_wardTRUE_intFALSE_scaled_phase12FALSE_0.002_0.02',
             # 'STAFF_incub7_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02'
             )
lapply(mod.list, read.output) #convert

# check R hats and effective chains 
mcmcsummary.filenames = list.files('output/')[intersect(grep('STAFF', list.files('output/')), grep('mcmcsummary', list.files('output/')))]
check_Rhat_effn = list()
for (file in mcmcsummary.filenames){
  load(paste0('output/', file))
  check_Rhat_effn[[file]] = summary(mcmc.summary)
}
save(check_Rhat_effn, file = paste0('output/check_Rhat_effn_staff.Rdata'))

