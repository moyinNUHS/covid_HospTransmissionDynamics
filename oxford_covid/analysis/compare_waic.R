###########################################
############## COMPARE WAIC   #############
###########################################

# set working directory 
setwd('~/oxford_covid/analysis/')

library(loo)

# prepare data 
mcmcchain.filenames = list.files('output/')[grep('mcmclist', list.files('output/'))]
mcmcchain.wardFALSE_intFALSE = mcmcchain.filenames[grep('wardFALSE_intFALSE', mcmcchain.filenames)]
mcmcchain.wardFALSE_intTRUE = mcmcchain.filenames[grep('wardFALSE_intTRUE', mcmcchain.filenames)]

check_waic = list()
for (file in c(mcmcchain.wardFALSE_intFALSE, mcmcchain.wardFALSE_intTRUE)){
  
  load(paste0('output/', file))
  
  loglik_matrix_list = lapply(mcmc.chains, function(x){
    x[, grep('log', colnames(x))]
  })
  loglik_matrix = do.call('rbind', loglik_matrix_list)
  
  check_waic[[file]] = as.data.frame(waic(loglik_matrix, 
                            cores = getOption("mc.cores", 10)))
}
save(check_waic, file = paste0('output/check_waic.Rdata'))
