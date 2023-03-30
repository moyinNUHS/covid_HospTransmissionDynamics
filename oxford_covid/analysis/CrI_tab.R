################################################
########### Plot credible intervals  ###########
################################################

# set working directory 
setwd('~/oxford_covid/analysis/')

# prepare data
filelist = list.files('output/')
mcmcsummary.filelist = filelist[grep('mcmcsummary', filelist)]
senstab.filelist = c(mcmcsummary.filelist[grep('incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02', mcmcsummary.filelist)], 
                     mcmcsummary.filelist[grep('0.01_0.1', mcmcsummary.filelist)], 
                     mcmcsummary.filelist[grep('scaled', mcmcsummary.filelist)])

get.tab <- function(infected.population, filelist = senstab.filelist) {
  
  filelist.pop = filelist[grep(infected.population, filelist)]
  
  df.list = lapply(filelist.pop, function(filename) {
    
    load(paste0('output/', filename))
    summ.red = as.data.frame(mcmc.summary[-grep('log', rownames(mcmc.summary)),])
    summ.red = summ.red*100
    summ.red$type = substr(rownames(summ.red), 1, 1)
    summ.mean = list()
    for (var in unique(summ.red$type)){
      summ.mean[[var]] = t(quantile(summ.red$mean[which(summ.red$type == var)], prob = c(0.025, 0.5, 0.975)))
    }
    summ.df = do.call('rbind.data.frame', summ.mean)
    summ.df$var = rownames(summ.df)
    summ.df$mod.type = filename
    
    summ.df[, c('50%','2.5%','97.5%')] = apply(summ.df[, c('50%','2.5%','97.5%')], 2, function(x){round(x, 2)})
    summ.df$range = paste0('(', paste(summ.df[, '2.5%'], summ.df[, '97.5%'], sep = '-'), ')')
    summ.df = summ.df[, c('mod.type','var','50%','range')]
    
    return(summ.df)
    
  })
  
  df = do.call('rbind.data.frame', df.list)
  df$mod.type = as.factor(df$mod.type)
  df$mod.type = factor(df$mod.type, levels = c(senstab.filelist[grep('incub5_wardTRUE_intFALSE_binary_phase12FALSE_0.002_0.02', senstab.filelist)], 
                                               senstab.filelist[grep('0.01_0.1', senstab.filelist)],
                                               senstab.filelist[grep('scaled', senstab.filelist)]))
  
  
  return(df)
}

tab.pt = get.tab(infected.population = 'PT')
write.csv(tab.pt, file = '~/oxford_covid/analysis/output/sens_tab_pt.csv')

tab.staff = get.tab(infected.population = 'STAFF')
write.csv(tab.pt, file = '~/oxford_covid/analysis/output/sens_tab_staff.csv')

