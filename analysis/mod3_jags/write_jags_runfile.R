#####################################################
######## WRITE RUN COMMANDS AS TEXT FILES ###########
#####################################################

# set working directory 
setwd('~/Documents/nBox/COVID/HA_COVID/Oxford/')

# clean environment 
rm(list = ls())

# function to write script 
write.runfile <- function(modName, datName, params.to.monitor){
  
  home_dir = "~/oxford_covid/analysis/"
  
  # directories in oxford server
  ssh = "ssh" 
  ip = "moyin@163.1.213.159"
  screen = paste0("screen -S jags_", datName, "_", Sys.Date())
  set.wd = paste0("cd ", home_dir, "output_raw")
  run.jags = "jags"
  model = paste0('model in "', home_dir, 'models/', modName, '.txt"')
  data = paste0('data in "', home_dir, 'data/', datName, '_data.R"')
  compile = "compile, nchains(3)"
  parameter = paste0('parameters in "', home_dir, 'data/', datName, '_init.R"')
  init = "initialize"
  burnin = "update 2000"
  monitor = paste0("monitor ", params.to.monitor, ', thin (10)')
  run = "update 10000"
  save = paste0("coda * , stem(", datName, ")")
  space = " "
  
  file = paste0(c(
    paste(ssh, ip),
    screen,
    set.wd,
    run.jags,
    model, 
    data, 
    compile,
    parameter,
    init,
    burnin,
    monitor,
    run,
    save, 
    space
  ))
  
  writeLines(file, con = paste0("analysis_code/jags_commandline/runfiles/", datName, "_runfile.txt"))
  
}

# write scripts 
dat.list.full = list.files('analysis_code/jags_commandline/data/')
dat.list = unique(substr(dat.list.full, 1, nchar(dat.list.full)-7))
for (dat in dat.list){
  
  ward = ifelse(substr(unlist(strsplit(dat, "ward"))[2], 1, 1) == 'T', '', 'no')
  int = ifelse(substr(unlist(strsplit(dat, "int"))[2], 1, 1) == 'T', '', 'no')
  
  if (ward == "no" & int == "no"){
    
    params.to.monitor = c('a', 'b', 'c', 'd')
    
  } else if (ward == "" & int == "no"){
    
    params.to.monitor = c("a", "b", "c", "d"
                          #'aprimed', 'bprimed', 'cprimed', 'dprimed', 
                          #'a0', 'b0', 'c0', 'd0', 
                          #'sigma.a', 'sigma.b', 'sigma.c', 'sigma.d'
                          ) 
    
  } else if (ward == "no" & int == ""){
    params.to.monitor = c(#'a0', 'a_p2', 'a_p3', 'b_comm', 'b_noso', 'b_staff', 
                          #'b_comm_p2', 'b_noso_p2', 'b_staff_p2',' b_comm_p3', 
                          #'b_noso_p3', 'b_staff_p3'
                          )
  }
  
  mod = paste0(int, "int_", ward, "ward")
  
  write.runfile(modName = mod, datName = dat, params.to.monitor = c(params.to.monitor, 'loglike'))
  
}


