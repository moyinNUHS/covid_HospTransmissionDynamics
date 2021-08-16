################################################
######## WRITE MODELS  AS TEXT FILES ###########
################################################

setwd('~/Documents/nBox/COVID/HA_COVID/Oxford/analysis_code/jags_commandline/')

######### PATIENTS ###########
##############################
#### NO INTERACTIONS #########
# EACH WARD NO RANDOM EFFECT #
##############################

noint_noward_pt ="
model{
    # Likelihood:
    for (i in 1:N){
      outcome[i] ~ dbern(mu[i]) 
      
      mu[i] <- a +   # intercept  
        b * foi_comm[i] +  
        c * foi_noso[i]  + 
        d * foi_staff[i]
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    # Priors:
    a ~ dnorm(0, sd_prior) T(0, ) 
    b ~ dnorm(mean_prior, sd_prior) T(0, ) 
    c ~ dnorm(mean_prior, sd_prior) T(0, ) 
    d ~ dnorm(mean_prior, sd_prior) T(0, ) 
}
"
writeLines(noint_noward_pt, con = "models/noint_noward_pt.txt")

###########################
#### NO INTERACTIONS ######
# EACH WARD RANDOM EFFECT #
###########################

noint_ward_pt ="
model {
    # Likelihood:
    for (i in 1:N){
      outcome[i] ~ dbern(mu[i]) 
      
      mu[i] <- a[admission_ward_index[i]] +   # intercept - each ward has an intercept 
        b[admission_ward_index[i]] * foi_comm[i] + # each ward has a slope 
        c[admission_ward_index[i]] * foi_noso[i]  + 
        d[admission_ward_index[i]] * foi_staff[i]
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    for (w in 1:N_ward){
      a[w] <- a0 + aprimed[w] * sigma.a
      b[w] <- b0 + bprimed[w] * sigma.b
      c[w] <- c0 + cprimed[w] * sigma.c
      d[w] <- d0 + dprimed[w] * sigma.d
      aprimed[w] ~ dnorm(0, 1) T(0,)
      bprimed[w] ~ dnorm(0, 1) T(0,)
      cprimed[w] ~ dnorm(0, 1) T(0,)
      dprimed[w] ~ dnorm(0, 1) T(0,)
    }
    
    # Priors:
    a0 ~ dnorm(0, sd_prior) T(0,)
    sigma.a ~ dunif(0, 1);
    b0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.b ~ dunif(0, 1);
    c0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.c ~ dunif(0, 1);
    d0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.d ~ dunif(0, 1);
    
}
"
writeLines(noint_ward_pt, con = "models/noint_ward_pt.txt")

############################
###### INTERACTIONS ########
#EACH WARD NO RANDOM EFFECT#
############################

int_noward_pt ="
model{
    
    # Likelihood:
    for (i in 1:N){
      
      outcome[i] ~ dbern(mu[i]) 
      
      logit(mu[i]) <- a0 + 
        a_p2 * phase2[i] + 
        a_p3 * phase3[i] +
        (b_comm + b_comm_p2 * phase2[i] + b_comm_p3 * phase3[i]) * foi_comm[i] + 
        (b_noso + b_noso_p2 * phase2[i] + b_noso_p3 * phase3[i]) * foi_noso[i] + 
        (b_staff + b_staff_p2 * phase2[i] + b_staff_p3 * phase3[i]) * foi_staff[i] 
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    # Priors:
    a0 ~ dnorm(0, sd_prior) T(0,)
    a_p2 ~ dnorm(0, sd_prior) T(0,)
    a_p3 ~ dnorm(0, sd_prior) T(0,)
    b_comm ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    
}
"
writeLines(int_noward_pt, con = "models/int_noward_pt.txt")



########## STAFF #############
##############################
#### NO INTERACTIONS #########
# EACH WARD NO RANDOM EFFECT #
##############################

noint_noward_staff ="
model{
    # Likelihood:
    for (i in 1:N){
      outcome[i] ~ dbern(mu[i]) 
      
      mu[i] <- a +   # intercept  
        b * foi_comm[i] +  
        c * foi_noso[i]  + 
        d * foi_staff[i] + 
        e * foi_comm_admit[i]
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    # Priors:
    a ~ dnorm(0, sd_prior) T(0, ) 
    b ~ dnorm(mean_prior, sd_prior) T(0, ) 
    c ~ dnorm(mean_prior, sd_prior) T(0, ) 
    d ~ dnorm(mean_prior, sd_prior) T(0, ) 
    e ~ dnorm(mean_prior, sd_prior) T(0, ) 
}
"
writeLines(noint_noward_staff, con = "models/noint_noward_staff.txt")

###########################
#### NO INTERACTIONS ######
# EACH WARD RANDOM EFFECT #
###########################

noint_ward_staff ="
model {
    # Likelihood:
    for (i in 1:N){
      outcome[i] ~ dbern(mu[i]) 
      
      mu[i] <- a[admission_ward_index[i]] +   # intercept - each ward has an intercept 
        b[admission_ward_index[i]] * foi_comm[i] + # each ward has a slope 
        c[admission_ward_index[i]] * foi_noso[i]  + 
        d[admission_ward_index[i]] * foi_staff[i] + 
        e[admission_ward_index[i]] * foi_comm_admit[i] 
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    for (w in 1:N_ward){
      a[w] <- a0 + aprimed[w] * sigma.a
      b[w] <- b0 + bprimed[w] * sigma.b
      c[w] <- c0 + cprimed[w] * sigma.c
      d[w] <- d0 + dprimed[w] * sigma.d
      e[w] <- e0 + eprimed[w] * sigma.e
      aprimed[w] ~ dnorm(0, 1) T(0,)
      bprimed[w] ~ dnorm(0, 1) T(0,)
      cprimed[w] ~ dnorm(0, 1) T(0,)
      dprimed[w] ~ dnorm(0, 1) T(0,)
      eprimed[w] ~ dnorm(0, 1) T(0,)
    }
    
    # Priors:
    a0 ~ dnorm(0, sd_prior) T(0,)
    sigma.a ~ dunif(0, 1);
    b0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.b ~ dunif(0, 1);
    c0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.c ~ dunif(0, 1);
    d0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.d ~ dunif(0, 1);
    e0 ~ dnorm(mean_prior, sd_prior) T(0,)
    sigma.e ~ dunif(0, 1);
}
"
writeLines(noint_ward_staff, con = "models/noint_ward_staff.txt")

############################
###### INTERACTIONS ########
#EACH WARD NO RANDOM EFFECT#
############################

int_noward_staff ="
model{
    
    # Likelihood:
    for (i in 1:N){
      
      outcome[i] ~ dbern(mu[i]) 
      
      logit(mu[i]) <- a0 + 
        a_p2 * phase2[i] + 
        a_p3 * phase3[i] +
        (b_comm + b_comm_p2 * phase2[i] + b_comm_p3 * phase3[i]) * foi_comm[i] + 
        (b_noso + b_noso_p2 * phase2[i] + b_noso_p3 * phase3[i]) * foi_noso[i] + 
        (b_staff + b_staff_p2 * phase2[i] + b_staff_p3 * phase3[i]) * foi_staff[i] + 
        (b_comm_admit + b_comm_admit_p2 * phase2[i] + b_comm_admit_p3 * phase3[i]) * foi_staff[i] 
      
      loglike[i] <- dbin(outcome[i], mu[i], 1) # For WAIC computation
    }
    
    # Priors:
    a0 ~ dnorm(0, sd_prior) T(0,)
    a_p2 ~ dnorm(0, sd_prior) T(0,)
    a_p3 ~ dnorm(0, sd_prior) T(0,)
    b_comm ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_admit ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_admit_p2 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_noso_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_staff_p3 ~ dnorm(mean_prior, sd_prior) T(0,)
    b_comm_admit_p3 ~ dnorm(mean_prior, sd_prior) T(0,)

}
"
writeLines(int_noward_staff, con = "models/int_noward_staff.txt")



