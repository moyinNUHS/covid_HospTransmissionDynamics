################################################
######## WRITE MODELS  AS TEXT FILES ###########
################################################

##############################
#### NO INTERACTIONS #########
# EACH WARD NO RANDOM EFFECT #
##############################

noint_noward = "
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
writeLines(noint_noward, con = "models/noint_noward.txt")

###########################
#### NO INTERACTIONS ######
# EACH WARD RANDOM EFFECT #
###########################

noint_ward = "
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
writeLines(noint_ward, con = "models/noint_ward.txt")

############################
###### INTERACTIONS ########
#EACH WARD NO RANDOM EFFECT#
############################

int_noward = "
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
writeLines(int_noward, con = "models/int_noward.txt")

