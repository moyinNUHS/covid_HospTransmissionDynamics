#=======================================#
#       Oxford JR COVID19 Data          #
#       Nosocomial transmission         #
#             GAM models                #
#=======================================#

#clean environment 
rm(list = ls())

#load libraries 
require(mgcv)
require(tidymv)
require(lubridate)
library(survival)
library(pammtools)
library(mgcViz)


###################################################
#Aims:
#1. Determine how risk of infection change as a function of day of stay 
#2. How does risk of infection change over calendar time, and how much of this variation is explained by infection 
# pressure from staff and patients?

# assumes data frame has a variable day_of_stay which represents the day of stay the observation is from 
# (so day_of_stay==1 corresponds to the admission day, 2 is the day after admission etc)
# also assumes data frame has a variable calendar_day, an integer representation of the calendar day
###################################################

###################################################
#### PATIENTS 
###################################################

### look at partial effect of calendar day after accounting for infection pressures 
# impose no constraints about the functional relationship between risk of nosocomial infection  and infection pressure from patients and staff
k = 7

pt.gam.mod.onlycal <- gam(infection_day_incub5_outcome ~ 
                            s(calendar_day, k=k), 
                          family = binomial(link = 'logit'), 
                          data = pt.dat)


pt.gam.mod <- gam(infection_day_incub5_outcome ~ 
                    s(calendar_day, k=k) +
                    s(day.of.stay, k=k) +
                    s(pt_infectpa_binary_incub5_comm, k=k) +
                    s(pt_infectpa_binary_incub5_noso, k=k) +
                    s(staff_infectpa_binary_incub5, k=k) +
                    age +
                    gender +
                    ward_type +
                    ethnic.group +
                    hospital.code +
                    phase,
                  family = binomial(link = 'logit'),
                  data = pt.dat)

###################################################
#### STAFF
###################################################
staff.gam.mod.onlycal <- gam(infection_day_incub5_outcome ~
                               s(calendar_day, k=k),
                             family = binomial(link = 'logit'),
                             data = staff.dat)


staff.gam.mod <- gam(infection_day_incub5_outcome ~
                       s(calendar_day, k = k) +
                       s(pt_infectpa_binary_incub5_comm, k = k) +
                       s(pt_infectpa_binary_incub5_noso, k = k) +
                       s(staff_infectpa_binary_incub5, k = k) +
                       age +
                       gender +
                       ward_type +
                       role +
                       hospital.code +
                       phase,
                     family=binomial(link = 'logit'),
                     data=dat.select)

