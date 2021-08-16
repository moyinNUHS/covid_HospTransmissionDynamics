#=======================================#
#       Oxford JR COVID19 Data          #
#       Nosocomial transmission         #
#             GLM models                #
#=======================================#

#clean environment 
rm(list = ls())

# set working directory where the data are stored 
setwd('~/Documents/nBox/COVID/HA_COVID/Oxford/')

# load libraries 
library(gtsummary)
library(dbplyr)
library(knitr)
library(stringr)
library(lubridate)

## clean data functions
clean.pt <- function(dat.select){
  
  colnames(dat.select)[grep('outcome', colnames(dat.select))] = 'outcome'
  colnames(dat.select)[grep('age', colnames(dat.select))] = 'Age'
  colnames(dat.select)[grep('gender', colnames(dat.select))] = 'Gender'
  colnames(dat.select)[grep('ethnic', colnames(dat.select))] = 'Ethnic group'
  colnames(dat.select)[grep('comm', colnames(dat.select))] = 'Infectious patients on the same ward (community-acquired)'
  colnames(dat.select)[grep('noso', colnames(dat.select))] = 'Infectious patients on the same ward (hospital-acquired)'
  colnames(dat.select)[grep('staff', colnames(dat.select))] = 'Infectious staff on the same ward'
  colnames(dat.select)[grep('hospital.code', colnames(dat.select))] = 'Hospital'
  colnames(dat.select)[grep('ward_type', colnames(dat.select))] = 'Type of ward'
  colnames(dat.select)[grep('phase', colnames(dat.select))] = 'Phase'
  colnames(dat.select)[grep('calendar', colnames(dat.select))] = 'Calendar day'
  colnames(dat.select)[grep('day.of.stay', colnames(dat.select))] = 'Day of stay'
  
  dat.select$Gender = ifelse(dat.select$Gender == 'F', 'Female', 'Male')
  dat.select$`Ethnic group` = factor(dat.select$`Ethnic group`, levels = c('White', 'BAME', 'U'))
  levels(dat.select$`Ethnic group`) = c('White', 'BAME', 'Unknown')
  levels(dat.select$Hospital) = paste('Hospital', levels(dat.select$Hospital))
  levels(dat.select$`Type of ward`) = c('General Ward', 'ICU/ HDU')
  return(dat.select)
}

clean.staff <- function(dat.select){
  
  colnames(dat.select)[grep('outcome', colnames(dat.select))] = 'outcome'
  colnames(dat.select)[grep('age', colnames(dat.select))] = 'Age'
  colnames(dat.select)[grep('gender', colnames(dat.select))] = 'Gender'
  colnames(dat.select)[grep('_comm', colnames(dat.select))] = 'Infectious patients on the same ward (community-acquired)'
  colnames(dat.select)[grep('comm_', colnames(dat.select))] = 'Infectious cases in the community'
  colnames(dat.select)[grep('noso', colnames(dat.select))] = 'Infectious patients on the same ward (hospital-acquired)'
  colnames(dat.select)[grep('staff', colnames(dat.select))] = 'Infectious staff on the same ward'
  colnames(dat.select)[grep('hospital.code', colnames(dat.select))] = 'Hospital'
  colnames(dat.select)[grep('ward_type', colnames(dat.select))] = 'Type of ward'
  colnames(dat.select)[grep('role', colnames(dat.select))] = 'Role'
  colnames(dat.select)[grep('phase', colnames(dat.select))] = 'Phase'
  colnames(dat.select)[grep('calendar', colnames(dat.select))] = 'Calendar day'
  
  levels(dat.select$Hospital) = paste('Hospital', levels(dat.select$Hospital))
  levels(dat.select$`Type of ward`) = c('General Ward', 'ICU/ HDU')
  dat.select$Role = factor(dat.select$Role, levels = c('Doctor', 'Nurse', 'Allied health', 'Non-clinical staff'))
  return(dat.select)
}

## load data
load('metadata/pt.df190221.Rdata')
load('metadata/staff.df190221.Rdata')

pt.df$calendar_day = day(pt.df$date)
staff.df$calendar_day = day(staff.df$date)

## get data - incubation 5 days (main analysis)
dat5 = pt.df[which(pt.df$atrisk_incub5 == 1 & pt.df$admitted == 1), ]
dat.select = dat5 %>% select(infection_day_incub5_outcome, age, gender, ethnic.group, pt_infectpa_binary_incub5_comm, 
                             pt_infectpa_binary_incub5_noso, staff_infectpa_binary_incub5, hospital.code, ward_type, 
                             phase, calendar_day, day.of.stay)
pt.dat5 = clean.pt(dat.select)

dat3 = pt.df[which(pt.df$atrisk_incub3 == 1 & pt.df$admitted == 1), ]
dat.select = dat3 %>% select(infection_day_incub3_outcome, age, gender, ethnic.group, pt_infectpa_binary_incub3_comm, 
                             pt_infectpa_binary_incub3_noso, staff_infectpa_binary_incub3, hospital.code, ward_type, 
                             phase, calendar_day, day.of.stay)
pt.dat3 = clean.pt(dat.select)

dat7 = pt.df[which(pt.df$atrisk_incub7 == 1 & pt.df$admitted == 1), ]
dat.select = dat7 %>% select(infection_day_incub7_outcome, age, gender, ethnic.group, pt_infectpa_binary_incub7_comm, 
                             pt_infectpa_binary_incub7_noso, staff_infectpa_binary_incub7, hospital.code, ward_type, 
                             phase, calendar_day, day.of.stay)
pt.dat7 = clean.pt(dat.select)

## STAFF 
dat5 = staff.df[which(staff.df$atrisk_incub5 == 1), ]
dat.select = dat5 %>% select(infection_day_incub5_outcome, age, gender, role, pt_infectpa_binary_incub5_comm, pt_infectpa_binary_incub5_noso, 
                             staff_infectpa_binary_incub5, comm_infectpa, hospital.code, ward_type, phase, calendar_day)
staff.dat5 = clean.staff(dat.select)

dat3 = staff.df[which(staff.df$atrisk_incub3 == 1), ]
dat.select = dat3 %>% select(infection_day_incub3_outcome, age, gender, role, pt_infectpa_binary_incub3_comm, pt_infectpa_binary_incub3_noso, 
                              staff_infectpa_binary_incub3, comm_infectpa, hospital.code, ward_type, phase, calendar_day)
staff.dat3 = clean.staff(dat.select)

dat7 = staff.df[which(staff.df$atrisk_incub7 == 1), ]
dat.select = dat7 %>% select(infection_day_incub7_outcome, age, gender, role, pt_infectpa_binary_incub7_comm, pt_infectpa_binary_incub7_noso, 
                              staff_infectpa_binary_incub7, comm_infectpa, hospital.code, ward_type, phase, calendar_day)
staff.dat7 = clean.staff(dat.select)

###################################
#1. Univariate analysis 
## PATIENTS 
## run analysis 
pt.uni5 = tbl_uvregression(pt.dat5, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)
pt.uni3 = tbl_uvregression(pt.dat3, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)
pt.uni7 = tbl_uvregression(pt.dat7, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)

staff.uni5 = tbl_uvregression(staff.dat5, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)
staff.uni3 = tbl_uvregression(staff.dat3, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)
staff.uni7 = tbl_uvregression(staff.dat7, method = glm, y = outcome, method.args = list(family = binomial), exponentiate = TRUE)

###################################
#2. Multivaiate analysis
### PATIENT 
mod.pt.multi5 = glm(outcome ~ Age + Gender + `Ethnic group` + `Infectious patients on the same ward (likely community-acquired)` +
                     `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                     `Type of ward` +  `Day of stay` + Phase + splines::ns(`Calendar day`, 2), data = pt.dat5, family = 'binomial')
summary(mod.pt.multi5)
pt.multi5 = tbl_regression(mod.pt.multi5, exponentiate = TRUE)

mod.pt.multi3 = glm(outcome ~ Age + Gender + `Ethnic group` + `Infectious patients on the same ward (likely community-acquired)` +
                      `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                      `Type of ward` +  `Day of stay` + Phase + splines::ns(`Calendar day`, 2), data = pt.dat3, family = 'binomial')
summary(mod.pt.multi3)
pt.multi3 = tbl_regression(mod.pt.multi3, exponentiate = TRUE)

mod.pt.multi7 = glm(outcome ~ Age + Gender + `Ethnic group` + `Infectious patients on the same ward (likely community-acquired)` +
                      `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                      `Type of ward` +  `Day of stay` + Phase + splines::ns(`Calendar day`, 2), data = pt.dat7, family = 'binomial')
summary(mod.pt.multi7)
pt.multi7 = tbl_regression(mod.pt.multi7, exponentiate = TRUE)


### STAFF
mod.staff.multi5 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (community-acquired)` + 
                         `Infectious cases in the community` +
                        `Infectious patients on the same ward (hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat5, family = 'binomial')
staff.multi5 = tbl_regression(mod.staff.multi5, exponentiate = TRUE)

mod.staff.multi3 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (community-acquired)` + 
                         `Infectious cases in the community` +
                         `Infectious patients on the same ward (hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat3, family = 'binomial')
staff.multi3 = tbl_regression(mod.staff.multi3, exponentiate = TRUE)

mod.staff.multi7 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (community-acquired)` + 
                         `Infectious cases in the community` +
                         `Infectious patients on the same ward (hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat7, family = 'binomial')
staff.multi7 = tbl_regression(mod.staff.multi7, exponentiate = TRUE)
