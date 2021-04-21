#=======================================#
#       Oxford JR COVID19 Data          #
#       Nosocomial transmission         #
#             GLM models                #
#=======================================#

#clean environment 
rm(list = ls())

# load libraries 
library(gtsummary)
library(dbplyr)
library(knitr)
library(stringr)
library(lubridate)

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
mod.staff.multi5 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (likely community-acquired)` +
                        `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat5, family = 'binomial')
staff.multi5 = tbl_regression(mod.staff.multi5, exponentiate = TRUE)

mod.staff.multi3 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (likely community-acquired)` +
                        `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat3, family = 'binomial')
staff.multi3 = tbl_regression(mod.staff.multi3, exponentiate = TRUE)

mod.staff.multi7 = glm(outcome ~ Age + Gender + Role + `Infectious patients on the same ward (likely community-acquired)` +
                        `Infectious patients on the same ward (likely hospital-acquired)` + `Infectious staff on the same ward` + Hospital +
                        `Type of ward` + Phase + splines::ns(`Calendar day`, 2), data = staff.dat7, family = 'binomial')
staff.multi7 = tbl_regression(mod.staff.multi7, exponentiate = TRUE)
