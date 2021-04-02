## generalised additive models 

#clean environment 
rm(list = ls())

# set working directory where the data are stored 
setwd('~/Documents/nBox/COVID/HA COVID/Oxford JR/')

require(mgcv)
require(tidymv)
require(lubridate)
library(ggplot2)
theme_set(theme_minimal())
library(survival)
library(pammtools)
library(mgcViz)
library(ggpubr)

# load data 
load('metadata/pt.df190221.Rdata')
load('metadata/staff.df190221.Rdata')

pt.dat = pt.df[which(pt.df$atrisk_incub5 == 1 & pt.df$admitted == 1), ]
pt.dat = pt.dat %>% select(infection_day_incub5_outcome, day.of.stay, date, pt_infectpa_binary_incub5_comm, pt_infectpa_binary_incub5_noso, staff_infectpa_binary_incub5, 
                           age, gender, ethnic.group, hospital.code, ward_type, phase)
pt.dat$calendar_day = yday(as.Date(pt.dat$date))

staff.dat = staff.df[which(staff.df$atrisk_incub5 == 1), ]
staff.dat = staff.dat %>% select(infection_day_incub5_outcome, date, pt_infectpa_binary_incub5_comm, pt_infectpa_binary_incub5_noso, 
                                 staff_infectpa_binary_incub5, age, gender, ward_type, role, hospital.code, phase)
staff.dat$calendar_day = yday(as.Date(staff.dat$date))

font.size = 15

# functions 
# get data from gam model fits for  plots 
get.plot.dat <- function(mod, var.no) {
  
  g = getViz(mod)
  dat = plot(sm(g, var.no))$data$fit
  dat$var = paste0(as.character(mod$call)[2], as.character(var.no))
  
  dat
}

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
# k = 7
# pt.gam.mod.onlycal <- gam(infection_day_incub5_outcome ~ 
#                             s(calendar_day, k=k), 
#                           family = binomial(link = 'logit'), 
#                           data = pt.dat)
# save(pt.gam.mod.onlycal, file = 'analysis code/pt.gam.mod.onlycal_190221.Rdata')

load('analysis code/pt.gam.mod.onlycal_190221.Rdata')
# pt.gam.mod <- gam(infection_day_incub5_outcome ~ 
#                     s(calendar_day, k=k) + 
#                     s(day.of.stay, k=k) +
#                     s(pt_infectpa_binary_incub5_comm, k=k) + 
#                     s(pt_infectpa_binary_incub5_noso, k=k) + 
#                     s(staff_infectpa_binary_incub5, k=k) + 
#                     age +
#                     gender +
#                     ward_type + 
#                     ethnic.group + 
#                     hospital.code + 
#                     phase,
#                   family = binomial(link = 'logit'), 
#                   data = pt.dat)
# save(pt.gam.mod, file = 'analysis code/pt.gam.mod_190221.Rdata')

load('analysis code/pt.gam.mod_190221.Rdata')

cal.only = get.plot.dat(mod = pt.gam.mod.onlycal, var.no = 1)
cal = get.plot.dat(mod = pt.gam.mod, var.no = 1)
pt.comm = get.plot.dat(mod = pt.gam.mod, var.no = 3)
pt.noso = get.plot.dat(mod = pt.gam.mod, var.no = 4)
staff = get.plot.dat(mod = pt.gam.mod, var.no = 5) 

cal.plot.dat = rbind.data.frame(cal.only, cal)
cal.plot.dat$lower = cal.plot.dat$y - cal.plot.dat$se
cal.plot.dat$upper = cal.plot.dat$y + cal.plot.dat$se

foi.plot.dat = rbind.data.frame(pt.comm, pt.noso, staff)
foi.plot.dat$lower = foi.plot.dat$y - foi.plot.dat$se
foi.plot.dat$upper = foi.plot.dat$y + foi.plot.dat$se


# get calendar day plot 
cal.plot = ggplot(cal.plot.dat, aes(x = x, y = y, group = var, color = var, linetype = var)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = var),  color = NA, alpha = 0.3) + 
  scale_x_continuous(breaks= c(1, lubridate::yday(as.Date('2020-02-01')),
                               lubridate::yday(as.Date('2020-03-01')),
                               lubridate::yday(as.Date('2020-04-01')),
                               lubridate::yday(as.Date('2020-05-01')),
                               lubridate::yday(as.Date('2020-06-01')),
                               lubridate::yday(as.Date('2020-07-01')),
                               lubridate::yday(as.Date('2020-08-01')),
                               lubridate::yday(as.Date('2020-09-01')),
                               lubridate::yday(as.Date('2020-10-01'))), 
                     labels = c('January','February', 'March', 
                                'April', 'May', 'June', 
                                'July', 'August', 'September', 
                                'October'),
                     limits = c(61, 250))+
  scale_y_continuous(limits = c(-2, 20), breaks = seq(0, 20, by = 5))+
  scale_linetype_manual(values = c('solid', 'dashed'), guide = 'none')+
  scale_color_manual(values = c('grey20', 'grey80'),
                       name = '', 
                       labels = c('Accounted for demographics and forces of infection', 
                                  'Unadjusted analysis')) + 
  scale_fill_manual(values = c('grey20', 'grey80'), guide = 'none') + 
  ylab('Partial effect of calendar days on daily\nrisk of nosocomial SARS-CoV-2 infection') + 
  xlab('') + 
  theme_minimal() + 
  theme(text = element_text(size = font.size), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.5, 0.8)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


# get FOI plot 
foi.plot = ggplot(foi.plot.dat, aes(x = x, y = y, group = var, color = var)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = var),  color = NA, alpha = 0.3) + 
  scale_y_continuous(limits = c(-1, 5)) + 
  scale_x_continuous(limits = c(0, 4)) +
  scale_color_discrete(name = 'Types of infectious individual', 
                       labels = c('Patients with likely community-acquired COVID-19', 
                                  'Patients with potentially hospital-acquired COVID-19',
                                  'Health care staff')) + 
  scale_fill_discrete(guide = 'none') + 
  ylab('Partial effect of infectious individuals on daily\nrisk of nosocomial SARS-CoV-2 infection') + 
  xlab('') + 
  theme_minimal() + 
  theme(text = element_text(size = 15), 
        legend.position = c(0.5, 0.8)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggarrange(cal.plot, foi.plot, ncol = 2)


###################################################
#### STAFF
###################################################

# MODEL 
# k = 7
# staff.gam.mod.onlycal <- gam(infection_day_incub5_outcome ~ 
#                             s(calendar_day, k=k), 
#                           family = binomial(link = 'logit'), 
#                           data = staff.dat)
# 
# save(staff.gam.mod.onlycal, file = 'analysis code/staff.gam.mod.onlycal_190221.Rdata')

load('analysis code/staff.gam.mod.onlycal_190221.Rdata')

# staff.gam.mod <- gam(infection_day_incub5_outcome ~ 
#                        s(calendar_day, k = k) + 
#                        s(pt_infectpa_binary_incub5_comm, k = k) + 
#                        s(pt_infectpa_binary_incub5_noso, k = k) + 
#                        s(staff_infectpa_binary_incub5, k = k) +
#                        age +
#                        gender +
#                        ward_type + 
#                        role + 
#                        hospital.code + 
#                        phase, 
#                      family=binomial(link = 'logit'), 
#                      data=dat.select)
# save(staff.gam.mod, file = 'analysis code/staff.gam.mod_190221.Rdata')

load('analysis code/staff.gam.mod_190221.Rdata')

cal.only = get.plot.dat(mod = staff.gam.mod.onlycal, var.no = 1)
cal = get.plot.dat(mod = staff.gam.mod, var.no = 1)
pt.comm = get.plot.dat(mod = staff.gam.mod, var.no = 3)
pt.noso = get.plot.dat(mod = staff.gam.mod, var.no = 4)
staff = get.plot.dat(mod = staff.gam.mod, var.no = 5) 

cal.plot.dat = rbind.data.frame(cal.only, cal)
cal.plot.dat$lower = cal.plot.dat$y - cal.plot.dat$se
cal.plot.dat$upper = cal.plot.dat$y + cal.plot.dat$se

foi.plot.dat = rbind.data.frame(pt.comm, pt.noso, staff)
foi.plot.dat$lower = foi.plot.dat$y - foi.plot.dat$se
foi.plot.dat$upper = foi.plot.dat$y + foi.plot.dat$se

cal.plot = ggplot(cal.plot.dat, aes(x = x, y = y, group = var, color = var, linetype = var)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = var),  color = NA, alpha = 0.3) + 
  scale_x_continuous(breaks= c(1, lubridate::yday(as.Date('2020-02-01')),
                               lubridate::yday(as.Date('2020-03-01')),
                               lubridate::yday(as.Date('2020-04-01')),
                               lubridate::yday(as.Date('2020-05-01')),
                               lubridate::yday(as.Date('2020-06-01')),
                               lubridate::yday(as.Date('2020-07-01')),
                               lubridate::yday(as.Date('2020-08-01')),
                               lubridate::yday(as.Date('2020-09-01')),
                               lubridate::yday(as.Date('2020-10-01'))), 
                     labels = c('January','February', 'March', 
                                'April', 'May', 'June', 
                                'July', 'August', 'September', 
                                'October'),
                     limits = c(61, 250))+
  scale_y_continuous(limits = c(-3, 20), breaks = seq(0, 20, by = 5))+
  scale_linetype_manual(values = c('solid', 'dashed'), guide = 'none')+
  scale_color_manual(values = c('grey20', 'grey80'),
                     name = '', 
                     labels = c('Analysis accounted for demographics and forces of infection', 
                                'Unadjusted analysis')) + 
  scale_fill_manual(values = c('grey20', 'grey80'), guide = 'none') + 
  ylab('Partial effect of calendar days on daily\nrisk of nosocomial SARS-CoV-2 infection') + 
  xlab('') + 
  theme_minimal() + 
  theme(text = element_text(size = font.size), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.5, 0.8)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


# get FOI plot 
foi.plot = ggplot(foi.plot.dat, aes(x = x, y = y, group = var, color = var)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = var),  color = NA, alpha = 0.3) + 
  scale_y_continuous(limits = c(-1, 5)) + 
  scale_x_continuous(limits = c(0, 4)) +
  scale_color_discrete(name = 'Types of infectious individual', 
                       labels = c('Patients with likely community-acquired COVID-19', 
                                  'Patients with potentially hospital-acquired COVID-19',
                                  'Health care staff')) + 
  scale_fill_discrete(guide = 'none') + 
  ylab('Partial effect of infectious individuals on daily\nrisk of nosocomial SARS-CoV-2 infection') + 
  xlab('') + 
  theme_minimal() + 
  theme(text = element_text(size = 15), 
        legend.position = c(0.5, 0.8)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggarrange(cal.plot, foi.plot, ncol = 2)


######## plot daily risk (day 1 -20) on 1sy day of month for every month on probability scale 
# load('analysis code/pt.gam.mod_190221.Rdata')
# pred_mod_pt = predict_gam(pt.gam.mod)
# save(pred_mod_pt, file = 'analysis code/pred_mod_pt190221.Rdata')
load('analysis code/pred_mod_pt190221.Rdata')
pred_mod = pred_mod_pt 

### PATIENT 
pred_mod = pred_mod[which(pred_mod$day.of.stay <= 20), ]
pred_mod$probs = boot::inv.logit(pred_mod$fit)
pred_mod$date = as.Date(round(pred_mod$calendar_day), origin = "2020-01-01")
pred_mod$month = as.factor(lubridate::month(pred_mod$date))
pred_mod = pred_mod[! pred_mod$month %in% c(1, 10),]
pred_mod$day = lubridate::day(pred_mod$date)
df = as.data.frame(pred_mod)
df = df[which(df$month != 10 | df$month != 1),]

mean(df$probs[which(df$month == 4)])*100
round(quantile(df$probs[which(df$month == 4)], probs = c(0.025, 0.5, 0.975))*100, 3)
round(mean(df$probs[which(df$month == 4)])*100)

### STAFF 
pred_mod$probs = boot::inv.logit(pred_mod$fit)
pred_mod$date = as.Date(round(pred_mod$calendar_day), origin = "2020-01-01")
pred_mod$month = as.factor(lubridate::month(pred_mod$date))
pred_mod = pred_mod[! pred_mod$month %in% c(1, 10),]
pred_mod$day = lubridate::day(pred_mod$date)
df = as.data.frame(pred_mod)

p = ggplot(data = df, aes(x = day.of.stay, y = probs, group = month, color = month)) +
  geom_smooth(method = lm, se = F) + 
  scale_color_discrete(labels = c('Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'), name = 'Month') +
  ylab('Daily probability of being tested positive for\nSARS-CoV-2 while hospitalised') + 
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.005)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  xlab('Day of stay') + 
  theme_minimal() + 
  theme(legend.position = 'bottom',
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        text = element_text(size = 15))

p

