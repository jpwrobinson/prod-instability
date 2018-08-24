#!/bin/env Rscript

#Read in species and gear from command line argument
args <- commandArgs(trailingOnly = TRUE)
focal.sp<-args[1]
focal.gear<-args[2]

library(dplyr); library(tidyr); library(mgcv)
source('../scaling_function.R')

if(focal.gear=='FIXS'){
load(file='../data/CAS_landings_targets_trap.Rdata')
}

if(focal.gear=='FIXA'){
load(file='../data/CAS_landings_targets_trap.Rdata')
}

## loaded dataframe = 'y'

# subset to relevant years + species
y<- y %>% group_by(year, location, month, DATE, gear.type, speciesgr) %>% 
              mutate(catches = ifelse(is.na(catches), 0, catches)) %>%
    summarise(catch = sum(catches)) %>% 
    filter(speciesgr %in% c('capitaine', 'cordonier', 'other trap fish')) %>%
    select(year, location, month, DATE, gear.type, speciesgr, catch)

# capitaine = lethrinid
# cordonier = siganid
# other trap fish = mixed species


### Examining temporal patterns in CPUE
## Response = catch (tonnes/month)
## Fixed = Year, month, DMI, ENSO, fleet size
## Random = time by landings stratum

### fit mixed effects models with full fixed structure and vary random structure
focal <- catch %>% filter(speciesgr==focal.sp)
focal$catch<-log10(focal$catch+1)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal <- scaler(focal, ID=c('speciesgr', 'ISLAND', 'DATE', 'location','gear.type', 'catch', 'log10catch', 'time'))
focal<-droplevels(focal)

# scale all exp. vars
focal <- scaler(focal, ID=c('speciesgr', 'ISLAND', 'DATE.ym', 'stratum', 'cpue', 'time'))

knots=c(5,10,20)

## Fit GAMs with different knot values for smoothers
for (i in 1:length(knots))   {
  ## date fixed + random smooth for stratum, site + year + month

  mgam <- gam(log10catch ~ 
                s(time, k=knots[i]) + ## scaled temporal covariate of interest
                s(month, bs='cc', k=12) +  ## cyclic month term
                s(dmi, bs='cr', k=knots[i]) + ## cyclic dipole mode index term
                s(benso, bs='cr', k=knots[i]) + ## enso cyclic term
                s(nboats, bs = 'cr', k=knots[i]) + ## total fleet size term
                s(time, location, k=knots[i], bs = "fs", m=1), ## smoothed random trends per site
                data = focal)

  save(mgam, focal, file=paste('catch', focal.sp,'_',focal.gear,'_', knots[i], '.Rdata', sep=''))

} 

