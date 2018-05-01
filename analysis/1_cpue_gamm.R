#!/bin/env Rscript


#Read in site from command line argument
args <- commandArgs(trailingOnly = TRUE)
focal.sp<-args[1] ## is target group name
focal.gear<-args[2] ## is focal gear (fixed or active trap)

library(dplyr); library(tidyr); library(mgcv)
source('../scaling_function.R')

## Data load. Identify CPUE variable
## FIXS = fixed trap; FIXA = active trap
if(focal.gear=='FIXS'){
load(file='../data/CAS_landings_targets_trapCPUE.Rdata')
cpue <- cpue %>% filter(GEAR==focal.gear)
}

if(focal.gear=='FIXA'){
load(file='../data/CAS_landings_targets_trapCPUE.Rdata')
cpue <- cpue %>% filter(GEAR==focal.gear & !is.na(cpue2) & is.finite(cpue2)) %>% mutate(cpue = cpue2)
}

## cap outliers by species group
sp<-unique(cpue$CAS.name)
for(i in 1:length(sp)){
  one_col<-cpue$cpue[which(cpue$CAS.name == sp[i])]
  a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
  one_col[which(one_col > a)]<-a
  cpue$cpue[which(cpue$CAS.name == sp[i])]<-one_col
}


## subset to predictor dataframe
cpue <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
          # CAS.name != 'Job' &
          year != 2017 & 
          year > 1993 
          # GEAR == 'FIXS' &
          # !is.na(NO.GEAR) 
          ) %>% 
  mutate(SITE.NO=factor(SITE.NO)) %>%
  group_by(CAS.name, survey, year, ISLAND, stratum, month, DATE.ym, SITE.NO, dmi, benso, nboats) %>% 
  summarise(cpue=mean(cpue)) %>%
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    summarise(cpue=mean(cpue))%>%
    arrange(DATE.ym)



### Examining temporal patterns in CPUE
## Response = cpue (kg/trap or kg/set/hour)
## Fixed = Year, month, DMI, ENSO, boat effort
## Random = time by landings stratum

## scale data for modelling
focal <- cpue %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal <- scaler(focal, ID=c('CAS.name', 'ISLAND', 'DATE.ym', 'stratum', 'cpue'))


# set up knot values for looping gams
yrs<-length(unique(focal$year))
knots=c(yrs/2, yrs, yrs*2)

## Fit GAMs with different knot values for smoothers
for (i in 1:length(knots))   {

  mgam <- gam(cpue ~ 
                s(time, bs='cr', k=knots[i]) + ## scaled temporal covariate of interest
                s(month, bs='cc', k=12) +  ## cyclic month term
                s(dmi, bs='cr', k=knots[i]) + ## cyclic dipole mode index term
                s(benso, bs='cr', k=knots[i]) + ## enso cyclic term
                s(nboats, bs = 'cr', k=knots[i]) + ## total boat effort term
                s(time, stratum,  k=knots[i], bs = "fs", m=1), ## smoothed random trends per strata
                data = focal, method='REML', family='Gamma')

  save(mgam, focal, file=paste('cpue', focal.sp,'_',focal.gear,'_', knots[i], '.Rdata', sep=''))

} 
