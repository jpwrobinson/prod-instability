#!/bin/env Rscript

#Read in species and gear from command line argument
args <- commandArgs(trailingOnly = TRUE)
focal.sp<-args[1] ## is target group name
threshold<-as.numeric(args[2])  ## is 5 or 10 landings per species per stratum per month

library(dplyr); library(tidyr); library(mgcv)
source('../scaling_function.R')

## Data load - filter to fixed trap gear
if(focal.gear=='FIXS'){
load(file='../data/CAS_landings_targets_trapCPUE.Rdata')
cpue <- cpue %>% filter(GEAR==focal.gear)
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
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    mutate(Ncatch = length(cpue)) %>%
    filter(Ncatch >= threshold) %>%
    summarise(cpue=sd(cpue)/mean(cpue)*100) %>% 
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    summarise(cpue=mean(cpue)) %>% 
    filter(!is.na(cpue) & cpue > 0) %>%
    arrange(DATE.ym)

### Examining temporal patterns in CV-CPUE
## Response = cpue (kg/trap or kg/set/hour)
## Fixed = Year, month, DMI, ENSO, boat effort
## Random = time by landings stratum


### scale data for modelling 
focal <- cpue %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal <- scaler(focal, ID=c('CAS.name', 'ISLAND', 'DATE.ym', 'stratum', 'cpue', 'time'))

## set up different knot values for looping models
yrs<-length(unique(focal$year))
knots=c(5, round(yrs/2), yrs, yrs*2)


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

  save(mgam, focal, file=paste('CVcpue', focal.sp,'_',threshold, '_', knots[i], '.Rdata', sep=''))

}



