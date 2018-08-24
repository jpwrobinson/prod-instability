 
## Script for estimating lower and upper CPUE limits through time

library(tidyverse)

## Data load - trap landings
load(file='data/cleaned/CAS_landings_targets_trapCPUE.Rdata')

## cap outliers by species group
sp<-unique(cpue$CAS.name)
for(i in 1:length(sp)){
  one_col<-cpue$cpue[which(cpue$CAS.name == sp[i])]
  a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
  one_col[which(one_col > a)]<-a
  cpue$cpue[which(cpue$CAS.name == sp[i])]<-one_col
}


threshold=5
sp.filter<-c('Cordonier', 'Other trap fish', 'Capitaine')

## Cordonier = siganid
## Other trap fish = mixed species
## Capitaine = lethrinid

# subset to sites and species
## subset to predictor dataframes
cpue <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
          CAS.name %in% sp.filter &
          year != 2017 & 
          year > 1993  &
          GEAR == 'FIXS' 
          # !is.na(NO.GEAR) 
          )  %>% 
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    mutate(Ncatch = length(cpue)) %>%
    filter(Ncatch >= threshold) %>% ungroup()

## measure quantile of CPUE over time
sp<-unique(cpue$CAS.name)

quantile.func<-function(df, species, quantile, message=TRUE){
  # subset dataframe by species
  one_col<-df %>% filter(CAS.name == species) %>% select(cpue)
  # print sample size
  if(message==TRUE){print(paste0('N CPUE', ' for ', species, ' = ', dim(one_col)[1]))}
  # extract quantile
  a<-as.vector(round(quantile(one_col$cpue, c(quantile), na.rm = T),2))
  return(a)
}

## Estimate lower quantile of each target species group
cord.low<-ddply(cpue, .(year), .fun=quantile.func, species= 'Cordonier', quantile= 0.1)
otf.low<-ddply(cpue, .(year), .fun=quantile.func, species= 'Other trap fish', quantile= 0.1)
cap.low<-ddply(cpue, .(year), .fun=quantile.func, species= 'Capitaine', quantile= 0.1)

## Estimate upper quantile of each target species group
cord.high<-ddply(cpue, .(year), .fun=quantile.func, species= 'Cordonier', quantile= 0.9)
otf.high<-ddply(cpue, .(year), .fun=quantile.func, species= 'Other trap fish', quantile= 0.9)
cap.high<-ddply(cpue, .(year), .fun=quantile.func, species= 'Capitaine', quantile= 0.9)


pdf(file='figures/cas/models/CPUE_quantile_bootstrap.pdf', height=7, width=12)

### bootstrap quantiles for 100 CPUEs randomly drawn per year, repeated for 100 replicates

##--------##--------##--------##--------##--------##--------##--------
##--------##--------## Cordonier - Siganid ##--------##--------##--------
##--------##--------##--------##--------##--------##--------##--------

### LOWER
yr<-unique(cpue$year)
cord.low.boot<-matrix(nrow=100, ncol=length(yr))
start=0; boot=100
species<-'Cordonier'

repeat{

  low.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 100 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      id<-sample(1:dim(dat)[1], 100)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species=species, quantile=0.1, message=FALSE)
      low.boot[,i]<-thresh
}
  start<-start+1

  # fill quantile bootstrap into main df
  cord.low.boot[start,]<-low.boot

  if (start==boot) {break}

}
  cord.low.boot<-data.frame(cord.low.boot)
  cl<-gather(cord.low.boot, year, threshold)
  cl$year <- as.numeric(str_replace_all(cl$year, 'X', ''))
  cl$year.num<-rep(yr, each=100)

### UPPER
yr<-unique(cpue$year)
cord.high.boot<-matrix(nrow=100, ncol=length(yr))
start=0; boot=100
species<-'Cordonier'

repeat{

  high.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 100 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      id<-sample(1:dim(dat)[1], 100)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species=species, quantile=0.9, message=FALSE)
      high.boot[,i]<-thresh
}
  start<-start+1

  # fill quantile bootstrap into main df
  cord.high.boot[start,]<-high.boot

  if (start==boot) {break}

}
  cord.high.boot<-data.frame(cord.high.boot)
  ch<-gather(cord.high.boot, year, threshold)
  ch$year <- as.numeric(str_replace_all(ch$year, 'X', ''))
  ch$year.num<-rep(yr, each=100)

##--------##--------##--------##--------##--------##--------##--------
##--------##--------## OTF - mixed species ##--------##--------##--------
##--------##--------##--------##--------##--------##--------##--------

## LOWER
yr<-unique(cpue$year)
otf.low.boot<-matrix(nrow=100, ncol=length(yr))
start=0; boot=100
species='Other trap fish'

repeat{

  low.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 100 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      id<-sample(1:dim(dat)[1], 100)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species= species, quantile=0.1, message=FALSE)
      low.boot[,i]<-thresh
}
  start<-start+1

  # fill quantile bootstrap into main df
  otf.low.boot[start,]<-low.boot

  if (start==boot) {break}

}
  otf.low.boot<-data.frame(otf.low.boot)
  ol<-gather(otf.low.boot, year, threshold)
  ol$year <- as.numeric(str_replace_all(ol$year, 'X', ''))
  ol$year.num<-rep(yr, each=100)


## UPPER
yr<-unique(cpue$year)
otf.high.boot<-matrix(nrow=100, ncol=length(yr))
start=0; boot=100
species='Other trap fish'

repeat{

  high.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 100 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      id<-sample(1:dim(dat)[1], 100)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species= species, quantile=0.9, message=FALSE)
      high.boot[,i]<-thresh
}
  start<-start+1

  # fill quantile bootstrap into main df
  otf.high.boot[start,]<-high.boot

  if (start==boot) {break}

}
  otf.high.boot<-data.frame(otf.high.boot)
  oh<-gather(otf.high.boot, year, threshold)
  oh$year <- as.numeric(str_replace_all(oh$year, 'X', ''))
  oh$year.num<-rep(yr, each=100)


##--------##--------##--------##--------##--------##--------##--------
##--------##--------## capitaine - lethrinid ##--------##--------##--------
##--------##--------##--------##--------##--------##--------##--------

## LOWER
yr<-unique(cpue$year)
cap.low.boot<-matrix(nrow=50, ncol=length(yr))
start=0; boot=50
species='Capitaine'

repeat{

  low.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 50 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      if(dim(dat)[1]<50){low.boot[,i]<-NA} else {
      id<-sample(1:dim(dat)[1], 50)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species= species, quantile=0.1, message=FALSE)
      low.boot[,i]<-thresh 
      }
}
  start<-start+1

  # fill quantile bootstrap into main df
  cap.low.boot[start,]<-low.boot

  if (start==boot) {break}

}
  cap.low.boot<-data.frame(cap.low.boot)
  capl<-gather(cap.low.boot, year, threshold)
  capl$year <- as.numeric(str_replace_all(capl$year, 'X', ''))
  capl$year.num<-rep(yr, each=50)


## UPPER
yr<-unique(cpue$year)
cap.high.boot<-matrix(nrow=50, ncol=length(yr))
start=0; boot=50
species='Capitaine'

repeat{

  high.boot<-matrix(nrow=1, ncol=length(yr))
  
  for (i in 1:length(yr)){
      ## subset to 1 year, sample 50 CPUEs, estimate quantile, save  
      dat<-cpue[cpue$year == yr[i] & cpue$CAS.name==species,]
      if(dim(dat)[1]<50){high.boot[,i]<-NA} else {
      id<-sample(1:dim(dat)[1], 50)
      focal<-dat[id,]
      thresh<-quantile.func(focal, species= species, quantile=0.9, message=FALSE)
      high.boot[,i]<-thresh 
      }
}
  start<-start+1

  # fill quantile bootstrap into main df
  cap.high.boot[start,]<-high.boot

  if (start==boot) {break}

}
  cap.high.boot<-data.frame(cap.high.boot)
  caph<-gather(cap.high.boot, year, threshold)
  caph$year <- as.numeric(str_replace_all(caph$year, 'X', ''))
  caph$year.num<-rep(yr, each=50)

## save all boots for plotting Supplementary Figure 
save(cl, ch, ol, oh, capl, caph, file='CAS_quantile_thresholds.Rdata')

