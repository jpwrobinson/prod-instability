

## Script for fitting GAMs to UVC biomass data 
library(tidyverse)
library(mgcv)


## Data load - UVC biomass data
biom<-read.csv('data/results/biomass/UVC_temporal_meanbiom_CASgroups.csv')


# ============================================
            # 1. Cordonier = Siganid #
# =============================================
## subset to focal species and scale predictors
focal<-biom %>% filter(Species.group == 'Cordonier')
focal$logbiom<-log10(focal$biom+1)
focal$year<-scale(focal$Year)
focal$complexity<-scale(focal$complexity)
focal$macroalgae<-scale(focal$macroalgae)

focal$hard.coral<-scale(focal$hard.coral)


cord.gam <- gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +  
      s(year, region, bs = "fs", m=1, k=5),
              data = focal)


par(mfrow=c(3,2))
plot(cord.gam)
gam.check(cord.gam)
summary(cord.gam)
## 46.3% dev. explained

##------------------------------------------------------------------##
## Estimate relative deviance explained by each benthic variable ##
##------------------------------------------------------------------##
cord.relinf<-data.frame(EXP = c('year', 'complexity', 'macroalgae', 'hard.coral'), deviance.sat=NA)
dev.global<-deviance(cord.gam)

## create null model for null deviance
cord.null <- gam(logbiom ~ 1,
              data = focal)
dev.null<-deviance(cord.null)

## remove each covariate in turn, keeping smoothing params from global model
## remove time
  M_temp1<-gam(logbiom ~ #s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) +
      s(hard.coral, k = 3)
      # s(year, region, bs = "fs", m=1, k=5)
      ,  sp=cord.gam$sp[-c(1,5)], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp1)
  cord.relinf$deviance.sat[1]<-dev.temp
## remove complexity
M_temp2<-gam(logbiom ~ s(year, k=5) +
      # s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cord.gam$sp[-2], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp2)
  cord.relinf$deviance.sat[2]<-dev.temp
## remove macroalgae
M_temp3<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      # s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cord.gam$sp[-3], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp3)
  cord.relinf$deviance.sat[3]<-dev.temp
## remove hard coral
M_temp4<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      # s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cord.gam$sp[-4], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp4)
  cord.relinf$deviance.sat[4]<-dev.temp

#--------------------------------------------------------------------------------#
                  # Calculate proportion deviance explained  
#--------------------------------------------------------------------------------#
cord.relinf$deviance.exp<-(cord.relinf$deviance.sat - dev.global)/dev.null
cord.relinf$deviance.sat<-NULL
cord.relinf$species <- 'Cordonier'


# ==================================================
            # 2. Other trap fish = mixed species #
# ===================================================
## subset to focal species and scale predictors
focal<-biom %>% filter(Species.group=='Other trap fish')
focal$logbiom<-log10(focal$biom+1)
focal$year<-scale(focal$Year)
focal$complexity<-scale(focal$complexity)
focal$macroalgae<-scale(focal$macroalgae)
focal$hard.coral<-scale(focal$hard.coral)


otf.gam <- gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),
              data = focal)

par(mfrow=c(3,2))
plot(otf.gam)
gam.check(otf.gam)
summary(otf.gam)

##------------------------------------------------------------------##
## Estimate relative deviance explained by each benthic variable ##
##------------------------------------------------------------------##
otf.relinf<-data.frame(EXP = c('year', 'complexity', 'macroalgae', 'hard.coral'), deviance.sat=NA)
dev.global<-deviance(otf.gam)

## create null model for null deviance
otf.null <- gam(logbiom ~ 1,
              data = focal)
dev.null<-deviance(otf.null)

## remove each covariate in turn, keeping smoothing params from global model
## remove time
  M_temp1<-gam(logbiom ~ #s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) +
      s(hard.coral, k = 3)
      # s(year, region, bs = "fs", m=1, k=5)
      ,  sp=otf.gam$sp[-c(1,5)], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp1)
  otf.relinf$deviance.sat[1]<-dev.temp
## remove complexity
M_temp2<-gam(logbiom ~ s(year, k=5) +
      # s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=otf.gam$sp[-2], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp2)
  otf.relinf$deviance.sat[2]<-dev.temp
## remove macroalgae
M_temp3<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      # s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=otf.gam$sp[-3], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp3)
  otf.relinf$deviance.sat[3]<-dev.temp
## remove hard coral
M_temp4<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      # s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=otf.gam$sp[-4], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp4)
  otf.relinf$deviance.sat[4]<-dev.temp

#--------------------------------------------------------------------------------#
                  # Calculate proportion deviance explained  
#--------------------------------------------------------------------------------#
otf.relinf$deviance.exp<-(otf.relinf$deviance.sat - dev.global)/dev.null
otf.relinf$deviance.sat<-NULL
otf.relinf$species <- 'Other trap fish'

# ===============================================
            # Capitaine = Lethrinid #
# ===============================================
## subset to focal species and scale predictors
focal<-biom %>% filter(Species.group=='Capitaine')
focal$logbiom<-log10(focal$biom+1)
focal$year<-scale(focal$Year)
focal$complexity<-scale(focal$complexity)
focal$macroalgae<-scale(focal$macroalgae)
focal$hard.coral<-scale(focal$hard.coral)

cap.gam <- gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),
              data = focal)

par(mfrow=c(3,2))
plot(cap.gam)
gam.check(cap.gam)
summary(cap.gam)

##------------------------------------------------------------------##
## Estimate relative deviance explained by each benthic variable ##
##------------------------------------------------------------------##
cap.relinf<-data.frame(EXP = c('year', 'complexity', 'macroalgae', 'hard.coral'), deviance.sat=NA)
dev.global<-deviance(cap.gam)

## create null model for null deviance
cap.null <- gam(logbiom ~ 1,
              data = focal)
dev.null<-deviance(cap.null)

## remove each covariate in turn, keeping smoothing params from global model
## remove time
  M_temp1<-gam(logbiom ~ #s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) 
      # s(year, region, bs = "fs", m=1, k=5
      ,  sp=cap.gam$sp[-c(1,5)], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp1)
  cap.relinf$deviance.sat[1]<-dev.temp
## remove complexity
M_temp2<-gam(logbiom ~ s(year, k=5) +
      # s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cap.gam$sp[-2], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp2)
  cap.relinf$deviance.sat[2]<-dev.temp
## remove macroalgae
M_temp3<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      # s(macroalgae, k=5) + 
      s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cap.gam$sp[-3], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp3)
  cap.relinf$deviance.sat[3]<-dev.temp
## remove hard coral
M_temp4<-gam(logbiom ~ s(year, k=5) +
      s(complexity, k=5) + 
      s(macroalgae, k=5) + 
      # s(hard.coral, k=5) +
      s(year, region, bs = "fs", m=1, k=5),  sp=cap.gam$sp[-4], data=focal, family='gaussian')
  dev.temp<-deviance(M_temp4)
  cap.relinf$deviance.sat[4]<-dev.temp

#--------------------------------------------------------------------------------#
                  # Calculate proportion deviance explained  
#--------------------------------------------------------------------------------#
cap.relinf$deviance.exp<-(cap.relinf$deviance.sat - dev.global)/dev.null
cap.relinf$deviance.sat<-NULL
cap.relinf$species <- 'Capitaine'

#--------------------------------------------------------------------------------#
                  # Models finished...save outputs
#--------------------------------------------------------------------------------#

## save relinf estimates
relinf<-rbind(cord.relinf, otf.relinf, cap.relinf)
write.csv(relinf, file='GAM_relinf.csv')

## save global models
save(cord.gam, otf.gam, cap.gam, file='UVC_biom_GAMs.Rdata')