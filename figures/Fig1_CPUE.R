rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/beta-catch')
library(dplyr); library(ggplot2); library(itsadug)
## Plotting yields and effort for sharing with coauthors
add_label <-function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

load(file='data/cleaned/CAS_landings_targets_trapCPUE.Rdata')
cpuef <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
          stratum != 'La Digue' &
          # CAS.name != 'Job' &
          year != 2017 & 
          year > 1993 
          # GEAR == 'FIXS' &
          # !is.na(NO.GEAR) 
          ) %>% 
  mutate(SITE.NO=factor(SITE.NO)) %>%
  group_by(CAS.name, survey, year, ISLAND, stratum, month, DATE.ym, SITE.NO,
    dmi, benso) %>% 
  summarise(cpue=mean(cpue)) %>%
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym,
    dmi, benso) %>% 
    summarise(cpue=mean(cpue)) 

# load predictions active TRAPS
focal.gear='FIXA'
load(file='data/cleaned/CAS_landings_targets_trapCPUE.Rdata')
cpuea <- cpue %>% filter(GEAR==focal.gear & !is.na(cpue2) & is.finite(cpue2))  %>% mutate(cpue = cpue2)
# subset to sites and species
## subset to predictor dataframes
cpuea <- cpuea %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
          # stratum != 'La Digue' &
          # CAS.name != 'Job' &
          year != 2017 & 
          year > 1993 
          # GEAR == 'FIXS' &
          # !is.na(NO.GEAR) 
          ) %>% 
  group_by(CAS.name, survey, year, ISLAND, stratum, month, DATE.ym, SITE.NO,
    dmi, benso) %>% 
  summarise(cpue=mean(cpue)) %>%
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso) %>% 
    summarise(cpue=mean(cpue)) %>%
    arrange(DATE.ym)

# load predictions
y1<-read.csv('data/results/predicted/CPUE/Cordonier_FIXS_CPUE.csv')
y1$ISLAND<-cpue$ISLAND[match(y1$stratum, cpue$stratum)]
y1$col<-ifelse(y1$ISLAND=='Mahe', '#addd8e', '#238443')

y2<-read.csv('data/results/predicted/CPUE/Other trap fish_FIXS_CPUE.csv')
y2$ISLAND<-cpue$ISLAND[match(y2$stratum, cpue$stratum)]
y2$col<-ifelse(y2$ISLAND=='Mahe', '#fdae6b', '#d94801')

y3<-read.csv('data/results/predicted/CPUE/Capitaine_FIXS_CPUE.csv')
y3$ISLAND<-cpue$ISLAND[match(y3$stratum, cpue$stratum)]
y3$col<-ifelse(y3$ISLAND=='Mahe', '#bcbddc', '#6a51a3')

y4<-read.csv('data/results/predicted/CPUE/ALL_FIXS.csv')
y4$ISLAND<-cpue$ISLAND[match(y4$stratum, cpue$stratum)]
y4$col<-ifelse(y4$ISLAND=='Mahe', '#bdbdbd', '#252525')


y1.global<-read.csv('data/results/predicted/CPUE/Cordonier_FIXS_CPUE_globalpred.csv')
y2.global<-read.csv('data/results/predicted/CPUE/Other trap fish_FIXS_CPUE_globalpred.csv')
y3.global<-read.csv('data/results/predicted/CPUE/Capitaine_FIXS_CPUE_globalpred.csv')
y4.global<-read.csv('data/results/predicted/CPUE/ALL_FIXS_globalpred.csv')
  
### active traps
y1a<-read.csv('data/results/predicted/cpue/Cordonier_FIXA_CPUE.csv')
y1a$ISLAND<-cpue$ISLAND[match(y1a$stratum, cpue$stratum)]
y1a$col<-ifelse(y1a$ISLAND=='Mahe', '#addd8e', '#238443')

y2a<-read.csv('data/results/predicted/cpue/Other trap fish_FIXA_CPUE.csv')
y2a$ISLAND<-cpue$ISLAND[match(y2a$stratum, cpue$stratum)]
y2a$col<-ifelse(y2a$ISLAND=='Mahe', '#fdae6b', '#d94801')

y3a<-read.csv('data/results/predicted/cpue/Capitaine_FIXA_CPUE.csv')
y3a$ISLAND<-cpue$ISLAND[match(y3a$stratum, cpue$stratum)]
y3a$col<-ifelse(y3a$ISLAND=='Mahe', '#bcbddc', '#6a51a3')

y4a<-read.csv('data/results/predicted/cpue/ALL_FIXA.csv')
y4a$ISLAND<-cpue$ISLAND[match(y4a$stratum, cpue$stratum)]
y4a$col<-ifelse(y4a$ISLAND=='Mahe', '#bdbdbd', '#252525')

y1.globala<-read.csv('data/results/predicted/cpue/Cordonier_FIXA_CPUE_globalpred.csv')
y2.globala<-read.csv('data/results/predicted/cpue/Other trap fish_FIXA_CPUE_globalpred.csv')
y3.globala<-read.csv('data/results/predicted/cpue/Capitaine_FIXA_CPUE_globalpred.csv')
y4.globala<-read.csv('data/results/predicted/CPUE/ALL_FIXA_globalpred.csv')




pdf(file='figures/final/1_fig_cpue_strata.pdf', height=5, width=12)

## plotting info
# col.vec<-c('#d73027','#f46d43','#fdae61','#fee08b','#a6d96a','#1a9850')
cols<-c('#66c2a5','#fc8d62','#8da0cb', '#737373')
mat<-matrix(c(1,2,3,4,5,6,7,8), nrow=2, byrow=F)
# mat<-matrix(c(1,2,7,1,2,7,3,4,7,3,4,8,5,6,8,5,6,8), nrow=6, byrow=T)
layout(mat)

par(oma = c(0.5,2,0,0), mgp=c(3,0.6,0),xpd=TRUE)

par(mar=c(1.5,1.5,1.5,1.5))
## SIGANID - FIXED
focal.sp='Cordonier'
focal <- cpuef %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2

times <- seq(min(focal$time),max(focal$time), length=30) 

emptyPlot(range(times), c(1.5, 5.5), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='a', font=2, cex=1.4)
add_label(0.01, 0.08, label='Siganid (fixed)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2, at =seq(1.5, 5.5, 0.5))
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
mtext(side=2, 'CPUE', line=2, cex=1)
# mtext(side=2, expression(paste('kg trap'^-1)), line=2, cex=1)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y1[y1$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1, xpd=TRUE))
}

    with(y1.global, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[1],lty=1, lwd=3, xpd=TRUE))

# box()

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.1),
  lty=1, lwd=c(1,1,3), col=c('#addd8e', '#238443', cols[1]), cex=0.9, bty='n')

par(mar=c(1.5,1.5,1.5,1.5))
## SIGANID - ACTIVE
focal.sp='Cordonier'
focal <- cpuea %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)


axis.vec<-data.frame(labels=seq(1994, 2016, 2)) 
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=length(unique(focal$year))) 

emptyPlot(range(times), c(0.5, 2.5), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='e', font=2, cex=1.4)
add_label(0.01, 0.08, label='Siganid (active)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2)
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
mtext(side=2, 'CPUE', line=2, cex=1)
# mtext(side=2, expression(paste('kg trap'^-1,'set'^-1, 'hr'^-1)), line=2, cex=1)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y1a[y1a$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lty=5, lwd=1, xpd=TRUE))
}

    with(y1.globala, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[1],lty=5, lwd=3, xpd=TRUE))

# box()


# MIXED - FIXED
par(mar=c(1.5,1.5,1.5,1.5))
focal.sp='Other trap fish'
focal <- cpuef %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=30) 

emptyPlot(range(times), c(2, 8), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='b', font=2, cex=1.4)
add_label(0.01, 0.08, label='Mixed species (fixed)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2)
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
# mtext(side=2, expression(paste('kg trap'^-1)), line=2, cex=1)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y2[y2$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1, xpd=TRUE))
}

    with(y2.global, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[2],lty=1, lwd=3, xpd=TRUE))


# box()

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.1),
  lty=1, lwd=c(1,1,3), col=c( '#fdae6b', '#d94801', cols[2]), cex=0.9, bty='n')


# MIXED - ACTIVE
par(mar=c(1.5,1.5,1.5,1.5))
focal.sp='Other trap fish'
focal <- cpuea %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=length(unique(focal$year))) 


emptyPlot(range(times), c(0.25, 1.75), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='f', font=2, cex=1.4)
add_label(0.01, 0.08, label='Mixed species (active)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2, at =seq(0.25, 1.75, 0.5))
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
# mtext(side=2, expression(paste('kg trap'^-1)), line=2, cex=1)
strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y2a[y2a$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lty=5, lwd=1, xpd=TRUE))
}

    with(y2.globala, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[2],lty=5, lwd=3, xpd=TRUE))


# mtext(side=2, expression(paste('kg trap'^-1,'set'^-1, 'hr'^-1)), line=2, cex=1)
# box()

# LETHRINID - FIXED
par(mar=c(1.5,1.5,1.5,1.5))
focal.sp='Capitaine'
focal <- cpuef %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=30) 

emptyPlot(range(times), c(1, 5), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='c', font=2, cex=1.4)
add_label(0.01, 0.08, label='Lethrinid (fixed)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2)
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
# mtext(side=2, expression(paste('kg trap'^-1)), line=2, cex=1)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y3[y3$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1, xpd=TRUE))
}
    with(y3.global, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[3],lty=1, lwd=3, xpd=TRUE))

# box()

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.1),
  lty=1, lwd=c(1,1,3), col=c( '#bcbddc', '#6a51a3', cols[3]), cex=0.9, bty='n')


# LETHRINID - ACTIVE
par(mar=c(1.5,1.5,1.5,1.5))
focal.sp='Capitaine'
focal <- cpuea %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=length(unique(focal$year))) 

emptyPlot(range(times), c(0.5, 1.7), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='g', font=2, cex=1.4)
add_label(0.01, 0.08, label='Lethrinid (active)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2, at = seq(0.5, 1.7, 0.2))
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y3a[y3a$stratum==strat[su],], 
    plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lty=5, lwd=1, xpd=TRUE))
}
    with(y3.globala, 
plot_error(time, CPUEp, se.fit=0,shade=T, 
          col=cols[3],lty=5, lwd=3, xpd=TRUE))

# mtext(side=2, expression(paste('kg trap'^-1,'set'^-1, 'hr'^-1)), line=2, cex=1)
# box()

## OVERALL CPUE
load(file='data/cleaned/CAS_landings_targets_trapCPUE.Rdata')
#  FIXED
par(mar=c(1.5,1.5,1.5,1.5))
focal <- cpue <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
           CAS.name %in% c('Cordonier', 'Other trap fish', 'Capitaine') &
          year != 2017 & 
          year > 1993 &
          GEAR == 'FIXS' 
          # !is.na(NO.GEAR) 
          ) %>% 
  group_by( survey, year, ISLAND, stratum, month, DATE.ym, SITE.NO, dmi, benso, nboats) %>% 
  summarise(cpue=mean(cpue)) %>%
    group_by( year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    summarise(cpue=mean(cpue)) %>% arrange(DATE.ym)


focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=30) 

emptyPlot(range(times), c(2, 6), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='d', font=2, cex=1.4)
add_label(0.01, 0.08, label='Groups combined (fixed)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2)
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)
# mtext(side=2, expression(paste('kg trap'^-1)), line=2, cex=1)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y4[y4$stratum==strat[su],], 
    plot_error(time, p, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1, xpd=TRUE))
}
    with(y4.global, 
plot_error(time, p, se.fit=0,shade=T, 
          col=cols[4],lty=1, lwd=3, xpd=TRUE))

# box()

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.1),
  lty=1, lwd=c(1,1,3), col=c( '#bdbdbd', '#252525', cols[4]), cex=0.9, bty='n')


# ACTIVE
par(mar=c(1.5,1.5,1.5,1.5))

load(file='data/cleaned/CAS_landings_targets_trapCPUE.Rdata')
focal <- cpue <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
           CAS.name %in% c('Cordonier', 'Other trap fish', 'Capitaine') &
          year != 2017 & 
          year > 1993 &
          GEAR == 'FIXA' &
          !is.na(cpue2) & 
          is.finite(cpue2) 
          # !is.na(NO.GEAR) 
          ) %>% 
  mutate(cpue = cpue2) %>%
  group_by( survey, year, ISLAND, stratum, month, DATE.ym, SITE.NO, dmi, benso, nboats) %>% 
  summarise(cpue=mean(cpue)) %>%
    group_by( year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    summarise(cpue=mean(cpue)) %>% arrange(DATE.ym)


focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

axis.vec<-data.frame(labels=seq(1994, 2016, 2))
axis.vec$breaks<-focal$time[match(axis.vec$labels,focal$year )]
axis.vec$breaks[2]<-axis.vec$breaks[1]+(axis.vec$breaks[3]-axis.vec$breaks[1])/2
times <- seq(min(focal$time),max(focal$time), length=length(unique(focal$year))) 

emptyPlot(range(times), c(0.5, 3), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='h', font=2, cex=1.4)
add_label(0.01, 0.08, label='Groups combined (active)', font=1, cex=0.9)
axis(1, at=axis.vec$breaks, label=axis.vec$labels ); axis(2)
axis(1, at = c(min(focal$time), max(focal$time)), label=NA, tck=0)

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y4a[y4a$stratum==strat[su],], 
    plot_error(time, p, se.fit=0,shade=T, 
          col=alpha(col,0.7), lty=5, lwd=1, xpd=TRUE))
}
    with(y4.globala, 
plot_error(time, p, se.fit=0,shade=T, 
          col=cols[4],lty=5, lwd=3, xpd=TRUE))

# mtext(side=2, expression(paste('kg trap'^-1,'set'^-1, 'hr'^-1)), line=2, cex=1)
# box()

dev.off()