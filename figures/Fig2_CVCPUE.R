
library(tidyverse) ## data tidying
library(itsadug) ## emptyPlot and plot_error functions


## Fig 2 - temporal trends in CV CPUE 

## From Sean Anderson http://seananderson.ca/
add_label <- function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

threshold=5
cols<-c('#66c2a5','#fc8d62','#8da0cb')


load(file='CAS_landings_targets_trapCPUE.Rdata')

## subset to predictor dataframe
cpue <- cpue %>% ungroup() %>% filter(
          ISLAND != 'La Digue' &
          # CAS.name != 'Job' &
          year != 2017 & 
          year > 1993 &
          GEAR == 'FIXS' 
          # !is.na(NO.GEAR) 
          ) %>% 
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    mutate(Ncatch = length(cpue)) %>%
    filter(Ncatch >= threshold) %>%
    summarise(cpue=sd(cpue)/mean(cpue)*100) %>% 
    group_by(CAS.name, year, ISLAND, stratum, month, DATE.ym, dmi, benso, nboats) %>% 
    summarise(cpue=mean(cpue)) %>% 
    arrange(DATE.ym)


# # load predictions

## stratum trends
y1<-read.csv('Cordonier_FIXS_cvCPUE.csv')
y1$ISLAND<-cpue$ISLAND[match(y1$stratum, cpue$stratum)]
y1$col<-ifelse(y1$ISLAND=='Mahe', '#addd8e', '#238443')

y2<-read.csv('Other trap fish_FIXS_cvCPUE.csv')
y2$ISLAND<-cpue$ISLAND[match(y2$stratum, cpue$stratum)]
y2$col<-ifelse(y2$ISLAND=='Mahe', '#fdae6b', '#d94801')

y3<-read.csv('Capitaine_FIXS_cvCPUE.csv')
y3$ISLAND<-cpue$ISLAND[match(y3$stratum, cpue$stratum)]
y3$col<-ifelse(y3$ISLAND=='Mahe', '#bcbddc', '#6a51a3')

## mean trends
y1.global<-read.csv('Cordonier_FIXS_cvCPUE_globalpred.csv')
y2.global<-read.csv('Other trap fish_FIXS_cvCPUE_globalpred.csv')
y3.global<-read.csv('Capitaine_FIXS_cvCPUE_globalpred.csv')


## plotting info
cols<-c('#66c2a5','#fc8d62','#8da0cb')
mat<-matrix(c(1,2,3), nrow=3, byrow=T)
layout(mat)

par(oma = c(0.5,0,0,0), mgp=c(3,0.6,0))


par(mar=c(2,4,0.5,2), xpd=TRUE)
## SIGANID - FIXED
focal.sp='Cordonier'
focal <- cpue %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

times <- as.Date(y1.global$time.raw)
axis.vec<-data.frame(labels=str_split_fixed(times, '-', 3)[,1], breaks=as.numeric(times))
seqs<-seq(1, 21, 2)


emptyPlot(range(as.numeric(times)), c(35, 105), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='a', font=2, cex=1.2)
add_label(0.01, 0.08, label='Siganid', font=1, cex=0.9)

axis(2, at = seq(35, 105, 10))
axis(1, at=axis.vec$breaks, label=NA, lwd.ticks=1, lwd=0)
axis(1, at=c(axis.vec$breaks,max(axis.vec$breaks)+0.3),labels=NA, lwd.ticks=0)
mtext(side=2, expression(paste('CV'['CPUE'])), line=2, cex=0.8)

    with(y1.global, 
plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=cols[1], lwd=3))

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y1[y1$stratum==strat[su],], 
    plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1))
}


legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.07),
  lty=1, lwd=c(1,1,3), col=c('#addd8e', '#238443', cols[1]), cex=0.9, bty='n')


# MIXED - FIXED
focal.sp='Other trap fish'
focal <- cpue %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

times <- as.Date(y2.global$time.raw)
axis.vec<-data.frame(labels=str_split_fixed(times, '-', 3)[,1], breaks=as.numeric(times))


emptyPlot(range(as.numeric(times)), c(30, 90), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='b', font=2, cex=1.2)
add_label(0.01, 0.08, label='Mixed species', font=1, cex=0.9)

axis(2)
axis(1, at=axis.vec$breaks, label=NA, lwd.ticks=1, lwd=0)
axis(1, at=c(axis.vec$breaks,max(axis.vec$breaks)+0.3),labels=NA, lwd.ticks=0)
mtext(side=2, expression(paste('CV'['CPUE'])), line=2, cex=0.8)

    with(y2.global, 
plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=cols[2], lwd=3))

    strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y2[y2$stratum==strat[su],], 
    plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1))
}

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.07),
  lty=1, lwd=c(1,1,3), col=c( '#fdae6b', '#d94801', cols[2]), cex=0.9, bty='n')

## save times for range in capitaine
times.base<-times

# MIXED - FIXED
par(mar=c(2,4,0.5,2))
focal.sp='Capitaine'
focal <- cpue %>% filter(CAS.name==focal.sp)
focal$time <- scale(as.numeric(focal$DATE.ym))
focal<-droplevels(focal)

times <- as.Date(y3.global$time.raw)

emptyPlot(range(as.numeric(times.base)), c(50, 100), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='c', font=2, cex=1.2)
add_label(0.01, 0.08, label='Lethrinid', font=1, cex=0.9)
axis(2, bty='l')
axis(1, at=axis.vec$breaks, label=axis.vec$labels, lwd.ticks=1, lwd=0, las=1)
axis(1, at=c(axis.vec$breaks,max(axis.vec$breaks)+0.3),labels=NA, lwd.ticks=0)
mtext(side=2, expression(paste('CV'['CPUE'])), line=2, cex=0.8)

    with(y3.global, 
plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=cols[3], lwd=3))

strat <- levels(focal$stratum)
for(su in 1:length(strat)){
    with(y3[y3$stratum==strat[su],], 
    plot_error(as.numeric(as.Date(time.raw)), CPUEp, se.fit=0,shade=T, 
          col=alpha(col,0.7), lwd=1))
}

legend('topright', legend=c('Mah\uE9', 'Praslin', 'Mean'), ncol=3,inset=c(0, -0.07),
  lty=1, lwd=c(1,1,3), col=c( '#bcbddc', '#6a51a3', cols[3]), cex=0.9, bty='n')

