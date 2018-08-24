
library(tidyverse) ## data tidying
library(itsadug) ## emptyPlot and plot_error functions


## Fig 3 - temporal trends in catches

## From Sean Anderson http://seananderson.ca/
add_label <- function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

load('data/cleaned/CAS_yields_FIXS_FIXA_allsp.Rdata') ## dataframe = 'y'

## subset to predictor dataframe
y<- y %>% group_by(year, location, month, DATE,dmi,benso, gear.type, speciesgr3) %>% 
              mutate(catches = ifelse(is.na(catches), 0, catches)) %>%
    summarise(catch = sum(catches)) %>% 
    filter(speciesgr3 %in% c('capitaine', 'cordonier', 'other trap fish'))
y$time <- scale(as.numeric(y$DATE)); y$log10catch<-log10(y$catch+1)

## load predictions
y1<-read.csv('data/results/predicted/yield/cordonier_Static Trap.csv')
y2<-read.csv('data/results/predicted/yield/Other trap fish_Static Trap.csv')
y3<-read.csv('data/results/predicted/yield/capitaine_Static Trap.csv')
y0F<-read.csv('data/results/predicted/yield/OTHER_Static Trap.csv')
y0Fall<-read.csv('data/results/predicted/yield/ALL_Static Trap.csv')

y4<-read.csv('data/results/predicted/yield/cordonier_Active Trap.csv')
y5<-read.csv('data/results/predicted/yield/Other trap fish_Active Trap.csv')
y6<-read.csv('data/results/predicted/yield/capitaine_Active Trap.csv')
y0A<-read.csv('data/results/predicted/yield/OTHER_Active Trap.csv')
y0all<-read.csv('data/results/predicted/yield/ALL_Active Trap.csv')

## plotting information
col.vec<-c('#d73027','#1a9850')
axis.vec<-data.frame(labels=seq(2000, 2016, 4))
axis.vec$breaks<-y$time[match(axis.vec$labels, y$year )]
strat <- unique(y1$location)
times <- seq(min(y1$time),max(y1$time), length=30) 
lwd <- c(1.5, 1.5)
mat<-matrix(c(1,2,3,4), nrow=2, ncol=2,byrow=T)
cols<-c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3', 'grey')
cx.ax=0.8; cx.label=1

layout(mat)
par(mgp=c(3,0.5,0))
par(xpd=TRUE)

#### Fixed gear yields - Mahe
par(mar=c(2,4,1,0))
emptyPlot(range(times), c(0, 1.5), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='a', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Mah\uE9 catch (fixed)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
mtext(side=2, expression(paste('Total catch (', 'log'[10], ' t)')), line=2, cex=0.7)

siganid
with(y1[y1$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[1], lwd=-10, xpd=TRUE))
# mixed herb
with(y2[y2$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[2], lwd=-10, xpd=TRUE))
lethrinid
with(y3[y3$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[3], lwd=-10, xpd=TRUE))
other
with(y0F[y0F$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[4], lwd=-10, xpd=TRUE))
all
with(y0Fall[y0Fall$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[5], lwd=-10, xpd=TRUE))

## overlay lines
with(y1[y1$location == strat[1],],
    lines(time, p, col=cols[1], lwd=lwd[1]))
with(y2[y2$location == strat[1],],
    lines(time, p, col=cols[2], lwd=lwd[1]))
with(y3[y3$location == strat[1],],
    lines(time, p, col=cols[3], lwd=lwd[1]))
with(y0F[y0F$location == strat[1],],
    lines(time, p, col=cols[4], lwd=lwd[1]))
with(y0Fall[y0Fall$location == strat[1],],
    lines(time, p, col=cols[5], lwd=lwd[1]))

#### Active gear yields - Mahe
par(mar=c(2,2,1,2))
emptyPlot(range(times), c(-0.25, 1), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='c', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Mah\uE9 catch (active)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
# mtext(side=2, 'Landed weight (MT)', line=2, cex=0.7)

# siganid
with(y4[y4$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[1], lwd=-10, lty=5, xpd=TRUE))
# mixed herb
with(y5[y5$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[2], lwd=-10, lty=5, xpd=TRUE))
lethrinid
with(y6[y6$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[3], lwd=-10, lty=5, xpd=TRUE))
other
with(y0A[y0A$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[4], lwd=-10, lty=5, xpd=TRUE))
all
with(y0all[y0all$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[5], lwd=-10, lty=5, xpd=TRUE))

## overlay lines
with(y4[y4$location == strat[1],],
    lines(time, p, col=cols[1], lwd=lwd[1],lty=5))
with(y5[y5$location == strat[1],],
    lines(time, p, col=cols[2], lwd=lwd[1],lty=5))
with(y6[y6$location == strat[1],],
    lines(time, p, col=cols[3], lwd=lwd[1],lty=5))
with(y0A[y0A$location == strat[1],],
    lines(time, p, col=cols[4], lwd=lwd[1],lty=5))
with(y0all[y0all$location == strat[1],],
    lines(time, p, col=cols[5], lwd=lwd[1],lty=5))



legend("topright", inset=c(-0.12,-0.05), 
  legend=c('Siganid', 'Mixed species', 'Lethrinid'), 
  lty=1,col=c(cols[1:3]), lwd=2,bty='n', cex=0.6)

legend("topright", inset=c(0.27,-0.05), 
  legend=c('Total', 'Other'), 
  lty=1,col=c(cols[c(5,4)]), lwd=2,bty='n', cex=0.6)



#### Fixed gear yields - Praslin
par(mar=c(2,4,1,0))
emptyPlot(range(times), c(-0.25, 1.25), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='b', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Praslin catch (fixed)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, at=seq(-0.25, 1.25, 0.25),cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
mtext(side=2, expression(paste('Total catch (', 'log'[10], ' t)')), line=2, cex=0.7)

## mixed herb
with(y2[y2$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[2], lwd=-10, xpd=TRUE))
with(y2[y2$location == strat[2],],
    lines(time, p, col=cols[2], lwd=lwd[2]))


# lethrinid
with(y3[y3$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[3], lwd=-10, xpd=TRUE))
with(y3[y3$location == strat[2],],
    lines(time, p, col=cols[3], lwd=lwd[2]))

## siganid
with(y1[y1$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[1], lwd=-10, xpd=TRUE))
with(y1[y1$location == strat[2],],
    lines(time, p, col=cols[1], lwd=lwd[2]))

# other
with(y0F[y0F$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[4], lwd=-10, xpd=TRUE))
with(y0F[y0F$location == strat[2],],
    lines(time, p, col=cols[4], lwd=lwd[2]))

# total
with(y0Fall[y0Fall$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[5], lwd=-10,lty=5, xpd=TRUE))
with(y0Fall[y0Fall$location == strat[2],],
    lines(time, p, col=cols[5], lwd=lwd[2],lty=1))


#### Active gear yields - Praslin
par(mar=c(2,2,1,2))
emptyPlot(range(times), c(-0.1, 0.6), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='d', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Praslin catch (active)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
# mtext(side=2, 'Landed weight (MT)', line=2, cex=0.7)

## siganid
with(y4[y4$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[1], lwd=-10,lty=5, xpd=TRUE))
with(y4[y4$location == strat[2],],
    lines(time, p, col=cols[1], lwd=lwd[2],lty=5))

## mixed herb
with(y5[y5$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[2], lwd=-10,lty=5, xpd=TRUE))
with(y5[y5$location == strat[2],],
    lines(time, p, col=cols[2], lwd=lwd[2],lty=5))

# lethrinid
with(y6[y6$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[3], lwd=-10,lty=5, xpd=TRUE))
with(y6[y6$location == strat[2],],
    lines(time, p, col=cols[3], lwd=lwd[2],lty=5))

# other
with(y0A[y0A$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[4], lwd=-10,lty=5, xpd=TRUE))
with(y0A[y0A$location == strat[2],],
    lines(time, p, col=cols[4], lwd=lwd[2],,lty=5))

## total
with(y0all[y0all$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1,
          col=cols[5], lwd=-10,lty=5, xpd=TRUE))
with(y0all[y0all$location == strat[2],],
    lines(time, p, col=cols[5], lwd=lwd[2],lty=5))


# ## load raw estimates
# boats<-read.csv('data/cleaned/CAS_active_boats_bygear_byisland.csv')
# boats1<-read.csv('data/results/predicted/effort/nboats_total_FIXS.csv')
# boats2<-read.csv('data/results/predicted/effort/nboats_total_FIXA.csv')
# boats$time <- scale(as.numeric(boats$DATE.ym))

# # subset to 2000
# boat.lim = unique(boats$time[boats$DATE.ym=='2000-01-01'])
# boats<-boats[boats$time >= boat.lim,]
# boats1<-boats1[boats1$time >= boat.lim,]
# boats2<-boats2[boats2$time >= boat.lim,]



# ## plotting info
# cols<-c('black')

# #### nboats - fixed gear
# par(mar=c(2,1,1,2))

# ## traps - mahe
# focal<-boats1[boats1$ISLAND=='Mahe',]
# yrs<-uniques(boats$year)
# times <- seq(min(focal$time),max(focal$time), length=yrs) 

# ## get x vals
# xlims<-range(times)

# axis.vec<-data.frame(labels=seq(2000, 2016, 4))
# axis.vec$breaks<-boats$time[match(axis.vec$labels, boats$year)]

# emptyPlot(xlims, c(0,27), 
#            xlab='', 
#           ylab="", axes=F)
# with(focal[focal$ISLAND == 'Mahe',],
#     plot_error(time, ngboatp, se.fit=2*CI, f =1, shade=T,alpha=0.1,
#           col=cols[1], lwd=-10,lty=5, xpd=TRUE))
# with(boats2[boats2$ISLAND == 'Mahe',],
#     plot_error(time, ngboatp, se.fit=2*CI, f =1, shade=T,alpha=0.1,
#           col=cols[1], lwd=-10,lty=5, xpd=TRUE))

# add_label(0.01, 0.001, label='e', font=2, cex=cx.label)
# add_label(0.01, 0.08, label='Mah\uE9 fleet size', font=1, cex=0.8)
# axis(2, at = seq(0,27, by =5), cex.axis=cx.ax)
# axis(1, at =axis.vec$breaks , label=axis.vec$labels, cex.axis=cx.ax)
# mtext(side=2, 'Number of active boats', line=1.5, cex=0.7)
#     with(focal[focal$ISLAND=='Mahe',],
# lines(time, ngboatp, col=cols[1],lty=1, lwd=1, xpd=TRUE, cex=0.5,pch=19))
#     with(boats2[boats2$ISLAND=='Mahe',],
# lines(time, ngboatp, col=cols[1],lty=5, lwd=1, xpd=TRUE, cex=0.5,pch=19))
# axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0,lty=1)

# par(xpd=TRUE)
# legend("topright", inset=c(-0.067,-0.07), 
#   legend=c('Fixed traps', 'Active traps'), 
#   lty=c(1,2),col=c('black'), lwd=1,bty='n', cex=0.6)

# ## traps - praslin
# focal<-boats1[boats1$ISLAND=='Praslin',]

# ## get x vals
# xlims<-range(times)

# emptyPlot(xlims, c(1,12), 
#            xlab='', 
#           ylab="", axes=F)
# with(focal[focal$ISLAND == 'Praslin',],
#     plot_error(time, ngboatp, se.fit=2*CI, f =1, shade=T,alpha=0.1,
#           col=cols[1], lwd=-10,lty=5, xpd=TRUE))
# with(boats2[boats2$ISLAND == 'Praslin',],
#     plot_error(time, ngboatp, se.fit=2*CI, f =1, shade=T,alpha=0.1,
#           col=cols[1], lwd=-10,lty=5, xpd=TRUE))

# add_label(0.01, 0.001, label='f', font=2, cex=cx.label)
# add_label(0.01, 0.08, label='Praslin fleet size', font=1, cex=0.8)
# axis(2, at = seq(1,12, by =2), cex.axis=cx.ax)
# axis(1, at =axis.vec$breaks , label=axis.vec$labels, cex.axis=cx.ax)
# mtext(side=2, 'Number of active boats', line=1.5, cex=0.7)
#     with(focal[focal$ISLAND=='Praslin',],
# lines(time, ngboatp, col=cols[1],lty=1, lwd=1, xpd=TRUE, cex=0.5,pch=19))
#     with(boats2[boats2$ISLAND=='Praslin',],
# lines(time, ngboatp, col=cols[1],lty=5, lwd=1, xpd=TRUE, cex=0.5,pch=19))
# axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0,lty=1)


dev.off()