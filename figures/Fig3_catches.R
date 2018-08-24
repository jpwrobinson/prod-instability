
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

## Load observed catches (not available here)
yields<-read.csv('yields_observed_94-16.csv')

## Load model predictions
y1<-read.csv('model_predictions/siganid_Static Trap.csv')
y2<-read.csv('model_predictions/mixedspecies_Static Trap.csv')
y3<-read.csv('model_predictions/lethrinid_Static Trap.csv')

y4<-read.csv('model_predictions/siganid_Active Trap.csv')
y5<-read.csv('model_predictions/mixedspecies_Active Trap.csv')
y6<-read.csv('model_predictions/lethrinid_Active Trap.csv')


## plotting information
col.vec<-c('#d73027','#1a9850')
axis.vec<-data.frame(labels=seq(1994, 2014, 4))
axis.vec$breaks<-yields$time[match(axis.vec$labels, yields$year )]
strat <- unique(yields$location)
times <- seq(min(yields$time),max(yields$time), length=30) 
lwd <- c(1.5, 1.5)
mat<-matrix(c(1,2,3,4), nrow=2, ncol=2,byrow=T)
cols<-c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3', 'grey')
cx.ax=0.7; cx.label=0.9


layout(mat)
par(mgp=c(3,0.5,0),xpd=TRUE)

#### Fixed gear yields - Mahe
par(mar=c(2,4,1,0))
emptyPlot(range(times), c(0, 1.4), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='a', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Mah\uE9 catch (fixed)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
mtext(side=2, expression(paste('Total catch (', 'log'[10], ' t)')), line=2, cex=0.75)

# siganid
with(y1[y1$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[1], lwd=-10, xpd=TRUE))
# mixed herb
with(y2[y2$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[2], lwd=-10, xpd=TRUE))
# lethrinid
with(y3[y3$location == strat[1],],
    plot_error(time, p, se.fit=2*CI, f =1, shade=T,alpha=0.1,
          col=cols[3], lwd=-10, xpd=TRUE))

## overlay lines
with(y1[y1$location == strat[1],],
    lines(time, p, col=cols[1], lwd=lwd[1]))
with(y2[y2$location == strat[1],],
    lines(time, p, col=cols[2], lwd=lwd[1]))
with(y3[y3$location == strat[1],],
    lines(time, p, col=cols[3], lwd=lwd[1]))


#### Active gear yields - Mahe
par(mar=c(2,2,1,2))
emptyPlot(range(times), c(-0.2, 0.9), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='c', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Mah\uE9 catch (active)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
# mtext(side=2, 'Landed weight (MT)', line=2, cex=0.7)

# siganid
with(y4[y4$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[1], lwd=-10, xpd=TRUE))
# mixed herb
with(y5[y5$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[2], lwd=-10, xpd=TRUE))
# lethrinid
with(y6[y6$location == strat[1],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[3], lwd=-10, xpd=TRUE))


## overlay lines
with(y4[y4$location == strat[1],],
    lines(time, p, col=cols[1], lwd=lwd[1],lty=5))
with(y5[y5$location == strat[1],],
    lines(time, p, col=cols[2], lwd=lwd[1],lty=5))
with(y6[y6$location == strat[1],],
    lines(time, p, col=cols[3], lwd=lwd[1],lty=5))



legend("topright", inset=c(0.12,-0.1), 
  legend=c('Siganid', 'Mixed species', 'Lethrinid'), 
  lty=1,col=c(cols[1:3]), lwd=2,bty='n', cex=0.5)


#### Fixed gear yields - Praslin
par(mar=c(2,4,1,0))
emptyPlot(range(times), c(-0.25, 0.9), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='b', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Praslin catch (fixed)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, at=seq(-0.25, 1, 0.25),cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)
mtext(side=2, expression(paste('Total catch (', 'log'[10], ' t)')), line=2, cex=0.75)

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



#### Active gear yields - Praslin
par(mar=c(2,2,1,2))
emptyPlot(range(times), c(-0.1, 1), 
           xlab='', 
          ylab="", axes=F)
add_label(0.01, 0.001, label='d', font=2, cex=cx.label)
add_label(0.01, 0.08, label='Praslin catch (active)', font=1, cex=0.8)
axis(1, at=axis.vec$breaks, label=axis.vec$labels, cex.axis=cx.ax ); axis(2, cex.axis=cx.ax)
axis(1, at=c(0,max(times)), labels=c("",""), lwd.ticks=0)


## siganid
with(y4[y4$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[1], lwd=-10, xpd=TRUE))
with(y4[y4$location == strat[2],],
    lines(time, p, col=cols[1], lwd=lwd[2],lty=5))

## mixed herb
with(y5[y5$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[2], lwd=-10, xpd=TRUE))
with(y5[y5$location == strat[2],],
    lines(time, p, col=cols[2], lwd=lwd[2],lty=5))

# lethrinid
with(y6[y6$location == strat[2],],
    plot_error(time, p, se.fit=(2*CI), f =1, shade=T,alpha=0.1, lty=5,
          col=cols[3], lwd=-10, xpd=TRUE))
with(y6[y6$location == strat[2],],
    lines(time, p, col=cols[3], lwd=lwd[2],lty=5))

