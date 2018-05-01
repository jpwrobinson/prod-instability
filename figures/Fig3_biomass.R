rm(list=ls())
# setwd('/Users/robins64/Documents/git_repos/beta-catch')
library(plotrix); library(here)
source(here('scripts/loads.R'))

pdf(here('figures/final/4_fig_fishhab_variance.pdf'), height=7, width=12)


### Figure shows spatial variation through time in fish biomass + benthic composition.
mat<-matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,4,4,5,5,3,3,4,4,5,5), nrow=4, ncol=6, byrow=T)
layout(mat)
par(mar=c(4,4,2,2))

## Panel A = fish biomass raw
biom<-read.csv('data/results/biomass/UVC_temporal_meanbiom_CASgroups.csv')
species=c('Cordonier', 'Other trap fish', 'Capitaine')
biom <- biom %>% filter(Year != '2017' & Species.group %in% species) %>%
			select(biom, Year, Species.group) %>% mutate(biom = log10(biom+1))
biom$fam <- plyr::revalue(biom$Species.group, 
			c('Capitaine' = "Lethrinid", 
				'Cordonier' = 'Siganid', 
				'Other trap fish' = "Other herbivores")) 

cols<-c('#66c2a5','#fc8d62','#8da0cb')

with(biom[biom$fam=='Siganid',], boxplot(biom ~ Year, col=cols[1],at=c(1:5), ylim=c(0, 3), xlim=c(0.9, 15), lty=1,boxwex=0.6, axes=F))
with(biom[biom$fam=='Other herbivores',], boxplot(biom ~ Year, col=cols[2],at=c(6:10), lty=1,boxwex=0.6, add=TRUE, axes=F))
with(biom[biom$fam=='Lethrinid',], boxplot(biom ~ Year, col=cols[3],at=c(11:15), lty=1,boxwex=0.6, add=TRUE, axes=F))
axis(1, at=c(1:15), labels=rep(c(1994, 2005, 2008, 2011, 2014), times=3), las=1, cex.axis=1)
axis(2); mtext(2, text=expression(paste('Log'['10'],'(biomass + 1) kg ha'^'-1')), line=2, cex=0.8)
abline(v=5.5, lty=2, col='grey'); abline(v=10.5, lty=2, col='grey')
add_label(0.01, 0.05, 'a', cex=1.4, font=2)

legend('topright', legend=c('Siganid', 'Mixed species', 'Lethrinid'), 
	col=cols, pch=15, bty='n', cex=1.1, inset=c(0,0), xpd=TRUE)


## Panel B = fish and benthic PCoA
load(here('data/results/benthos/benthic_fish_dispersion.Rdata'))

# par(mar=c(5,5,4,2))
emptyPlot(c(0.3,2.5), c(0.8, 2.4), 
           xlab='', 
          ylab="", axes=F)
mtext(1, text='Benthic dispersion', line=2.5, cex=0.8)
mtext(2, text='Fish dispersion', line=2.5, cex=0.8)

abline(lm1, lwd=2, col=alpha('black',0.5))
abline(confint(lm1)[,1], lty=2)
abline(confint(lm1)[,2], lty=2)
plotCI(d.ben$mean , d.ben$fish.mean,  err='x', ui=d.ben$mean + 2*d.ben$se, li=d.ben$mean - 2*d.ben$se, axes=F, xlab='', ylab='',
		pch = 16, cex=1.4, col=cols, xlim=c(0.5, 2), add=T)
plotCI(d.ben$mean , d.ben$fish.mean,add=TRUE, ui=d.ben$fish.mean + 2*d.ben$fish.se, li=d.ben$fish.mean - 2*d.ben$fish.se, axes=F, xlab='', ylab='',
		pch = 16, cex=1.4, col=cols)
for(i in 1:5){
	points(benthos$dis[benthos$year==yrs[i]], fish$dis[fish$year==yrs[i]], col=alpha(cols[i],0.6), pch=16, cex=1)}
axis(1, at=seq(0.3, 2.5, 0.2)); axis(2 , at=seq(0.8, 2.4, 0.2))
text(d.ben$mean+0.05, d.ben$fish.mean-0.05, label=yrs, col=cols, font=2, cex=0.8)
add_label(0.01, 0.05, 'b', cex=1.4, font=2)

## Fish - habitat - c, d ,e
# load top models
load(here('data/results/models/UVC_biom_GAMs.Rdata'))

## load biomass estimates
biom<-read.csv(here('data/results/biomass/UVC_temporal_meanbiom_CASgroups.csv'))
regions<-plyr::ldply(strsplit(as.character(biom$Location), '\ '))
biom$region<-factor(with(regions, paste(V1, V2, sep='\ ')))
biom <- biom %>% filter(Year != '2017') %>% mutate(logbiom=log10(biom + 1))
head(biom[biom$macroalgae>50,],20)
# plotting info 
par(mar=c(4,4,2,1))
cols<-c('#66c2a5','#fc8d62','#8da0cb')
col.cords<-c('black', '#99d8c9','#41ae76','#238b45', '#006d2c')
otf.cords<-c('black', '#fdae6b', '#f16913','#d94801','#a63603')
cap.cords<-c('black', '#a6bddb', '#3690c0', '#0570b0','#045a8d')
state.shapes<-c(16, 16, rep(17, 6), 16, 16, 17, 16)

## get raw x axis values
focal.cord<-biom %>% filter(Species.group=='Cordonier')
focal.cord$pch<-ifelse(focal.cord$state=='Shifted', 16, 17)
scaled.ma<-scale(focal.cord$macroalgae)
ma<-seq(min(biom$macroalgae), max(biom$macroalgae), 25)

focal.otf<-biom %>% filter(Species.group=='Other trap fish')
focal.otf$pch<-ifelse(focal.otf$state=='Shifted', 16, 17)
scaled.sc<-scale(focal.otf$complexity)
sc<-seq(min(biom$complexity), max(biom$complexity), 0.5)

focal.cap<-biom %>% filter(Species.group=='Capitaine')
focal.cap$pch<-ifelse(focal.cap$state=='Shifted', 16, 17)

a<-plot(cord.gam, select=3, residuals = T, col=cols[1], xlab='', ylab='',ylim=c(-1.5, 2),
	shade=T,shade.col=alpha(cols[1],0.2), cex=0.0001,pch=16, lwd=2, axes=F)
a<-a[[3]]; a[['col']]<-rep(col.cords, times=12); a[['pch']]<-focal.cord$pch
points(a$raw, a$p.resid, col=a$col, pch=a$pch, cex=1.5)
add_label(0.01, 0.05, 'c', cex=1.4, font=2)
legend('topright', legend=c(1994, 2005, 2008, 2011, 2014), 
			text.col=col.cords, bty='n', inset=c(0.03, -0.02), xpd=T)
axis(1, at=seq(min(scaled.ma), max(scaled.ma), length.out=4), labels=ma)
axis(2, at=seq(-1.5, 2,0.25))
mtext(1, text='Macroalgal cover (%)', line=2.5, cex=0.8)
mtext(2, text='Scaled effect on biomass', line=2.5, cex=0.8)

par(mar=c(4,1,2,1))
a<-plot(otf.gam, select=2, residuals = T, col=cols[2], xlab='', ylab='',ylim=c(-0.9, 0.7),
	shade=T,shade.col=alpha(cols[2],0.2), cex=0.00001,pch=16, lwd=2, axes=F)
a<-a[[2]]; a[['col']]<-rep(otf.cords, times=12); a[['pch']]<-focal.otf$pch
points(a$raw, a$p.resid, col=a$col, pch=a$pch, cex=1.5)
add_label(0.01, 0.05, 'd', cex=1.4, font=2)
legend('topright', legend=c(1994, 2005, 2008, 2011, 2014), 
			text.col=otf.cords, bty='n', inset=c(0.03, -0.02), xpd=T)
legend('topleft', legend=c('Shifted', 'Recovering'), pt.cex=1.5,
			text.col='black', col=otf.cords[5],pch=c(16,17), bty='n', inset=c(0.03, 0.1), xpd=T)
axis(1, at=seq(min(scaled.sc), max(scaled.sc), length.out=7), labels=sc)
axis(2, at=seq(-0.9, 0.7, 0.2))

mtext(1, text='Structural complexity', line=2.5, cex=0.8)

a<-plot(cap.gam, select=3, residuals = T, col=cols[3], xlab='', ylab='',
	shade=T,shade.col=alpha(cols[3],0.2), cex=0.00001,pch=16, lwd=2, axes=F)
a<-a[[3]]; a[['col']]<-rep(cap.cords, times=12); a[['pch']]<-focal.cap$pch
points(a$raw, a$p.resid, col=a$col, pch=a$pch, cex=1.3)
add_label(0.01, 0.05, 'e', cex=1.4, font=2)
legend('topright', legend=c(1994, 2005, 2008, 2011, 2014), 
			text.col=cap.cords, bty='n', inset=c(0.03, -0.02), xpd=T)
axis(1, at=seq(min(scaled.ma), max(scaled.ma), length.out=4), labels=ma)
axis(2, at = seq(-2.5, 1.1, 0.3))
mtext(1, text='Macroalgal cover (%)', line=2.5, cex=0.8)


dev.off()