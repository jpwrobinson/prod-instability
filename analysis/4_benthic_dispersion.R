
## Script for estimating multivariate dispersion of benthic taxa

library(vegan)
library(tidyverse)


## Data load - benthic surveys
load(file='data/cleaned/SEY_UVC_benthicPV_SC_DEPTH.Rdata')
SC<-SC %>% select(complexity, hard.coral, macroalgae)
colnames(SC)<-c('Complexity', 'Hard coral', 'Macroalgae')

## create community matrix of benthic cover, log10 transformed
mat<-scale(log10(as.matrix(SC)+1))

## ------------------------------------------------------------------------ ##
	## estimate beta dispersion for each year (Anderson et al. 2006, Ecol. Lett)
## ------------------------------------------------------------------------ ##
d<-vegdist(mat, 'euclidean')
groups = c(rep(c(1994, 2005, 2008, 2011, 2014), each=12))
permutest(dis)
dis<-betadisper(d, groups, 'centroid')

## difference in mean distances to centroid?
## permutatino test on PCoA
set.seed(42)
permutest(dis)

## PCoA space - 1 vs 2
plot(dis, axes=c(1,2), ellipse = TRUE, hull = TRUE, main='Functional dispersion of benthic taxa', 
	xlab=paste('PCoA1:', pc1, '%'),ylab=paste('PCoA2:', pc2, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp1<-envfit(dis, mat, perm=1000, choices=c(1,2))
#plot loading vectors
plot(vec.sp1, p.max=0.05, col='black', add=T) 

## PCoA space - 2 vs. 3
plot(dis, axes=c(1,3), ellipse = TRUE, hull = TRUE, main='Functional dispersion of benthic taxa',
	xlab=paste('PCoA1:', pc1, '%'),ylab=paste('PCoA3:', pc3, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp3<-envfit(dis, mat, perm=1000, choices=c(1,3))
#plot loading vectors
plot(vec.sp3, p.max=0.05, col='black', add=T) 

## PCoA space - 2 vs. 3
plot(dis, axes=c(2,3), ellipse = TRUE, hull = TRUE, main='Functional dispersion of benthic taxa',
	xlab=paste('PCoA2:', pc2, '%'),ylab=paste('PCoA3:', pc3, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp2<-envfit(dis, mat, perm=1000, choices=c(2:3))
#plot loading vectors
plot(vec.sp2, p.max=0.05, col='black', add=T) 

dev.off()

save(dis, SC, mat, d, vec.sp1,vec.sp2,vec.sp3, file='MV_benthic_dispersion.Rdata')

