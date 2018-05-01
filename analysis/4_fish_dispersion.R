

## Script for estimating multivariate dispersion of fish biomass

library(vegan)
library(tidyverse)

## Data load - fish biomass
biom<-read.csv('data/results/biomass/UVC_temporal_meanbiom_CASgroups.csv')

## filter to relevant fish gruops
biom<- biom %>% filter(Year !=2017 & Species.group %in% c('Cordonier', 'Other trap fish', 'Capitaine') )
biom$Species.group<- recode(biom$Species.group, 
					'Cordonier' = "Siganid", 'Other trap fish' = 'Mixed species', 'Capitaine' = "Lethrinid")
sites<-unique(biom$Location); yrs<-unique(biom$Year)

## spread biomass by species groups
biom.mat <- biom  %>% select(Species.group, biom) %>% 
			group_by(Species.group) %>%
			mutate(id = row_number()) %>% 
			spread(Species.group, biom)
biom.mat$id<-NULL

## create community matrix of biomass by species group
mat<-scale(log10(as.matrix(biom.mat)+1))

## ------------------------------------------------------------------------ ##
	## estimate beta dispersion for each Year (Anderson et al. 2006, Ecol. Lett)
## ------------------------------------------------------------------------ ##
d<-vegdist(mat, 'euclidean')
groups = c(rep(c(1994, 2005, 2008, 2011, 2014), times=12))

dis<-betadisper(d, groups, 'centroid')

## difference in mean distances to centroid
## permutatino test on PCoA
set.seed(42)
permutest(dis)

## PCoA space - 1 vs 2
plot(dis, axes=c(1,2), ellipse = TRUE, hull = TRUE, main='Functional dispersion of fish taxa', 
	xlab=paste('PCoA1:', pc1, '%'),ylab=paste('PCoA2:', pc2, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp1<-envfit(dis, biom.mat, perm=1000, choices=c(1,2))
#plot loading vectors
plot(vec.sp1, p.max=0.05, col='black', add=T) 

## PCoA space - 2 vs. 3
plot(dis, axes=c(1,3), ellipse = TRUE, hull = TRUE, main='Functional dispersion of fish taxa',
	xlab=paste('PCoA1:', pc1, '%'),ylab=paste('PCoA3:', pc3, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp3<-envfit(dis, biom.mat, perm=1000, choices=c(1,3))
#plot loading vectors
plot(vec.sp3, p.max=0.05, col='black', add=T) 

## PCoA space - 2 vs. 3
plot(dis, axes=c(2,3), ellipse = TRUE, hull = TRUE, main='Functional dispersion of fish taxa',
	xlab=paste('PCoA2:', pc2, '%'),ylab=paste('PCoA3:', pc3, '%'))
#calculate the loading (i.e., variable weights) on each axis
vec.sp2<-envfit(dis, biom.mat, perm=1000, choices=c(2:3))
#plot loading vectors
plot(vec.sp2, p.max=0.05, col='black', add=T) 

## save for final figures
save(dis, biom.mat, mat, d, vec.sp1,vec.sp2, vec.sp3, file='MV_fish_dispersion.Rdata')



