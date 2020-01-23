rm(list=ls())
library(maptools)
library(sp)
library(rgdal)
library(raster)

kt = read.csv('/Users/david/Documents/Projects/CalClimateChange/BayArea-Moore-09-10/BA-traits-2010/katie18aug.csv',as.is=T)
head(kt)
tail(kt)

# Descriptive stats for data set
dim(kt)
names(kt)
length(table(kt$Species))
sort(table(kt$Species))

# map locations
C10 <- readShapeSpatial('/Users/Shared/gisdata/maps/bayarea/political/tbc3_10counties/tbc3_10counties')

ktPts <- SpatialPoints(cbind(kt$long,kt$lat))
proj4string(ktPts) <- CRS('+proj=longlat +datum=WGS84')
ktPtsTA <- spTransform(ktPts,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))

plot(C10,axes=T)
plot(ktPtsTA,add=T,col='red')

# descriptive stats
names(kt)
spList <- sort(unique(kt$Species))
ag <- rep('A',33)
ag[c(16:19,22,32)] <- 'G'
agcols <- rep('blue',33)
agcols[c(16:19,22,32)] <- 'green'

# leafArea is in mm2
par(mar=c(5.1,5.1,1,1))
hist(kt$leafArea[which(kt$Species=='Baccharis pilularis')])

spLA <- tapply(kt$leafArea,kt$Species,mean)
spLA <- spLA[order(names(spLA))]
sort(spLA)
barplot(log10(spLA)[order(spLA)],col=agcols[order(spLA)],ylab='leaf size (mm2, log10)')

spSLA <- tapply(kt$sla,kt$Species,mean)
spSLA <- spSLA[order(names(spSLA))]
sort(spSLA)
barplot(log10(spSLA)[order(spSLA)],col=agcols[order(spSLA)],ylab='specific leaf area')

plot(log10(spLA),spSLA,pch=19,cex=1.5,col=agcols)
cor(log10(spLA),spSLA)
cor(log10(spLA)[ag=='A'],spSLA[ag=='A'])
names(spLA)

spHT <- tapply(kt$height,kt$Species,mean)
spHT <- spHT[order(names(spHT))]
sort(spHT)
barplot(log10(spHT+1)[order(spHT)],col=agcols[order(spHT)],ylab='height (m)')
barplot(spHT[order(spHT)],col=agcols[order(spHT)],ylab='height (m)')
length(spHT)
spHT

plot(spHT,spSLA,pch=19,cex=1.5,col=agcols)

## leaf level plots
plot(log10(kt$leafArea),log10(kt$sla))

## query climate data
tmax <- raster('/Users/Shared/gisdata/climate_layers/BCM/SFBA_tbc3/BA_HST-1.7z/tmx1951_1980_ave_HST.asc')
plot(tmax,axes=T)
kt.tmax <- extract(tmax,ktPtsTA)

tmin <- raster('/Users/Shared/gisdata/climate_layers/BCM/SFBA_tbc3/BA_HST-1.7z/tmn1951_1980_ave_HST.asc')
kt.tmin <- extract(tmin,ktPtsTA)

cwd <- raster('/Users/Shared/gisdata/climate_layers/BCM/SFBA_tbc3/BA_HST-1.7z/cwd1951_1980_ave_HST.asc')
kt.cwd <- extract(tmin,ktPtsTA)

ppt <- raster('/Users/Shared/gisdata/climate_layers/BCM/SFBA_tbc3/BA_HST-1.7z/ppt1951_1980_ave_HST.asc')
kt.ppt <- extract(ppt,ktPtsTA)

kt <- data.frame(kt,kt.tmax,kt.tmin,kt.ppt,kt.cwd)
dim(kt)
names(kt)[21:24] <- c('tmax','tmin','ppt','cwd')

## ancovas of within species variation vs. climate
nOcc <- table(kt$Species)
sort(nOcc)

hiN <- names(nOcc)[nOcc>8]

# leaf size declines at higher cwd, overall!!
lla <- log10(kt$leafArea)
plot(kt$cwd,lla)
slopes <- c()
pval <- c()
for (i in 1:length(hiN)) {
    spsel <- which(kt$Species==hiN[i])
    #points(lla[spsel],kt$sla[spsel],pch=19,col='red')
    fit <- lm(lla[spsel]~kt$cwd[spsel])
    abline(fit)  
    slopes <- c(slopes,fit$coeff[2])
    summary(fit)
}
hist(slopes)
spsel <- which(kt$Species %in% hiN)
fit <- lm(lla[spsel]~cwd[spsel]+factor(Species[spsel]),data=kt)
summary(fit)
anova(fit)



par(cex.axis=1,cex.lab=1,mai=c(2,2,0.5,0.5))
plot(kt$lat,kt$long,asp=1,pch=19,cex=1)
plot(kt$temp,kt$precip,cex=1,col='black',pch=19)
plot(kt$precip,kt$ldmc,pch=19,cex=1)

kt[which(kt$sla>70),]

species = sort(unique(kt$Species))
species
grasses = c(7,9,11,15)
non.grass.data = which(!kt$Species %in% species[grasses])
kt = kt[non.grass.data,]
oaks <- grep('Quercus',kt$Species)
oaks

plot(kt$cwd[oaks],kt$sla[oaks],pch=19,cex=1)

Nsp = table(kt$Species)
largeSample = as.numeric(which(Nsp>20))
names(Nsp)[largeSample]

ktl = subset(kt,kt$Species %in% names(Nsp)[largeSample])
dim(ktl)

#hist(ktl$l.sla)

lsp = sort(unique(ktl$Species))


fit = lm(y ~ as.factor(Species) * x,data=ktl)
anova(fit)
summary(fit)

plot(ktl$y~as.factor(ktl$Species),lwd=5)
plot(log10(kt$height),log10(kt$leafArea),cex=5,pch=19)

i=10
	dat = which(ktl$Species == lsp[i])
plot(ktl$lat[dat],ktl$long[dat],cex=5,pch=19,col=tim.colors(8)[cut(ktl$sla[dat],8)])


plot(ktl$long,ktl$lat,cex=5,pch=19,col=tim.colors(10)[cut(ktl$precip,10)])