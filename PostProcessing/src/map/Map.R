### Packages ###
#install.packages(c("maps","mapdata","maptools","mapproj"))
#install.packages("PBSmapping")
#install.packages("ggplot2")
#install.packages("digest") #sometimes, I have to quit R, delete the digest directory from the library folder, then reinstall
#devtools::install_github("dkahle/ggmap")
#install.packages("RgoogleMaps")
#install.packages('rgdal')
#install.packages("colorspace")


library(maps)       #basic mapping functions and some data
library(mapdata)    #some additional hires data
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)
library(rgdal)
library(colorspace)


#setwd("~/Hardrive_Red/Graduate Work 2 copy/Classwork/Fall 2018/Map")
#######################################################################################
# PBS Mapping
#######################################################################################
#install.packages("PBSmapping")
rm(list=ls())
graphics.off()

library(PBSmapping) #powerful mapping functions developed by Pacific Biological Station
data(nepacLL)
data(nepacLLhigh)

#plotMap(nepacLL, xlim=c(-129, -115.6), ylim=c(43, 51.1),col="green",bg="lightblue")
color_palette<- function (n, h = c(360, -154), c = 100, l = c(43, 82), power = 1.5, 
                          fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- c[1L]
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, -1, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                       C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                               h[2L])), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}



col2 <- color_palette(7)
col <- c('#B88A00','#50A315','#00AD9A','#009ADE','#C86DD7')

pdf(file="AKMap3.pdf", width=9, height=9)

mat <- matrix(data = c(1,1,1,1,1,1,1,
                       1,2,1,3,1,4,1,
                       1,5,1,6,1,7,1,
                       1,1,1,1,1,1,1), ncol=7, nrow=4, byrow=TRUE)

par(oma=c(0,0,0,0),mar=c(0, 0, 0, 0), family = 'sans')
lay <- layout(mat=mat,  heights=c(3,1.5,1.5,1.5), widths=c(1.75,2.5,0,2.5,0,2.5,1))
#layout.show(4)



plotMap(nepacLLhigh, xlim=c(-170, -125), ylim=c(35, 62.5),col="white", bg=col2[4], cex.lab=2,
        cex.axis=1.35,tckLab = FALSE)
mtext('-160', side = 1, line = 3, outer = FALSE, at = c(-160,48),
      adj = NA, padj = NA, cex = 1)
mtext('-140', side = 1, line = 3, outer = FALSE, at = c(-140,48),
      adj = NA, padj = NA, cex = 1)
mtext('42', side = 2, line = 2, outer = FALSE, at = c(-167,42),
      adj = NA, padj = NA, cex = 1)
mtext('62', side = 2, line = 2, outer = FALSE, at = c(-167,62),
      adj = NA, padj = NA, cex = 1)




locations <- read.csv("Locations.csv")
samples <- read.csv("AKSealSeaLionMAP.csv")
length(samples$UAM.ID)
samples.se <- subset(samples, Region == 'SE')
samples.bb <- subset(samples, Region == 'BB')
samples.sc <- subset(samples, Region == 'SC')
samples.il <- subset(samples, Region == 'IL')
samples.wg <- subset(samples, Region == 'WG')
samples.eg <- subset(samples, Region == 'EG')



t.2 <- adjustcolor(col[2], alpha.f=0.5)
t.1 <- adjustcolor(col[1], alpha.f=0.5)
t.6 <- adjustcolor(col='darkblue', alpha.f=0.75)
t.3 <- adjustcolor(col[3], alpha.f=0.5)
t.4 <- adjustcolor(col[4], alpha.f=0.5)
t.5 <- adjustcolor(col[5], alpha.f=0.75)
t.14 <- adjustcolor(col[7], alpha.f=1)

points(subset(samples.se, Long> -140)$Long, subset(samples.se, Long> -140)$Lat, pch=16, col=paste(t.2), cex=3)
points(samples.sc$Long, samples.sc$Lat, pch=16, col=paste(t.1), cex=3)
points(samples.bb$Long, samples.bb$Lat, pch=16, col=paste(t.3), cex=3)

points(samples.il$Long, samples.il$Lat, pch=16, col=paste(t.4), cex=3)
points(samples.wg$Long, samples.wg$Lat, pch=16, col=paste(t.5), cex=3)
points(samples.eg$Long, samples.eg$Lat, pch=16, col=paste(t.6), cex=3)




#points(locations$Long, locations$Lat, pch=18, col='yellow', cex=2)
#text(locations$Long[5], locations$Lat[5], labels=expression(paste(bold("Aialik"))), pos=2, cex=1.25)
#text(locations$Long[1], locations$Lat[1], labels=expression(paste(bold("Tugidak"))), pos=1, cex=1.25)
#text(locations$Long[2], locations$Lat[2], labels=expression(paste(bold("PWS"))), pos=4, cex=1.25)
#text(locations$Long[3], locations$Lat[3], labels=expression(paste(bold("Nanvak"))), pos=1, cex=1.25)
#text(locations$Long[6], locations$Lat[6], labels=expression(paste(bold("Glacier Bay"))), pos=3, cex=1.25)
#text(locations$Long[4], locations$Lat[4], labels=expression(paste(bold("Sitka"))), pos=2, cex=1.25)

SC <- read.csv("SC.csv")
SC[is.na(SC)] <- 0

SE <- read.csv("SE.csv")
SE[is.na(SE)] <- 0

BB <- read.csv("BB.csv")
BB[is.na(BB)] <- 0

IL <- read.csv("IL.csv")
IL[is.na(IL)] <- 0

WG <- read.csv("WG.csv")
WG[is.na(WG)] <- 0

EG <- read.csv("EG.csv")
EG[is.na(EG)] <- 0


library(png)
hs.bb <- readPNG("harborseal.3.png")
hs.pws <- readPNG("harborseal.1.png")
hs.se <- readPNG("harborseal.2.png")
hs.il <- readPNG("harborseal.4.png")

sl.wg <- readPNG("sealion.5.png")
sl.eg <- readPNG("sealion.6.png")

text(-169, 48.5, labels="Sample n", las=1, srt=90, cex=1.25)

par(mai=c(0.25, 0.25, 0.25, 0.1))
barplot(BB$Total, ylab = "", xlab="", col=col[3], 
      ylim = c(0,20), main = "Bristol Bay Stock", cex.lab=1.5, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(hs.bb, 6,14,8.5,17.5, interpolate = TRUE)

#axis(1, at = NULL, labels = c(1950,1960,1970,1980,1990,2000))

barplot(SC$Total, ylab = "", xlab="", col= col[1], 
        ylim = c(0,20), main = "PWS Stock", cex.lab=1.5, 
        cex.axis = 1.25, cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(hs.pws, 6,14,8.5,17.5, interpolate = TRUE)


barplot(SE$Total, ylab = "", xlab="", col= col[2], 
        ylim = c(0,20), main = "Southeast Stocks", cex.lab=1, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(hs.se, 4,14,6.5,17.5, interpolate = TRUE)

barplot(IL$Total, ylab = "", xlab="", col= col[4], 
        ylim = c(0,20), main = "Iliamna Lake", cex.lab=1, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(hs.il, 6,14,8.5,17.5, interpolate = TRUE)


barplot(WG$Total, ylab = "", xlab="", col= col[5],
        names.arg = WG$Decade, ylim = c(0,20), main = "Western Stock", cex.lab=1.5, 
        cex.axis = 1.25, cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(sl.wg, 4,13,6.5,18, interpolate = TRUE)

mtext('Decade', side = 1, line = 9, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex = 1.1)


barplot(EG$Total, ylab = "", xlab="", col= "darkblue", 
        ylim = c(0,20), main = "Eastern Stock", cex.lab=1, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2)
rasterImage(sl.eg, 5,13,7.5,18, interpolate = TRUE)

#legend(175, 12, fill=c(col[2], col[5]),c('Salish Sea', 'Coastal'), col=c(col[5], col[2]






dev.off()

#library("colorspace") 
#pal <- choose_palette()
#pal(color_pa)









