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



col <- color_palette(7)


pdf(file="AKMap.pdf", width=9, height=7)

mat <- matrix(data = c(1,1,1,1,1,1,1,
                       1,2,1,3,1,4,1,
                       1,1,1,1,1,1,1), ncol=7, nrow=3, byrow=TRUE)

lay <- layout(mat=mat,  heights=c(3,1.5,1.15), widths=c(1.5,3,-0.75,3,-0.75,3,1))
#layout.show(4)

par(oma=c(0,0,0,0), mar=c(0,0,0,0))
par(family = 'sans')

plotMap(nepacLLhigh, xlim=c(-165, -130), ylim=c(50, 62.5),col="white", bg=col[4], cex.lab=2,
        cex.axis=1.35,tckLab = FALSE)
mtext('-160', side = 1, line = 3, outer = FALSE, at = c(-160,48),
      adj = NA, padj = NA, cex = 1)
mtext('-140', side = 1, line = 3, outer = FALSE, at = c(-140,48),
      adj = NA, padj = NA, cex = 1)
mtext('52', side = 2, line = 2, outer = FALSE, at = c(-167,52),
      adj = NA, padj = NA, cex = 1)
mtext('62', side = 2, line = 2, outer = FALSE, at = c(-167,62),
      adj = NA, padj = NA, cex = 1)



par(mar=c(7,10,5,5))
locations <- read.csv("Locations.csv")
samples <- read.csv("AK Samples.csv")
length(samples$UAM.ID)
samples.se <- subset(samples, Region == 'SE')
samples.bb <- subset(samples, Region == 'BB')
samples.sc <- subset(samples, Region == 'SC')



t.1 <- adjustcolor(col[7], alpha.f=0.5)
t.2 <- adjustcolor(col[1], alpha.f=0.5)
t.3 <- adjustcolor(col='darkblue', alpha.f=0.5)
t.14 <- adjustcolor(col[7], alpha.f=1)

points(samples.se$Long, samples.se$Lat, pch=16, col=paste(t.2), cex=3)
points(samples.sc$Long, samples.sc$Lat, pch=16, col=paste(t.1), cex=3)
points(samples.bb$Long, samples.bb$Lat, pch=16, col=paste(t.3), cex=3)
points(locations$Long, locations$Lat, pch=18, col='yellow', cex=2)
text(locations$Long[5], locations$Lat[5], labels=expression(paste(bold("Aialik"))), pos=2, cex=1.25)
text(locations$Long[1], locations$Lat[1], labels=expression(paste(bold("Tugidak"))), pos=1, cex=1.25)
text(locations$Long[2], locations$Lat[2], labels=expression(paste(bold("PWS"))), pos=4, cex=1.25)
text(locations$Long[3], locations$Lat[3], labels=expression(paste(bold("Nanvak"))), pos=1, cex=1.25)
text(locations$Long[6], locations$Lat[6], labels=expression(paste(bold("Glacier Bay"))), pos=3, cex=1.25)
text(locations$Long[4], locations$Lat[4], labels=expression(paste(bold("Sitka"))), pos=2, cex=1.25)

SC <- read.csv("SC.csv")
SC[is.na(SC)] <- 0

SE <- read.csv("SE.csv")
SE[is.na(SE)] <- 0

BB <- read.csv("BB.csv")
BB[is.na(BB)] <- 0

text(-162, 53.5, labels="Sample n", las=1, srt=90, cex=1.25)

barplot(BB$Total, ylab = "", xlab="", col='darkblue', 
      ylim = c(0,20), main = "Bristol Bay Stock", cex.lab=1.5, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2, line=1)

#axis(1, at = NULL, labels = c(1950,1960,1970,1980,1990,2000))

barplot(SC$Total, ylab = "", xlab="", col= col[7], 
        names.arg = SC$Decade, ylim = c(0,25), main = "PWS Stock", cex.lab=1.5, 
        cex.axis = 1.25, cex.names = 1.25, cex.main=1.25, las=2, line=1)

mtext('Decade', side = 1, line = 9, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex = 1.1)

barplot(SE$Total, ylab = "", xlab="", col= col[1], 
        ylim = c(0,20), main = "Southeast Stocks", cex.lab=1, cex.axis = 1.25, 
        cex.names = 1.25, cex.main=1.25, las=2, line=1)


#legend(175, 12, fill=c(col[2], col[5]),c('Salish Sea', 'Coastal'), col=c(col[5], col[2]






dev.off()

#library("colorspace") 
#pal <- choose_palette()
#pal(color_pa)









