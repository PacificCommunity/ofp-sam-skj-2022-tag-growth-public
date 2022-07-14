######################################
# Maps for tag recapture datasets
######################################

# Created on: 14 June 2022
# Created by: Jed Macdonald

## Packages
library(maps)
library(maptools)
library(mapproj)
library(rgdal)
library(sf)


# Set up map features
WPO_map<-map(database = "world2", exact = F, boundary = F, 
             resolution = 0, fill=T, col="grey75", xlim=c(110,280), ylim =c(-60,55), 
             mar = c(4.1, 7.1, 4.1, 6.1), type="l", plot=F)
map(database = "world2", regions=WPO_map$names[c(1:2, 7:500)], exact = F, boundary = F, 
    resolution = 0, fill=T, col="grey75", xlim=c(110,280), ylim =c(-60,55),
    mar = c(4.1, 7.1, 4.1, 6.1), type="l")

## Overlay EEZs
eez<-read.table("EZNEW2.txt", header=F)
lines(eez[,1], eez[,2], lwd=0.5, col="lightgrey")

# Read in tag-recapture dayta 
jdat<-read.csv("tags_jp.csv") # screened JPTP data: 304 records
spcdat<-read.csv("tags_spc.csv") # screened PTTP?RTTP data: 6752 records

# Plot release locations
points(x=jdat$Rellon, y=jdat$Rellat, col="red", cex=0.3)
points(x=spcdat$rel.lon, y=spcdat$rel.lat, col="blue", cex=0.3)

# Plot recapture locations
points(x=jdat$Reclon, y=jdat$Reclat, col="red", pch=16, cex=0.3)
points(x=spcdat$rec.lon, y=spcdat$rec.lat, col="blue", pch=16, cex=0.3)

## Plot map axes
map.axes(cex.axis=0.8, las=1)
