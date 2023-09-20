
#clear environments
rm(list=ls())

#load packages
library("EnvStats", lib.loc="/usr/local/lib/R/site-library")
library("lubridate", lib.loc="/usr/local/lib/R/site-library")
library("dplyr", lib.loc="/usr/local/lib/R/site-library")
library("data.table", lib.loc="/usr/local/lib/R/site-library")
library("foreign", lib.loc="/usr/local/lib/R/site-library")
library("tidyr", lib.loc="/usr/local/lib/R/site-library")
library("broom", lib.loc="/usr/local/lib/R/site-library")
library("maps", lib.loc="/usr/local/lib/R/site-library")
library("quantreg", lib.loc="/usr/local/lib/R/site-library")
library("zoo", lib.loc="/usr/local/lib/R/site-library")
library("gridExtra", lib.loc="/usr/local/lib/R/site-library")
library("mblm", lib.loc="/usr/local/lib/R/site-library")
library(sp)
#library(maptools)
library(maps)
library(rgdal)
library(reshape2)

#analyze modis data
setwd("/home/andylambert/cropped_rasters")

GDALinfo("cdl2008_reclass_cropped.tif")
r <- raster("cdl2008_reclass_cropped.tif")
#coord <- xyFromCell(r)
#plot(r)

us <- getData("GADM", country="USA", level=1)
states.sel <- c("montana","wyoming","colorado","south dakota","north dakota","nebraska","kansas","oklahoma","missouri","iowa","minnesota")
states = us[match(toupper(states.sel),toupper(us$NAME_1)),]

#test <- over(r,states)
test <- extract(r, states)




#r.df <- raster::as.data.frame(r, xy = T)
#r.df <-as.data.frame(getValues(r))
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 25 & y <= 50 & x >= -130 & x <= -85)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","blue","green","red")