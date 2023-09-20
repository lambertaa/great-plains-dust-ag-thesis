
rm(list=ls())

library(dplyr)
library(MODISTools)
#library("gdalUtils", lib.loc="/usr/local/lib/R/site-library")
#library("rgdal", lib.loc="/usr/local/lib/R/site-library")
#library("raster", lib.loc="/usr/local/lib/R/site-library")
library(gdalUtils)
library(rgdal)
library(raster)
library(gdalUtilities)


setwd("/home/andylambert/MODISdata")

sds <- get_subdatasets("MOD08_D3/2000/055/MOD08_D3.A2000055.061.2017276160246.hdf")

#gdalinfo <- GDALinfo("MOD08_D3/2000/055/MOD08_D3.A2000055.061.2017276160246.hdf", returnScaleOffset = FALSE)
#metadata <- attr(gdalinfo, "subdsmdata")

#sds <- metadata[grep("Aerosol_Optical_Depth_Land_Ocean_Mean", metadata)[1]]
