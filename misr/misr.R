rm(list=ls())

library(ncdf4)
library(maps)
library(fields)
library(lubridate)
library(dplyr)


setwd("/home/andylambert")
ncname <- "MISR_test"
ncfname <- paste(ncname, ".nc", sep = "")
dname <- "tmp"  # note: tmp means temperature (not temporary)

# open a NetCDF file
ncin <- nc_open(ncfname)
lat <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Latitude")
long <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Longitude")
elevation <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Elevation")
year <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Year")
month <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Month")
day <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Day")
hour <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Hour")
minute <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Minute")
nonsphere <- ncvar_get(ncin,"4.4_KM_PRODUCTS/Nonspherical_Aerosol_Optical_Depth")

plot(lat[200,])

time <- as.POSIXct(paste(year,month,day,hour,minute), format="%Y-%m-%d %H:%M")


lat.test <- c(lat)
long.test <- c(long)
nonsphere.test <- c(nonsphere)
data.test <- data.frame('lat' = lat.test,'long' = long.test, 'nonsphere' = nonsphere.test)
data.test <- data.test[complete.cases(lat.test), ]


world <- map_data("world")

all.plot <- ggplot() + geom_polygon(data = world, aes(x=long,y=lat, group = group), fill=NA, color="black") +
 # coord_fixed(xlim = c(-130, -95),ylim = c(27,50),ratio = 1.3)
  coord_fixed(ratio = 1.3)
all.plot +
  geom_point(data=data.test, aes(x=long, y = lat, color = ns_aod))

