
rm(list=ls())

library(lubridate)
library(dplyr)
library(data.table)
library(foreign)
library(ggplot2)
library(tidyr)
library(broom)
library(maps)
library(quantreg)
#library(ggmap)
library(zoo)
library(boot)
library(infer)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(raster)
library(RANN)
library(rgeos)
library(openair)
library(gridExtra)
library(reshape2)

#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- raster::brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

raster::plotRGB((rgb*0.0010000000474974513), 3,2,1)
plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- raster::as.data.frame(rgb, xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 25 & y <= 50 & x >= -130 & x <= -85)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","blue","green","red")
#scale factor
modis.scale.factor <- 0.00100000004749745
#multiply measurements by scale factor to get true AOD value
modis.df.tmp.sub.test[,3:5] <- modis.df.tmp.sub.test[,3:5] * modis.scale.factor
#create xtabs matrix from df
#modis.matrix.tmp.test <- xtabs(green ~ lat + lon, data = modis.df.tmp.sub)

#get states map
states <- map_data("state")

ggplot() + geom_tile(data = modis.df.tmp.sub.test, aes(x = lon, y = lat, fill = green)) +
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "gray") +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -85),ylim = c(25,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14)) +
  ggtitle(paste0("Trends in 75th Quantile Non-Spherical AOD (865 nm) 2000-2016"))



# get year directories in MODIS_tif
modis.years <- list.files()

#function to pull out all characters after 37 in string which will give parameter name for every file with extension
substrRight <- function(x, n){
  substr(x, n, nchar(x))
}

modis.file.path <- NULL
modis.yeardoy <- NULL
#for loop to get all filenames, paths, and dates
for (i in 1:length(modis.years)) {
  #i = 1
  #pull out dates - they come in yyyydoy format
  tmp.files <- list.files(modis.years[i])
  
  #get parameter names from each file and index where parameter name matches desired parameter
  modis.file.parameter.names <- substrRight(tmp.files,37)
  modis.file.ind <- which(modis.file.parameter.names == "land_aod_mean_3bands.tif")
  
  #pull out file names that contain desired parameter
  parameter.files <- tmp.files[modis.file.ind]
  
  #create path with filename to call for reading in files
  tmp.file.path <- paste0(modis.years[i],"/",parameter.files)
  
  #get year and doy from filename to use for time
  tmp.yeardoy <- substr(parameter.files,start = 11, stop = 17)
  
  #get filepath and doy information only for summer fire season (doy 170-220)
  # tmp.summer.ind <- which(as.numeric(substrRight(tmp.yeardoy,5)) >= 170 & as.numeric(substrRight(tmp.yeardoy,5)) <= 220)
  # tmp.yeardoy <- tmp.yeardoy[tmp.summer.ind]
  # tmp.file.path <- tmp.file.path[tmp.summer.ind]
  
  #add temporary file paths and yeardoy to full vectors
  modis.file.path <- append(modis.file.path, tmp.file.path, after = length(modis.file.path))
  modis.yeardoy <- append(modis.yeardoy, tmp.yeardoy, after = length(modis.yeardoy))
}



# read in files and store in modis.array

#create empty array
#get unique lat and lon from test modis dataframe
modis.lat <- sort(unique(modis.df.tmp.sub.test$lat), decreasing = FALSE)
modis.lon <- unique(modis.df.tmp.sub.test$lon)
modis.array.blue <- array(numeric(),c(length(modis.lat), 
                                      length(modis.lon), 
                                      length(modis.file.path))) 
modis.array.green <- array(numeric(),c(length(modis.lat), 
                                       length(modis.lon), 
                                       length(modis.file.path)))
modis.array.red <- array(numeric(),c(length(modis.lat), 
                                     length(modis.lon), 
                                     length(modis.file.path)))

#scale factor
modis.scale.factor <- 0.00100000004749745

for (i in 1:length(modis.file.path)) {
  #i = 1
  tmp.rgb <- raster::brick(modis.file.path[i])
  #put data in dataframe
  modis.df.tmp <- raster::as.data.frame(tmp.rgb, xy = T)
  #subset to desired region by lat and long
  modis.df.tmp.sub <- subset(modis.df.tmp, y >= 25 & y <= 50 & x >= -130 & x <= -85)
  #bands are blue - 470, green - 550, red - 660, name columns appropiately
  names(modis.df.tmp.sub) <- c("lon","lat","blue","green","red")
  #multiply measurements by scale factor to get true AOD value
  modis.df.tmp.sub[,3:5] <- modis.df.tmp.sub[,3:5] * modis.scale.factor
  #create xtabs matrix from df
  #modis.matrix.tmp <- xtabs(green ~ lat + lon, data = modis.df.tmp.sub)
  modis.matrix.tmp.blue <- acast(modis.df.tmp.sub, lat~lon, value.var = 'blue')
  modis.matrix.tmp.green <- acast(modis.df.tmp.sub, lat~lon, value.var = 'green')
  modis.matrix.tmp.red <- acast(modis.df.tmp.sub, lat~lon, value.var = 'red')
  modis.array.blue[,,i] <- modis.matrix.tmp.blue
  modis.array.green[,,i] <- modis.matrix.tmp.green
  modis.array.red[,,i] <- modis.matrix.tmp.red
  
  print(paste0("finished ",modis.yeardoy[i]))
}

colnames(modis.array.blue) <- modis.lon
colnames(modis.array.green) <- modis.lon
colnames(modis.array.red) <- modis.lon
rownames(modis.array.blue) <- modis.lat
rownames(modis.array.green) <- modis.lat
rownames(modis.array.red) <- modis.lat



#now read in climate data
setwd("~/Climate_data")

#open the nc file
ncin <- nc_open("daily_vwnd.nc")

dname <- "vwnd"

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

#get soil moisture var
var_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(var_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

nc_close(ncin)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, "hours since ")[[1]][2]
# tdstr <- strsplit(unlist(tustr)[3], "-")
# tmonth <- as.integer(unlist(tdstr)[2])
# tday <- as.integer(unlist(tdstr)[3])
# tyear <- as.integer(unlist(tdstr)[1])
# timenew <- chron(time,origin=c(tmonth, tday, tyear))
timenew <- as.POSIXct(time*3600, origin = tustr)

# replace netCDF fill values with NA's
var_array[var_array==fillvalue$value] <- NA

# get a single slice or layer (January)
m <- 1
var_slice <- var_array[,,m]

# quick map
#image(lon,lat,soil_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(0,0.5,1,5,10,15,20,25)
levelplot(var_slice ~ lon * lat, data=grid, at=cutpts, cuts=8, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- data.frame(expand.grid("lon" = lon,"lat" = lat))
dim(lonlat)

# # vector of `tmp` values
# var_vec <- as.vector(var_slice)
# length(var_vec)
# 
# # create dataframe and add names
# var_df01 <- data.frame(cbind(lonlat,var_vec))
# names(var_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
# head(na.omit(var_df01), 10)

#get uwnd
ncin <- nc_open("daily_uwnd.nc")
dname <- "uwnd"
var_array1 <- ncvar_get(ncin,dname)
var_array1[var_array==fillvalue$value] <- NA
nc_close(ncin)

total_wind <- sqrt((var_array^2) + (var_array1^2))
rm("var_array","var_array1","var_vec","tyear","tustr","tmonth","time","tday","nt","nlon","nlat","m","dname","cutpts","tunits","var_df01","var_slice","title","tdstr","references","institution","history","grid","dunits","dlname","datasource","Conventions")

#have to pull out only points that match AERONET sites
tmp.lat <- unique(var.quant$Latitude)
tmp.long <- unique(var.quant$Longitude)
improve.latlon <- data.frame("lon" = tmp.long, "lat" = tmp.lat)
rm("tmp.lat","tmp.long")
improve.latlon$lon <- 360 + improve.latlon$lon 
# aero.lonlat.sp <- SpatialPoints(aero.lonlat)
# climo.lonlat.sp <- SpatialPoints(data.frame(lonlat))
# 
# aero.lonlat$nearest <- apply(gDistance(climo.lonlat.sp, aero.lonlat.sp,byid=TRUE),1,which.min)
# 
# close.test <- lonlat[aero.lonlat$nearest,]
# close.test

#test <- rdist.earth(aero.lonlat,lonlat,miles = FALSE)

distp1p2 <- function(p1,p2) {
  dst <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(dst)
}

dist2b <- function(y) which.min(apply(lonlat, 1, function(x) min(distp1p2(x,y))))
#apply(aero.lonlat, 1, dist2b)

climo.improve.lonlat <- lonlat[apply(improve.latlon, 1, dist2b),]

for (i in 1:nrow(climo.improve.lonlat)) {
  #i = 1
  climo.var.lon <- which(lon == climo.improve.lonlat[i,1])
  climo.var.lat <- which(lat == climo.improve.lonlat[i,2])
  climo.var <- total_wind[climo.var.lon,climo.var.lat,]
  climo.var <- data.frame("Date" = timenew, "wspd" = climo.var)
  
  #site.var$Date <- as.Date(site.var$Date, "%Y/%m/%d")
  
  improve.site <- subset(var.quant, Longitude == unique(var.quant$Longitude)[i] & Latitude == unique(var.quant$Latitude)[i])
  climo.var <- subset(climo.var, year(Date) >= min(year(improve.site$Date)) & year(Date) <= max(year(improve.site$Date)))
  improve.site$Date <- as.Date(improve.site$Date, "%Y/%m/%d %h:%M:%s")
  
  tmp.site.improve.climo <- NULL
  for (j in 1:nrow(improve.site)) {
    #j = 1
    tmp.ind <- which(climo.var$Date == improve.site$Date[j])
    climovar.ave <- mean(climo.var$wspd[(tmp.ind - 12):(tmp.ind - 1)], na.rm = TRUE)
    #tmp.min <- min(abs(difftime(improve.site$Date[j],climo.var$Date,units = "hours")))
    #tmp.var.time <- climo.var[which.min(abs(difftime(improve.site$Date[j],climo.var$Date,units = "hours"))),]
    tmp.improvevar.climovar <- data.frame("improvedate" = improve.site$Date[j],
                                          "improvevar" = improve.site[[var.sel]][j],
                                          #"climodate" = tmp.var.time$Date,
                                          "climo_3day_ave" = climovar.ave)
    #"timedif" = tmp.min)
    tmp.site.improve.climo <- rbind(tmp.site.improve.climo,tmp.improvevar.climovar)
  }
  
  tmp.site.name <- improve.site$SiteName[1]
  tmp.climo.3day.ave <- climo.var %>%
    group_by(Date = cut(Date, breaks="3 days")) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  # savePlot <- function(myPlot) {
  #   pdf(paste0(tmp.site,"_winds_quantile.png"))
  #   print(myPlot)
  #   dev.off()
  # }
  
  
  myPlot <- ggplot(tmp.climo.3day.ave, aes(wspd)) +
    geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
    geom_histogram(data = tmp.site.improve.climo, aes(climo_3day_ave), binwidth = 0.5, fill = "red") +
    theme_bw() +
    labs(x = "Wind Speed (m/s)", y = "Count", title = paste0(tmp.site.name," - ",q.string,"th Quantile ",var.string)) +
    scale_y_continuous(trans = 'log2')
  
  ggsave(filename = paste0(tmp.site.name,"_",q.string,"_",var.sel,"_winds_quantile.png"),plot = myPlot)
  #savePlot(myPlot)
  
  #combos <- sapply(climo.var$Date,function(x) abs(x-aero.site$time_utc))
  # combos <- abs(outer(aero.site$time_utc,climo.var$Date,FUN="-"))
  # min(combos)
  # test <- which(combos <= "180",arr.ind=TRUE)
}
rm("climo.var.lon","climo.var.lat")
