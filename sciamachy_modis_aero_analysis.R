#clear environments
rm(list=ls())

#load packages
library("lubridate", lib.loc="/usr/local/lib/R/site-library")
library("dplyr", lib.loc="/usr/local/lib/R/site-library")
#library("mblm", lib.loc="/usr/local/lib/R/site-library")
library(sp)
library(maps)
library(rgdal)
library(reshape2)
library(scales)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)

dname <- "xCH4"

#open netcdf
setwd("~/")
ncin <- nc_open("./sciamachy/SCIA_v72_final_monthly_all.nc")
#store variables
lon <- ncvar_get(ncin, varid = "lon")
lat <- ncvar_get(ncin, varid = "lat")
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
#store ch4 variable and associate info
#units not reported - but in nmol/mol
ch4.array <- ncvar_get(ncin,"xCH4")
dlname <- ncatt_get(ncin,"xCH4","long_name")
dunits <- ncatt_get(ncin,"xCH4","units")
fillvalue <- ncatt_get(ncin,"xCH4","_FillValue")

#converting time variable
tustr <- strsplit(tunits$value, " ")
# tdstr <- strsplit(unlist(tustr)[3], "-")
# tmonth <- as.integer(unlist(tdstr)[2])
# tday <- as.integer(unlist(tdstr)[3])
# tyear <- as.integer(unlist(tdstr)[1])
# chron(time,origin=c(tmonth, tday, tyear))
date <- format(as.Date('2003-01-15') %m+% months(time), "%Y-%m")
hist(ch4.array)

#subset to desired region
#lonlat <- as.data.frame(expand.grid(lon,lat))
#lonlat.ind <- which(lonlat$lat > 37 & lonlat$lat < 49 & lonlat$lon > -116 & lonlat$lon < -103)

lat.ind <- which(lat > 24 & lat < 50)
lon.ind <- which(lon > -125 & lon < -66)

ch4.array.sub <- ch4.array[lon.ind,lat.ind,]
rownames(ch4.array.sub) <- lon[lon.ind]
colnames(ch4.array.sub) <- lat[lat.ind]

#take a look at a quick map of a timeslice for the region
ch4.slice <- ch4.array.sub[,,2]
lonlat.region <- as.matrix(expand.grid(lon[lon.ind],lat[lat.ind]))
ch4.vec <- as.vector(ch4.slice)
# create dataframe and add names
ch4.df <- data.frame(cbind(lonlat.region,ch4.vec))
names(ch4.df) <- c("lon","lat","CH4")
#image(lon[lon_ind],lat[lat_ind],ch4_slice, col=rev(brewer.pal(10,"RdBu")))

states <- map_data("state")

ggplot() + geom_tile(data = ch4.df, aes(x = lon, y = lat, fill = CH4)) +
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "gray") +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-125, -66),ylim = c(25,50),ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude")




#read in modis data
#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- raster::brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")
#r <- raster("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

raster::plotRGB((rgb*0.0010000000474974513), 3,2,1)
plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- raster::as.data.frame(rgb, xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 26 & y <= 50 & x >= -124 & x <= -68)
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
  coord_fixed(xlim = c(-125, -66),ylim = c(25,50),ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude")



# get year directories in MODIS_tif
modis.years <- list.files()
#only want through 2003-2012 to compare to 
modis.years <- modis.years[which(as.numeric(modis.years) <= 2018)]
#modis.years <- modis.years[which(as.numeric(modis.years) >= 2003 & as.numeric(modis.years) <= 2012)]

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
#modis.file.path <- head(modis.file.path,-1)


# read in files and store in modis.array

#create empty array
#get unique lat and lon from test modis dataframe
modis.lat <- sort(unique(modis.df.tmp.sub.test$lat), decreasing = FALSE)
modis.lon <- unique(modis.df.tmp.sub.test$lon)
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.lonlat.df <- as.data.frame(modis.lonlat)

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
  modis.df.tmp.sub <- subset(modis.df.tmp, y >= 26 & y <= 50 & x >= -124 & x <= -68)
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

#rename columns and rownames to match longitude and latitude respectively
colnames(modis.array.blue) <- modis.lon
colnames(modis.array.green) <- modis.lon
colnames(modis.array.red) <- modis.lon
rownames(modis.array.blue) <- modis.lat
rownames(modis.array.green) <- modis.lat
rownames(modis.array.red) <- modis.lat

# get angstrom exponent to isolate dust observations
angstrom <- -(log(modis.array.blue/modis.array.red))/(log(470/660))

# subset dust obs by setting all non dust obs equal to NA
dust.blue <- modis.array.blue
dust.green <- modis.array.green
dust.red <- modis.array.red
for (i in 1:nrow(angstrom)) {
  for (j in 1:ncol(angstrom)) {
    #i = 15
    #j = 22
    tmp.dat <- angstrom[i,j,]
    #select threshold for angstrom exponent
    tmp.dust.ind <- which(tmp.dat > 0.5)
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}


#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#subset to desired region
modis.lat.ind <- which(modis.lat > 25 & modis.lat < 51)
modis.lon.ind <- which(modis.lon > -127 & modis.lon < -65)

modis.sub <- modis.array.green[modis.lat.ind,modis.lon.ind,]

#take a look at a quick map of a timeslice for the region
modis.slice <- modis.sub[,,1]
modis.lonlat.region <- as.matrix(expand.grid(modis.lat[modis.lat.ind],modis.lon[modis.lon.ind]))
modis.vec <- as.vector(modis.slice)
# create dataframe and add names
modis.df <- data.frame(cbind(modis.lonlat.region,modis.vec))
names(modis.df) <- c("lat","lon","green")
#image(lon[lon_ind],lat[lat_ind],ch4_slice, col=rev(brewer.pal(10,"RdBu")))

states <- map_data("state")

ggplot() + geom_tile(data = modis.df, aes(x = lon, y = lat, fill = green)) +
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "gray") +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -85),ylim = c(25,50),ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude")




#resampling modis data to match sciamachy resolution
#only do this for sciamachy dates (we will have to average to monthly later)
modis.sci.date.ind <- which(modis.date >= "2003-01-01" & modis.date <= "2012-03-31")
modis.date.sub <- modis.date[modis.sci.date.ind]

modis.sub.date.sub <- modis.sub[,,modis.sci.date.ind]


#aggregate from 1x1 degree to 4x4 degree (factor = 4)

#create sample aggregate to get correct dimensions for empty array
gridded(modis.df) <- ~lon+lat
modis.raster <- raster(modis.df)
modis.raster.aggregate <- aggregate(modis.raster, fact=4)
modis.matrix.slice <- acast(as.data.frame(modis.raster.aggregate, xy=T), y~x, value.var = 'green')

#create empty array with lat,long,time dimensions to store resampled data
modis.array.green.resample <- array(numeric(),c(dim(modis.matrix.slice)[1], 
                                       dim(modis.matrix.slice)[2], 
                                       length(modis.date.sub)))

#for loop to run aggregation for each date and store into empty array
for (i in 1:length(modis.date.sub)) {
  #i=1
  tmp.slice <- modis.sub[,,i]
  tmp.vec <- as.vector(tmp.slice)
  # create dataframe and add names
  tmp.df <- data.frame(cbind(modis.lonlat.region,tmp.vec))
  names(tmp.df) <- c("lat","lon","green")
  gridded(tmp.df) <- ~lon+lat
  tmp.raster <- raster(tmp.df)
  tmp.raster.aggregate <- aggregate(tmp.raster,fact=4)
  tmp.matrix <- acast(as.data.frame(tmp.raster.aggregate,xy=T), y~x, value.var='green')
  modis.array.green.resample[,,i] <- tmp.matrix
  print(paste0("finished ", modis.date.sub[i]))
}

#year, month dataframe for array averaging
modis.date.sub.year.month <- data.frame("year" = year(modis.date.sub),
                                        "month" = month(modis.date.sub),
                                        "day" = day(modis.date.sub))

modis.date.monthly <- NULL
u.years <- unique(modis.date.sub.year.month$year)
u.months <- unique(modis.date.sub.year.month$month)
modis.monthly.means <- array(numeric(),c(dim(modis.matrix.slice)[1], 
                                         dim(modis.matrix.slice)[2], 
                                         length(u.years)*length(u.months)))
for (i in 1:length(u.years)) {
  for (j in 1:length(u.months)) {
    #i = 1
    #j = 1
    if (((i-1)*12)+j <= 111) {
      tmp.date.ind <- which(modis.date.sub.year.month$year == u.years[i] & modis.date.sub.year.month$month == u.months[j])
      tmp.array.sub <- modis.array.green.resample[,,tmp.date.ind]
      tmp.means <- rowMeans(tmp.array.sub, dims = 2, na.rm = T)
      modis.monthly.means[,,(((i-1)*12)+j)] <- tmp.means
      tmp.date <- as.Date(paste0(u.years[i],"-",u.months[j],"-","15"))
      modis.date.monthly <- append(modis.date.monthly,tmp.date)
      print(paste0("finished ",tmp.date)) 
    }
    else
      break
  }
}
rownames(modis.monthly.means) <- rownames(modis.matrix.slice)
colnames(modis.monthly.means) <- colnames(modis.matrix.slice)
#only keep the dates that match sciamachy
modis.monthly.means <- modis.monthly.means[,,c(1:111)]


modis.dust.freq <- array(numeric(),c(dim(modis.matrix.slice)[1], 
                                         dim(modis.matrix.slice)[2], 
                                         111))

#get monthly frequency of dust obs
for (i in 1:dim(modis.array.green.resample)[1]) {
  for (j in 1:dim(modis.array.green.resample)[2]) {
    #i = 1
    #j = 1
    tmp.modis <- c(modis.array.green.resample[i,j,])
    tmp.df <- data.frame("date" = modis.date.sub,
                         "dust_aod" = tmp.modis)
    tmp.freq <- tmp.df %>%
      group_by(year(date),month(date)) %>%
      summarise_at(vars(dust_aod), ~sum(!is.nan(.)))
    modis.dust.freq[i,j,] <- tmp.freq$dust_aod
  }
}
rownames(modis.dust.freq) <- rownames(modis.matrix.slice)
colnames(modis.dust.freq) <- colnames(modis.matrix.slice)



#correlation time!
dust.ch4.cor <- NULL
for (i in 1:dim(modis.monthly.means)[1]) {
  for (j in 1:dim(modis.monthly.means)[2]) {
    #i = 1
    #j = 1
    tmp.modis <- c(modis.monthly.means[i,j,])
    #ch4 array has opposite dimensions
    tmp.ch4 <- c(ch4.array.sub[j,i,])
    if (sum(is.nan(tmp.modis)) > 105 | sum(is.na(tmp.ch4)) > 105) {
      tmp.df <-  data.frame("lon" = as.numeric(colnames(modis.monthly.means)[j]),
                            "lat" = as.numeric(rownames(modis.monthly.means)[i]),
                            "cor" = NA,
                            "pval" = NA)
    } else {
      tmp.cor <- cor.test(tmp.modis, tmp.ch4, use = "pairwise.complete.obs", method = "pearson")
      tmp.df <- data.frame("lon" = as.numeric(colnames(modis.monthly.means)[j]),
                           "lat" = as.numeric(rownames(modis.monthly.means)[i]),
                           "cor" = tmp.cor$estimate,
                           "pval" = tmp.cor$p.value)
      dust.ch4.cor <- rbind(dust.ch4.cor,tmp.df)
    }
    print(paste0("finished ",tmp.df$lon," ",tmp.df$lat))
  } 
}

#make ss vector for statistical significane <= 0.1
dust.ch4.cor$ss <- dust.ch4.cor$pval <= 0.05
dust.ch4.cor$ss[!dust.ch4.cor$ss] <- NA

ggplot(data = dust.ch4.cor, aes(x = lon, y = lat, fill = cor)) + geom_tile() +
  geom_tile(data = dust.ch4.cor[!is.na(dust.ch4.cor$ss), ], aes(color = ss), size = 0.25) + 
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", breaks = c(-0.6, -0.3, 0, 0.3, 0.6)) +
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-124, -67),ylim = c(25,50),ratio = 1.3) +
  labs(fill = "r" ) +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))



#correlation time!
dust.freq.ch4.cor <- NULL
for (i in 1:dim(modis.dust.freq)[1]) {
  for (j in 1:dim(modis.dust.freq)[2]) {
    #i = 1
    #j = 1
    tmp.modis <- c(modis.dust.freq[i,j,])
    #ch4 array has opposite dimensions
    tmp.ch4 <- c(ch4.array.sub[j,i,])
    if (sum(is.nan(tmp.modis)) > 105 | sum(is.na(tmp.ch4)) > 105) {
      tmp.df <-  data.frame("lon" = as.numeric(colnames(modis.dust.freq)[j]),
                            "lat" = as.numeric(rownames(modis.dust.freq)[i]),
                            "cor" = NA,
                            "pval" = NA)
    } else {
      tmp.cor <- cor.test(tmp.modis, tmp.ch4, use = "pairwise.complete.obs", method = "pearson")
      tmp.df <- data.frame("lon" = as.numeric(colnames(modis.dust.freq)[j]),
                           "lat" = as.numeric(rownames(modis.dust.freq)[i]),
                           "cor" = tmp.cor$estimate,
                           "pval" = tmp.cor$p.value)
      dust.freq.ch4.cor <- rbind(dust.freq.ch4.cor,tmp.df)
    }
    print(paste0("finished ",tmp.df$lon," ",tmp.df$lat))
  } 
}

#make ss vector for statistical significane <= 0.1
dust.freq.ch4.cor$ss <- dust.freq.ch4.cor$pval <= 0.05
dust.freq.ch4.cor$ss[!dust.freq.ch4.cor$ss] <- NA

ggplot(data = dust.freq.ch4.cor, aes(x = lon, y = lat, fill = cor)) + geom_tile() +
  geom_tile(data = dust.ch4.cor[!is.na(dust.ch4.cor$ss), ], aes(color = ss), size = 0.25) + 
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", breaks = c(-0.6, -0.3, 0, 0.3, 0.6)) +
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-124, -67),ylim = c(25,50),ratio = 1.3) +
  labs(fill = "r" ) +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
