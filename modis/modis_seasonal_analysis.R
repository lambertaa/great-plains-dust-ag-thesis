
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
library(maptools)
library(maps)
library(rgdal)
library(reshape2)
library(scales)

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)


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

#get maps
states <- map_data("state")
canada = map_data("world","Canada")
mexico = map_data("world","Mexico")


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

# #generate name of every land_aod_mean_3bands.tif from each year
# modis.parameter.name <- "land_aod_mean_3bands.tif"
# modis.product.name <- "MOD08_D3.A"
# #pull out dates - they come in yyyydoy format
# tmp.files <- list.files(modis.years[1])
# tmp.yeardoy <- substr(tmp.files,start = 11, stop = 17)
# #get publish date
# tmp.publish <- substr(tmp.files,start = 23, stop = 35)
# #version
# modis.version <- "061"
# #put it all together to get filenames for desired parameters in MODIS_tif directory
# modis.filename <- paste0(modis.product.name,tmp.yeardoy,".",modis.version,".",tmp.publish,"_",modis.parameter.name)


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

#subsetting dates for modis data
#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))
# min.i <- which(modis.date == "2008-01-01")
# max.i <- which(modis.date == "2018-12-31")
# modis.file.path <- modis.file.path[min.i:max.i]
# modis.yeardoy <- modis.yeardoy[min.i:max.i]


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

# get angstrom exponent to isolate dust observations
angstrom <- -(log(modis.array.blue/modis.array.red))/(log(470/660))


#subset dust obs by setting all non dust obs equal to NA
dust.blue <- modis.array.blue
dust.green <- modis.array.green
dust.red <- modis.array.red
for (i in 1:nrow(angstrom)) {
  for (j in 1:ncol(angstrom)) {
    #i = 15
    #j = 22
    tmp.dat <- angstrom[i,j,]
    tmp.dust.ind <- which(tmp.dat > 0.75)
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}

wintermonths <- c(12,1,2)
springmonths <- c(3,4,5)
summermonths <- c(6,7,8)
fallmonths <- c(9,10,11)
winter.i <- which(month(modis.date) == wintermonths[1] | month(modis.date) == wintermonths[2] | month(modis.date) == wintermonths[3])
spring.i <- which(month(modis.date) == springmonths[1] | month(modis.date) == springmonths[2] | month(modis.date) == springmonths[3])
summer.i <- which(month(modis.date) == summermonths[1] | month(modis.date) == summermonths[2] | month(modis.date) == summermonths[3])
fall.i <- which(month(modis.date) == fallmonths[1] | month(modis.date) == fallmonths[2] | month(modis.date) == fallmonths[3])
seasons <- c("Winter","Spring","Summer","Fall")
#get quantile for each year at each gridpoint and trends in quantile
modis.seas.ave <- NULL
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 5
    #k = 15
    point.seas.ave <- NULL
    for (i in 1:4) {
      #i = 1
      if (i == 1) {
        tmp.ind <- winter.i
      } else if (i == 2) {
        tmp.ind <- spring.i
      } else if (i == 3) {
        tmp.ind <- summer.i
      } else {
        tmp.ind <- fall.i
      }
      tmp.dat <- dust.green[j,k,tmp.ind]
      tmp.dat[is.nan(tmp.dat)] <- NA
      tmp.ave <- mean(tmp.dat, na.rm = T)
      tmp.df <- data.frame("lat" = modis.lat[j],
                           "lon" = modis.lon[k],
                           "season" = seasons[i], 
                           "ave" = tmp.ave)
      point.seas.ave <- rbind(point.seas.ave, tmp.df)
      #point.quant.obs <- point.quant.obs[complete.cases(point.quant.obs), ]
    }
    modis.seas.ave <- rbind(modis.seas.ave,point.seas.ave)
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
  }
}

modis.seas.max <- NULL
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 5
    #k = 15
    tmp.ind <- which(modis.seas.ave$lat == modis.lat[j] & modis.seas.ave$lon == modis.lon[k])
    tmp.dat <- modis.seas.ave[tmp.ind,]
    #tmp.dat$ave[is.nan(tmp.dat$ave)] <- NA
    max.ind <- which.max(tmp.dat$ave)
    max.row <- tmp.dat[max.ind,]
    modis.seas.max <- rbind(modis.seas.max,max.row)
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
  }
}


#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = modis.seas.max$lat,"long" = modis.seas.max$lon)))

#filter to get only us tiles
modis.us.sub <- modis.seas.max[point.filter,]

#plot season max and average
  ggplot(data = modis.us.sub, aes(x = lon, y = lat, fill = season, color = season)) + geom_tile(aes(alpha = ave)) +
  scale_fill_manual(values = c("blue","green","red","orange")) +
  scale_color_manual(values = c("blue","green","red","orange"), guide = FALSE) +
  scale_alpha_continuous(range = c(0,1)) +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(alpha = "Average AOD",
       fill = "Season")+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))
  
