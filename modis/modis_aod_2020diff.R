
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
library(ggmap)
library(GISTools)
library(tigris)

#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- raster::brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")
#r <- raster("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

#plot single band and multiply by scale factor = 0.0010000000474974513
raster::plotRGB((rgb*0.0010000000474974513), 3,2,1)
#plot all bands
plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- raster::as.data.frame(rgb, xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 25 & y <= 50 & x >= -130 & x <= -65)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","blue","green","red")
#scale factor
modis.scale.factor <- 0.00100000004749745
#multiply measurements by scale factor to get true AOD value
modis.df.tmp.sub.test[,3:5] <- modis.df.tmp.sub.test[,3:5] * modis.scale.factor

#get states map and store in states
states <- map_data("state")

#plot test modis data
ggplot() + geom_tile(data = modis.df.tmp.sub.test, aes(x = lon, y = lat, fill = green)) +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -65),ylim = c(25,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14))



# get year directories in MODIS_tif
modis.years <- list.files()
#remove usmap file from modis.years
modis.years <- modis.years[!(modis.years == "gadm36_USA_1_sp.rds")]

#function to pull out all characters after 37 in string which will give parameter name for every file with extension
substrRight <- function(x, n){
  substr(x, n, nchar(x))
}

modis.file.path <- NULL
modis.yeardoy <- NULL
#for loop to get all filenames, paths, and dates
for (i in 1:length(modis.years)) {
  #i = 1
  #get temporary list of files for the year
  tmp.files <- list.files(modis.years[i])
  
  #get parameter names from each file and index where parameter name matches desired parameter
  modis.file.parameter.names <- substrRight(tmp.files,37)
  #get index for files that match parameter name we are looking to analyze
  modis.file.ind <- which(modis.file.parameter.names == "land_aod_mean_3bands.tif")
  
  #pull out file names that contain desired parameter
  parameter.files <- tmp.files[modis.file.ind]
  
  #create path with filename to call for reading in files
  tmp.file.path <- paste0(modis.years[i],"/",parameter.files)
  
  #get year and doy from filename to use for time
  tmp.yeardoy <- substr(parameter.files,start = 11, stop = 17)
  
  #subset days (March 15 thru April 15)
  #tmp.yeardoy.sub <- which(as.numeric(tmp.yeardoy) >= as.numeric(paste0(modis.years[i],"074")) & as.numeric(tmp.yeardoy) <= as.numeric(paste0(modis.years[i],"105")))
  
  #get filepath and doy information only for certain days of year (doy 170-220)
  tmp.sub.ind <- which(as.numeric(substrRight(tmp.yeardoy,5)) >= 71 & as.numeric(substrRight(tmp.yeardoy,5)) <= 131)
  tmp.yeardoy <- tmp.yeardoy[tmp.sub.ind]
  tmp.file.path <- tmp.file.path[tmp.sub.ind]
  
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
#create modis grid with lat and lon
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.lonlat.df <- as.data.frame(modis.lonlat)

#create empty arrays to store modis data in for loop - lat by lon by dates/files
modis.array.blue <- array(numeric(),c(length(modis.lat), 
                                      length(modis.lon), 
                                      length(modis.file.path))) 
modis.array.green <- array(numeric(),c(length(modis.lat), 
                                       length(modis.lon), 
                                       length(modis.file.path)))
modis.array.red <- array(numeric(),c(length(modis.lat), 
                                     length(modis.lon), 
                                     length(modis.file.path)))

# for loop to read in all tif files and store blue, green, red AOD bands into their own matrices
for (i in 1:length(modis.file.path)) {
  #i = 1
  #get temporary raster data from tif file
  tmp.rgb <- raster::brick(modis.file.path[i])
  #put data in dataframe
  modis.df.tmp <- raster::as.data.frame(tmp.rgb, xy = T)
  #subset to desired region by lat and long
  modis.df.tmp.sub <- subset(modis.df.tmp, y >= 25 & y <= 50 & x >= -130 & x <= -65)
  #bands are blue - 470, green - 550, red - 660, name columns appropiately
  names(modis.df.tmp.sub) <- c("lon","lat","blue","green","red")
  #multiply measurements by scale factor to get true AOD value
  modis.df.tmp.sub[,3:5] <- modis.df.tmp.sub[,3:5] * modis.scale.factor
  #create xtabs matrix from df
  #modis.matrix.tmp <- xtabs(green ~ lat + lon, data = modis.df.tmp.sub)
  
  #create matrix from temporary dataframe for each band
  modis.matrix.tmp.blue <- acast(modis.df.tmp.sub, lat~lon, value.var = 'blue')
  modis.matrix.tmp.green <- acast(modis.df.tmp.sub, lat~lon, value.var = 'green')
  modis.matrix.tmp.red <- acast(modis.df.tmp.sub, lat~lon, value.var = 'red')
  
  #put matrix into appropriate place in previously empty array
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


# get angstrom exponent to isolate dust observations for green and red bands
angstrom <- -(log(modis.array.blue/modis.array.red))/(log(470/660))


#subset dust obs by setting all non dust obs equal to NA
dust.blue <- modis.array.blue
dust.green <- modis.array.green
dust.red <- modis.array.red
for (i in 1:nrow(angstrom)) {
  for (j in 1:ncol(angstrom)) {
    #i = 15
    #j = 22
    #temporary angstrom data
    tmp.dat <- angstrom[i,j,]
    #tmp.dust.ind <- which(tmp.dat < 0.75)
    
    #index which observations are greater than 0.75 AE
    tmp.dust.ind <- which(tmp.dat > 0.75)
    #set all indexed observations to NA
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}

#subset green band data
sub.green <- modis.array.green[,,which(as.numeric(modis.yeardoy) >= 2016071 & as.numeric(modis.yeardoy) <= 2019131)]

fullave <- rowMeans(modis.array.green, dims = 2, na.rm = T)
#fullave <- rowMeans(sub.green, dims = 2, na.rm = T)
#fullave <- rowMeans(dust.green, dims = 2, na.rm = T)

sub.2020 <- modis.array.green[,,which(as.numeric(modis.yeardoy) >= 2020071 & as.numeric(modis.yeardoy) <= 2020131)]
#sub.2020 <- dust.green[,,which(as.numeric(modis.yeardoy) >= 2020074 & as.numeric(modis.yeardoy) <= 2020105)]
ave.2020 <- rowMeans(sub.2020, dims = 2, na.rm = T)

ave.diff <- ave.2020 - fullave
ave.diff.norm <- (ave.diff/fullave)*100
full.sd <- apply(sub.2020, c(1,2), function (x) sd(x, na.rm = T))

ave.diff.df <- NULL
for (i in 1:nrow(ave.diff)) {
  for (j in 1:ncol(ave.diff)) {
    #i = 3
    #j = 15
    tmp.lat <- as.numeric(rownames(ave.diff)[i])
    tmp.lon <- as.numeric(colnames(ave.diff)[j])
    tmp.dat <- ave.diff[i,j]
    tmp.norm <- ave.diff.norm[i,j]
    tmp.sd <- full.sd[i,j]
    tmp.df <- data.frame("long" = tmp.lon,"lat" = tmp.lat,"aoddiff" = tmp.dat,"normdiff" = tmp.norm,"sd" = tmp.sd)
    ave.diff.df <- rbind(ave.diff.df,tmp.df)
  }
}

ave.diff.df$aoddiff[is.nan(ave.diff.df$aoddiff)] <- NA
#ave.diff.df$long <- as.numeric(ave.diff.df$long)
#ave.diff.df$lat <- as.numeric(ave.diff.df$lat)
ave.diff.df$sddiff <- ave.diff.df$aoddiff/ave.diff.df$sd

canada = map_data("world","Canada")
mexico = map_data("world","Mexico")

#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = ave.diff.df$lat,"long" = ave.diff.df$long)))

#filter to get only us tiles
ave.diff.us <- ave.diff.df[point.filter,]

ggplot(data = ave.diff.us, aes(x = long, y = lat, fill = aoddiff)) + geom_tile() +
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-0.2,0.2)) +#, oob = squish) +
  scale_fill_gradientn(colours = c("blue","white","red"),
                       values = rescale(c(-0.1,0,0.2)),
                       guide = "colorbar", limits = c(-0.1,0.2),
                       na.value = "transparent",
                       oob = squish) +
  theme_bw() +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-124, -68),ylim = c(26,49),ratio = 1.3) +
  labs(fill = "AOD Difference" ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("2020 - 21 Year Average AOD (2000-2020) (DOY 71-131)") +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14))

# ggplot(data = ave.diff.us, aes(x = long, y = lat, fill = normdiff)) + geom_tile() +
#   #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-0.2,0.2)) +#, oob = squish) +
#   scale_fill_gradientn(colours = c("blue","white","red"),
#                        values = rescale(c(-80,0,280)),
#                        guide = "colorbar", 
#                        limits = c(-80,280),
#                        na.value = "transparent",
#                        breaks = seq(-80,280,40)) +
#   theme_bw() +
#   geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
#   #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
#   coord_fixed(xlim = c(-124, -68),ylim = c(26,49),ratio = 1.3) +
#   labs(fill = "AOD Difference" ) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   ggtitle("2020 AOD - 21 Year Average AOD") +
#   theme(legend.text = element_text(size=12)) +
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14))
# my.lm = function(y, modis.date = modis.yeardoy) {
#   coef(lm(y ~ modis.date, na.action = na.omit))
# }
# 
# 
# apply(modis.array.green, c(1,2), function(x) my.lm(y = x))
# 
# lapply(function(x) lm)

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

years <- modis.years[1:(length(modis.years) - 1)]
#get annual average
annual.ave <- array(numeric(),c(length(modis.lat), 
                                length(modis.lon), 
                                length(years)))
for (i in 1:length(years)) {
  #i = 22
  year.ind <- which(year(modis.date) == years[i])
  tmp.dat <- modis.array.green[,,year.ind]
  #tmp.dat <- dust.green[,,year.ind]
  tmp.mean <- apply(tmp.dat, c(1,2), function (x) mean(x, na.rm = T))
  tmp.mean[is.nan(tmp.mean)] <-NA
  annual.ave[,,i] <- tmp.mean 
}
colnames(annual.ave) <- modis.lon
rownames(annual.ave) <- modis.lat

res.list <- list()
annual.trends <- NULL
for (i in 1:length(modis.lon)) {
  for (j in 1:length(modis.lat)) {
    #i = 25
    #j = 15
    tmp.dat <- annual.ave[j,i,]
    if (sum(!is.na(tmp.dat)) < 5) {
      tmp.df <- data.frame("Longitude" = modis.lon[i],
                           "Latitude" = modis.lat[j],
                           "slope" = NA,
                           "normslope" = NA,
                           "pval" = NA,
                           "minresyear" = NA,
                           "maxresyear" = NA)
    } else {
      tmp.lm <- lm(tmp.dat ~ as.numeric(years), na.action = na.omit)
      tmp.summ <- summary(tmp.lm)
      tmp.res <- resid(tmp.lm)
      year.min <- years[which.min(tmp.res)]
      year.max <- years[which.max(tmp.res)]
      tmp.p <- tmp.summ$coefficients[2,4]
      tmp.slope <- tmp.summ$coefficients[2,1]
      tmp.normslope <- (tmp.slope/median(tmp.dat, na.rm = T))*100
      tmp.df <- data.frame("Longitude" = modis.lon[i],
                           "Latitude" = modis.lat[j],
                           "slope" = tmp.slope,
                           "normslope" = tmp.normslope,
                           "pval" = tmp.p,
                           "minresyear" = year.min,
                           "maxresyear" = year.max) 
    }
    annual.trends <- rbind(annual.trends,tmp.df)
    #res.list[i] <- resid(tmp.lm)
  }
}


tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = annual.trends$Latitude,"long" = annual.trends$Longitude)))

#filter to get only us tiles
annual.trends.us <- annual.trends[point.filter,]

#make ss vector for statistical significane
annual.trends.us$ss <- annual.trends.us$pval <= 0.1
annual.trends.us$ss[!annual.trends.us$ss] <- NA

ggplot(data = annual.trends.us, aes(x = Longitude, y = Latitude, fill = slope, color = ss)) + geom_tile() +
  geom_tile(data = annual.trends.us[!is.na(annual.trends.us$ss), ], aes(color = ss), fill = "transparent", size = 0.25) +
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-0.2,0.2)) +#, oob = squish) +
  scale_fill_gradientn(colours = c("blue","white","red"),
                       values = rescale(c(-0.01,0.01)),
                       guide = "colorbar", limits = c(-0.01,0.01),
                       na.value = "transparent",
                       oob = squish) +
  theme_bw() +
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
 # scale_color_manual(values = c(`TRUE` = "black"), labels = c("TRUE" = "< 0.1"), guide = "legend")  +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-124, -68),ylim = c(26,49),ratio = 1.3) +
  labs(fill = "AOD/year",
       color = "P-Val") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("2000-2020 AOD Trends (DOY 81-112)") +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14))

annual.trends.us$diff <- NA
annual.trends.us$sddiff <- NA
for (i in 1:nrow(annual.trends.us)) {
  #i = 1
  index <- which(ave.diff.us$long == annual.trends.us$Longitude[i] & ave.diff.us$lat == annual.trends.us$Latitude[i])
  annual.trends.us$diff[i] <- ave.diff.us$aoddiff[index]
  annual.trends.us$sddiff[i] <- ave.diff.us$sddiff[index]
}

#make ss vector for statistical significane
annual.trends.us$yearres <- annual.trends.us$minresyear == 2020 | annual.trends.us$maxresyear == 2020
annual.trends.us$yearres[!annual.trends.us$yearres] <- NA

ggplot(data = annual.trends.us, aes(x = Longitude, y = Latitude, fill = sddiff)) + geom_tile() +
  geom_tile(data = annual.trends.us[!is.na(annual.trends.us$yearres), ], aes(color = yearres), fill = "transparent", size = 0.25) +
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-0.2,0.2)) +#, oob = squish) +
  scale_fill_gradientn(colours = c("blue","white","red"),
                       values = rescale(c(-1,0,1)),
                       guide = "colorbar", limits = c(-1,1),
                       na.value = "transparent",
                       oob = squish) +
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-2,2), oob = squish) +
  theme_bw() +
  scale_color_manual(values = c(`TRUE` = "black"), labels = c("2020"), guide = "legend")  +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-124, -68),ylim = c(26,49),ratio = 1.3) +
  labs(fill = "Difference (SD)",
       color = "Min/max residual (year)") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("2020 - 21 year average AOD (DOY 71-131)") +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14))
