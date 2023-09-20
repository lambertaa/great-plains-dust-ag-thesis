
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

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

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
modis.file.path <- head(modis.file.path,-1)


# read in files and store in modis.array

#create empty array
#get unique lat and lon from test modis dataframe
modis.lat <- sort(unique(modis.df.tmp.sub.test$lat), decreasing = FALSE)
modis.lon <- unique(modis.df.tmp.sub.test$lon)
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.lonlat.df <- as.data.frame(modis.lonlat)

#output modis coordinate grid as raster
# coordinates(modis.lonlat.df) <- ~Var1 + Var2
# proj4string(modis.lonlat.df) <- proj4string(rgb)
# plot(modis.lonlat.df)
# test <- as(modis.lonlat.df,"SpatialPointsDataFrame")
# plot(test)
# test <- SpatialPointsDataFrame(modis.lonlat.df)
# test <- SpatialPolygons(modis.lonlat.df)
# 
# 
# #retry
# r <- r[[1]]
# r[is.na(r[])] <- 0
# rgb.r <- rasterToPolygons(r)
# spplot(rgb.r)
# raster::shapefile(rgb.r,"modisraster.shp", overwrite = TRUE)
# 
# 
# 
# 
# setwd("/home/andylambert/grad_deliverables")
# writeOGR(test, dsn = '.', layer = "poly", driver = "ESRI Shapefile")
# setwd("/home/andylambert/MODIS_tif")

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

#read in angstrom exponent
modis.file.path.ang <- NULL
#for loop to get all filenames, paths, and dates
for (i in 1:length(modis.years)) {
  #i = 1
  #pull out dates - they come in yyyydoy format
  tmp.files <- list.files(modis.years[i])
  
  #get parameter names from each file and index where parameter name matches desired parameter
  modis.file.parameter.names <- substrRight(tmp.files,37)
  modis.file.ind <- which(modis.file.parameter.names == "deepblue_land_ang_mean.tif")
  
  #pull out file names that contain desired parameter
  parameter.files <- tmp.files[modis.file.ind]
  
  #create path with filename to call for reading in files
  tmp.file.path <- paste0(modis.years[i],"/",parameter.files)
  
  #add temporary file paths to full vector
  modis.file.path.ang <- append(modis.file.path.ang, tmp.file.path, after = length(modis.file.path.ang))
}

#create empty array
#get unique lat and lon from test modis dataframe
modis.array.ang <- array(numeric(),c(length(modis.lat), 
                                      length(modis.lon), 
                                      length(modis.file.path.ang))) 

#read in angstrom tif and store in modis.array.ang
for (i in 1:length(modis.file.path.ang)) {
  #i = 1
  tmp.rgb <- raster::raster(modis.file.path.ang[i])
  #put data in dataframe
  modis.df.tmp <- raster::as.data.frame(tmp.rgb, xy = T)
  #subset to desired region by lat and long
  modis.df.tmp.sub <- subset(modis.df.tmp, y >= 25 & y <= 50 & x >= -130 & x <= -85)
  #bands are blue - 470, green - 550, red - 660, name columns appropiately
  names(modis.df.tmp.sub) <- c("lon","lat","angstrom")
  #multiply measurements by scale factor to get true AOD value
  modis.df.tmp.sub[,3] <- modis.df.tmp.sub[,3] * modis.scale.factor
  modis.matrix.tmp.ang <- acast(modis.df.tmp.sub, lat~lon, value.var = 'angstrom')
  modis.array.ang[,,i] <- modis.matrix.tmp.ang
  print(paste0("finished ",modis.yeardoy[i]))
}

colnames(modis.array.ang) <- modis.lon
rownames(modis.array.ang) <- modis.lat






# get angstrom exponent to isolate dust observations
angstrom <- -(log(modis.array.blue/modis.array.red))/(log(470/660))


#subset dust obs by setting all non dust obs equal to NA
# dust.blue <- modis.array.blue
# dust.green <- modis.array.green
# dust.red <- modis.array.red
# for (i in 1:nrow(modis.array.ang)) {
#   for (j in 1:ncol(modis.array.ang)) {
#     #i = 15
#     #j = 22
#     tmp.dat <- modis.array.ang[i,j,]
#     tmp.dust.ind <- which(tmp.dat > 0.75)
#     dust.blue[i,j,tmp.dust.ind] <- NA
#     dust.green[i,j,tmp.dust.ind] <- NA
#     dust.red[i,j,tmp.dust.ind] <- NA
#   }
# }

rm(modis.df.tmp,modis.df.tmp.sub,modis.df.tmp.sub.test,modis.df.tmp.test,modis.matrix.tmp.ang,modis.matrix.tmp.blue,modis.matrix.tmp.green,modis.matrix.tmp.red)
# subset dust obs by setting all non dust obs equal to NA
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

#get background trend for all modis observations
#get observations in desired quantile for each gridpoint
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.trend.df.allobs <- data.frame(modis.lonlat)
colnames(modis.trend.df.allobs) <- c("lon","lat")
modis.trend.df.allobs[,"slope"] <- NA
modis.trend.df.allobs[,"pval"] <- NA
modis.trend.df.allobs[,"normslope"] <- NA

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#get quantile for each year at each gridpoint and trends in quantile
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 10
    #k = 15
    point.obs <- dust.green[j,k,]
    tmp.ken <- kendallTrendTest(point.obs ~ modis.date, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
    tmp.ken.68 <- kendallTrendTest(point.obs ~ modis.date, na.action = na.pass, ci.slope = TRUE, conf.level = 0.68)
    tmp.95.ci <- tmp.ken$interval$limits * 365
    tmp.68.ci <- tmp.ken.68$interval$limits *365
    tmp.norm.95.ci <- (tmp.95.ci/median(point.obs, na.rm = TRUE))*100
    tmp.norm.68.ci <- (tmp.68.ci/median(point.obs, na.rm = TRUE))*100
    tmp.slope <- tmp.ken$estimate[2] * 365
    tmp.p <- tmp.ken$p.value
    aod.ind <- which(modis.trend.df.allobs$lon == modis.lon[k] & modis.trend.df.allobs$lat == modis.lat[j])
    modis.trend.df.allobs[aod.ind,3] <- tmp.slope
    modis.trend.df.allobs[aod.ind,4] <- tmp.p
    modis.trend.df.allobs[aod.ind,5] <- (tmp.slope/median(point.obs, na.rm = TRUE))*100
    modis.trend.df.allobs[aod.ind,6] <- tmp.norm.68.ci[1]
    modis.trend.df.allobs[aod.ind,7] <- tmp.norm.68.ci[2]
    modis.trend.df.allobs[aod.ind,8] <- tmp.norm.95.ci[1]
    modis.trend.df.allobs[aod.ind,9] <- tmp.norm.95.ci[2]
    # } else {
    #   aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
    #   modis.trend.df[aod.ind,3] <- NA
    #   modis.trend.df[aod.ind,4] <- NA
    #   modis.trend.df[aod.ind,5] <- NA
    # }
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
  }
}

colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V6"] <- "l68"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V7"] <- "h68"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V8"] <- "l95"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V9"] <- "h95"


test.sub <- modis.trend.df.allobs[416,]
limy <- max(abs(test.sub[8:9]))

ggplot(data = data.frame(x = c(0,19), y = c(limy*-19,limy*19))) +
  geom_blank() +
  geom_ribbon(aes(x=c(0,19), ymin = test.sub$l68*x, ymax = test.sub$h68*x), fill = "lightgrey", alpha = 0.8) +
  geom_ribbon(aes(x=c(0,19), ymin = test.sub$l95*x, ymax = test.sub$h95*x), fill = "lightgrey", alpha = 0.4) +
  geom_abline(slope = test.sub$l68, intercept = 0) +
  geom_abline(slope = test.sub$h68, intercept = 0) +
  geom_abline(slope = test.sub$l95, intercept = 0) +
  geom_abline(slope = test.sub$h95, intercept = 0) +
  geom_abline(slope = test.sub$normslope, intercept = 0, color = ifelse(test.sub$normslope >= 0, "red" , "blue")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(0,19), breaks = c(0,6,11,16), expand = c(0,0), labels = c(2000,2005,2010,2015)) +
  ylim(limy*-19,limy*19) +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), legend.title = element_text(size=14), title = element_text(size=14)) +
  xlab("Date") +
  ylab("Change (%)") +
  #ggtitle("40.5, -108.5") +
  theme(plot.title = element_text(size=6))


#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = modis.trend.df.allobs$lat,"long" = modis.trend.df.allobs$lon)))

#filter to get only us tiles
modis.us.sub.allobs <- modis.trend.df.allobs[point.filter,]

#make ss vector for statistical significane
modis.us.sub.allobs$ss <- modis.us.sub.allobs$pval <= 0.05
modis.us.sub.allobs$ss[!modis.us.sub.allobs$ss] <- NA

#get maps
canada = map_data("world","Canada")
mexico = map_data("world","Mexico")

modis.plot <- ggplot(data = modis.us.sub.allobs, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.us.sub.allobs[!is.na(modis.us.sub.allobs$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-5,5), oob = squish) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
  #ggtitle(paste0("Trends in MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot

#subset and plot region
modis.mw.sub <- subset(modis.us.sub.allobs, lat >= 40 & lat <= 50 & lon >= -113 & lon <= -97)

modis.plot.sub <- ggplot(data = modis.mw.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.mw.sub[!is.na(modis.mw.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-113, -97),ylim = c(40,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot.sub
