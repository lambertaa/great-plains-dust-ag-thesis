
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
#only want through 2018
modis.years <- modis.years[which(as.numeric(modis.years) <= 2018)]

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
    tmp.dust.ind <- which(tmp.dat > 0.75)
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}


#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#name modis.lonlat.df columns
colnames(modis.lonlat.df) <- c("Lon", "Lat")

#trend analysis for MODIS dust AOD
modis.trend.df <- data.frame(modis.lonlat)
colnames(modis.trend.df) <- c("lon","lat")
modis.trend.df[,"slope"] <- NA
modis.trend.df[,"pval"] <- NA
modis.trend.df[,"normslope"] <- NA

#change yeardoy to Date format
#modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#subset to most recent years (since 2010)
time.ind <- which(year(modis.date) >= 2008)
modis.date.sub <- modis.date[time.ind]
dust.green.sub <- dust.green[,,time.ind]

#get quantile for each year at each gridpoint and trends in quantile

modis.trend.df <- NULL
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 1
    #k = 19
    tmp.dat <- dust.green.sub[j,k,]
    tmp.ken <- kendallTrendTest(tmp.dat ~ modis.date.sub, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
    #multiply by 365 to get in terms of change per year
    tmp.slope <- tmp.ken$estimate[2] * 365
    tmp.p <- tmp.ken$p.value
    #aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
    tmp.df <- data.frame("Longitude" = modis.lon[k],
                         "Latitude" = modis.lat[j],
                         #divide by median to normalize change and multiply by 100 to get percent change
                         "normslope" = tmp.slope/median(tmp.dat, na.rm = TRUE)*100,
                         "pval" = tmp.p)
    #modis.trend.df[aod.ind,3] <- tmp.slope
    #modis.trend.df[aod.ind,4] <- tmp.p
    #modis.trend.df[aod.ind,5] <- (tmp.slope/median(point.quant.obs$aod_quant, na.rm = TRUE))*100
    #j = 5
    #k = 15
    modis.trend.df <- rbind(modis.trend.df,tmp.df)
    print(paste0("finished ", modis.lat[j], " ", modis.lon[k]))
  }
}

#get selected states
states.ag <- subset(states, #region == "wyoming" | 
                    #region == "colorado" | 
                    #region == "montana" | 
                    region == "north dakota" |
                      region == "south dakota" |
                      region == "nebraska" |
                      region == "kansas" |
                      region == "oklahoma" |
                      #region == "arkansas" |
                      region == "missouri" |
                      region == "iowa" |
                      region == "minnesota")

#make ss vector for statistical significane
modis.trend.df$ss <- modis.trend.df$pval <= 0.05
modis.trend.df$ss[!modis.trend.df$ss] <- NA

#get ag states by themselves
#us <- getData("GADM", country="USA", level=1)
us <- as_Spatial(read_sf("~/cb_2018_us_state_500k.shp"))
agstates = us[match(toupper(c(unique(states.ag$region))),toupper(us$NAME)),]
#agstates = us[match(toupper(c("North Dakota","South Dakota","Nebraska","Kansas","Oklahoma","Missouri","Iowa","Minnesota")),toupper(us$NAME_1)),]
CRS.new <- proj4string(agstates)
spdf <- SpatialPointsDataFrame(coords = modis.trend.df[, c("Longitude", "Latitude")], data = modis.trend.df)
#proj4string = CRS.new)
proj4string(spdf) <- CRS.new

modis.trend.df.region.b <- spdf[!is.na(over(spdf, as(agstates, "SpatialPolygons"))), ]
modis.trend.df.region.b <- as.data.frame(modis.trend.df.region.b)

ggplot(data = modis.trend.df.region, aes(x = Longitude, y = Latitude, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.trend.df.region[!is.na(modis.trend.df.region$ss), ], aes(color = ss), size = 0.25) + 
  scale_fill_gradientn(colours = c("blue","white","red"),
                       values = rescale(c(-5,0,5)),
                       guide = "colorbar", limits = c(-5,5),
                       na.value = "transparent",
                       oob = squish) +
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, limits = c(-1,1), colours = c("blue","red")) +
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +#, oob = squish) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  #geom_polygon(data = ut.map, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-104, -89),ylim = c(34,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))

modis.trend.df.region.b$sign <- NA
for (i in 1:nrow(modis.trend.df.region.b)) {
  if (modis.trend.df.region.b$normslope[i] >= 0) {
    modis.trend.df.region.b$sign[i] <- "Positive"
  } else {
    modis.trend.df.region.b$sign[i] <- "Negative"
  }
}

modis.trend.df.region.b$sign <- as.factor(modis.trend.df.region.b$sign)

modis.trend.df.region %>%
  ggplot(aes(x=pval, fill=sign)) +
  geom_histogram(position='identity', binwidth=0.025) +
  scale_fill_manual(values=c("Positive" = "red", "Negative" = "blue")) +
  theme_bw() +
  labs(fill="")

ggplot(modis.trend.df.region, aes(pval)) +
  stat_ecdf(geom="step") +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  stat_ecdf(data = modis.trend.df.region.b, geom="step", aes(pval), color = "light blue")

