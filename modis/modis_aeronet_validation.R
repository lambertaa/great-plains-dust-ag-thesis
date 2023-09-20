
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

raster::plotRGB((rgb*0.0010000000474974513), 3,2,1)
#plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- raster::as.data.frame(rgb, xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 25 & y <= 50 & x >= -130 & x <= -85)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","blue","green","red")
#scale factor
modis.scale.factor <- 0.0010000000474974513
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

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#subsetting dates for modis data
# min.i <- which(modis.date == "2008-01-01")
max.i <- which(modis.date == "2018-12-31")
modis.file.path <- modis.file.path[1:max.i]
modis.yeardoy <- modis.yeardoy[1:max.i]

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

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




#set working directory
setwd("~/AERONET/aeronet_ag")
#get filenames
file_list <- list.files()

#read in all files and combine into dat.aero 
dat.aero <- NULL
for (i in seq_along(file_list)){
  #i=10
  #pull out ith filename
  fname <- file_list[i]
  #read file, skip first 6 rows, read in first 53 columns, set 1st row as column names
  input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T, select = c(1:53))
  #because of weird error, first column doesn't have a name, pull out 2nd through 53 column names and use those as names
  header <- names(input)[2:53]
  input <- input[,1:52]
  colnames(input) <- header
  #change all -999 values to NA
  input[input == -999] <- NA
  #combine date and time
  input$Date <- as.POSIXct(paste(input$Date_.dd.mm.yyyy., input$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
  # reorder - put date/time at front 
  input2 <- input %>%
    dplyr::select(Date, everything())
  #apply 15 minute smoothing - average every 15 minutes
  input3 <- input2 %>%
    group_by(Date = cut(Date, breaks="15 min")) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  #change time to posixct
  input3$Date <- as.POSIXct(input3$Date)
  #create sitename var
  input3$SiteName <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$Date))
  #use rbind to add to dat.bind df
  dat.aero <- rbind(dat.aero, input3)
  print(paste0('finished ', fname))
}


rm(i,input,input2,input3,file_list,fname,header)

#rename columns to easier names
colnames(dat.aero)[colnames(dat.aero)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.aero)[colnames(dat.aero)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.aero)[colnames(dat.aero)=="Total_AOD_500nm.tau_a."] <- "AOD_total"
colnames(dat.aero)[colnames(dat.aero)=="Site_Longitude.Degrees."] <- "Longitude"
colnames(dat.aero)[colnames(dat.aero)=="Site_Latitude.Degrees."] <- "Latitude"

#select from AOD_fine and AOD_coarse
aero.var.choices <- c("AOD_coarse","AOD_fine","AOD_total")
aero.var.sel <- "AOD_coarse"

#get var name for plot titles
if (aero.var.sel == aero.var.choices[1]) {
  var.string <- "Coarse AOD"
} else if (aero.var.sel == aero.var.choices[2]) {
  var.string <- "Fine AOD"
} else if (aero.var.sel == aero.var.choices[3]) {
  var.string <- "Total AOD"
}

#2019 data is incomplete, subset to all years before 2019
dat.aero <- subset(dat.aero, year(dat.aero$Date) < 2019)

#require at least 1 observation per day per year
obs.req <- 365

# write function to count observations for network and check if it meets required annual observations for at least 7 years
check.yearly.obs <- function(x,req,var) {
  #count all non NA observations for each site for each year
  network.obs <- x %>%
    group_by(year(Date), SiteName) %>%
    summarise_at(vars(var), ~sum(!is.nan(.)))
  
  #get maxiumum year gap for each site
  dat.lag <- network.obs %>%
    group_by(SiteName) %>%
    mutate(Diff = `year(Date)` - lag(`year(Date)`)) %>%
    summarise_at(vars(Diff), ~max(Diff, na.rm = T))
  
  #check how many years satisfy observation number requirement
  #which years have observational frequency greater than obs.req?
  network.obs$check <- network.obs[[var]] >= req
  
  #how many years satisfy this observational threshold at each site?
  year.check <<- network.obs %>%
    group_by(SiteName) %>%
    summarise_at(vars(check), ~sum(check))
  #add dat.lag$Diff vector to year.check dataframe
  year.check$diff <<- dat.lag$Diff
  
  #check if number of years that meet obs criteria - the number of years between years with data is at least 7 years. This is done
  #so to determine how continuous the data is
  year.check$bestcons <<- year.check$check - year.check$diff + 1
  year.check$pick <<- year.check$bestcons >= 7
  return(year.check)
}

check.yearly.obs(dat.aero, obs.req,aero.var.sel)

#check on sites with large gaps
test.site <- subset(dat.aero, SiteName == "cart_site")
ggplot(test.site, aes(x = Date, y = AOD_coarse)) + geom_point() + theme_bw()

#remove all data from before 2000 for table mountain because of 9 year gap
dat.aero <- dat.aero[!(dat.aero$SiteName == "cart_site" & year(dat.aero$Date) <= 1995), ]

#check on site observations again to see subsetted data meet criteria
check.yearly.obs(dat.aero, obs.req, aero.var.sel)

#create new sites vector with only sites that meet criteria
year.check.sub <- subset(year.check, pick == "TRUE")
aero.sites <- unique(year.check.sub$SiteName)

rm(year.check,year.check.sub,test.site)

#subset dat.sub to sites that only meet criteria
dat.aero <- dat.aero[dat.aero$SiteName %in% aero.sites, ]

#generate ascending years vector
aero.years <- sort(unique(year(dat.aero$Date)), decreasing = FALSE)

#get date into numeric
dat.aero$datenum <- as.numeric(dat.aero$Date)

#check distribution of data for test site
test.site <- subset(dat.aero, SiteName == aero.sites[1])
test.site <- test.site[!is.na(test.site$AOD_coarse),]
boxplot(test.site$AOD_coarse)
hist(test.site$AOD_coarse)

#number of seconds in a year - need this to convert from change/second to change/year later on
year.sec <- 60*60*24*365





#compare aero to modis dust obs
aero.lonlat.df <- data.frame("lon" = unique(dat.aero$Longitude),
                             "lat" = unique(dat.aero$Latitude))
my.sf.point <- st_as_sf(x = aero.lonlat.df, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
my.sp.point <- as(my.sf.point,"Spatial")
my.sp.point <- SpatialPoints(my.sp.point)

modis.lonlat <- data.frame(as.matrix(expand.grid(modis.lon,modis.lat)))
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
colnames(modis.lonlat) <- c("lon","lat")
modis.lonlat <- raster(modis.lonlat, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
modis.lonlat[] <- 1:ncell(modis.lonlat)
sp.r <- as(modis.lonlat, "SpatialPixelsDataFrame")
p <- rasterToPolygons(modis.lonlat)
proj4string(p) <- proj4string(my.sp.point)

coordinates(modis.lonlat) <- ~ lon + lat
modis.lonlat <- raster(modis.lonlat)


proj4string(modis.lonlat) <- proj4string(my.sp.point)

#!is.na(over(my.sp.point,p)
proj4string(sp.r) <- proj4string(my.sp.point)
over(my.sp.point,sp.r)

extract(modis.lonlat,my.sp.point)






loni <- which(colnames(dust.green) == -93.5)
lati <- which(rownames(dust.green) == 42.5)
tmp.dust <- c(dust.green[lati,loni,])
tmp.dust.df <- data.frame("Date" = modis.date,
                          "dust" = tmp.dust)

tmp.aero <- dat.aero[which(dat.aero$Longitude == aero.lonlat.df$lon[4] & dat.aero$Latitude == aero.lonlat.df$lat[4]),]
unique(tmp.aero$SiteName)
tmp.aero$AOD_coarse[is.nan(tmp.aero$AOD_coarse)] <- NA

tmp.aero.daymax <- tmp.aero%>%
  mutate(Date = floor_date(Date, unit = "day")) %>%
  group_by(Date) %>%
  summarise_at(vars(AOD_coarse), ~max(.,na.rm = T))
#tmp.aero.daymax$ymd <- as.Date(paste(tmp.aero.daymax$`year(Date)`,tmp.aero.daymax$`month(Date)`,tmp.aero.daymax$`day(Date)`, sep='-'))
tmp.aero.daymax <- subset(tmp.aero.daymax, Date >= min(modis.date) & Date <= max(modis.date))
tmp.aero.daymax[sapply(tmp.aero.daymax, is.infinite)] <- NA
tmp.aero.daymax$Date <- as.Date(tmp.aero.daymax$Date)

tmp.dates <- seq(as.Date(modis.date[1]), as.Date(modis.date[length(modis.date)]), by="days")
tmp.df <- data.frame("Date" = tmp.dates)

tmp.combo <- left_join(tmp.df,tmp.dust.df, by="Date")

tmp.combo<- left_join(tmp.combo, tmp.aero.daymax, by="Date")

tmp.combo <- tmp.combo[!is.na(tmp.combo$AOD_coarse),]


ggplot(tmp.combo) + 
  geom_point(aes(x = Date, y = AOD_coarse), color = "blue", size = 0.5) + 
  geom_point(aes(x = Date, y = dust), color = "red", size = 0.5) + 
  theme_bw()

dust.sum <- sum(!is.na(tmp.combo$dust))
aero.dust.sum <- sum(tmp.combo$AOD_coarse >= 0.05 & !is.na(tmp.combo$dust), na.rm = T)
aero.dust.sum/dust.sum

aero.dust.cor <- cor.test(tmp.combo$dust,tmp.combo$AOD_coarse, use="pairwise.complete.obs")
aero.dust.cor$estimate
aero.dust.cor$p.value
