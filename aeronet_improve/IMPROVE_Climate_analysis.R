
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



#set working directory as /IMPROVE
setwd("~/IMPROVE")

#read in data, keep header
data1 <- fread("IMPROVE.txt", stringsAsFactors = F, header = T, skip = 0)
data2 <- fread("IMPROVE2.txt", stringsAsFactors = F, header = T, skip = 0)

data <- rbind(data1,data2)

rm("data1","data2")

#call all -999 values NA
data[data == -999] <- NA

#rename columns for pm10, pm25, reconstructed, soil, coarse, total carbon
colnames(data)[colnames(data)=="SOILf:Value"] <- "soil_pm25"
colnames(data)[colnames(data)=="RCTM:Value"] <- "pm10_rec"
colnames(data)[colnames(data)=="MT:Value"] <- "pm10"
colnames(data)[colnames(data)=="TCf:Value"] <- "totalcarbon"
colnames(data)[colnames(data)=="MF:Value"] <- "pm25"
colnames(data)[colnames(data)=="CM_calculated:Value"] <- "coarse"
colnames(data)[colnames(data)=="RCFM:Value"] <- "pm25_rec"

#remove all obs with POC = 2
data <- data[!(data$POC==2),]

#optional remove low elevations sites
#data <- subset(data, Elevation >= 2000)
data <- subset(data, Elevation < 2000)

#get date vector into date format
data$Date <- as.Date(data$Date, "%m/%d/%Y")

#Select desired quantiles
q <- c(0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

#select desired site
site.sel <- "Brooklyn Lake"

#select desired variable out of soil_pm25,pm25,pm25_rec,coarse,pm10,pm10_rec, or totalcarbon
var.choices <- c("soil_pm25","pm25","pm25_rec","coarse","pm10","pm10_rec","totalcarbon")
var.sel <- "soil_pm25"

#get var name for plot titles
if (var.sel == var.choices[1]) {
  var.string <- "Fine Soil"
} else if (var.sel == var.choices[2]) {
  var.string <- "Fine Mode"
} else if (var.sel == var.choices[3]) {
  var.string <- "Reconstructed Fine"
} else if (var.sel == var.choices[4]) {
  var.string <- "Coarse Mode"
} else if (var.sel == var.choices[5]) {
  var.string <- "PM10"
} else if (var.sel == var.choices[6]) {
  var.string <- "Reconstructed PM10"
} else if (var.sel == var.choices[2]) {
  var.string <- "Total Carbon"
}


#count number of observations per year by site
count.obs <- data %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(var.sel), ~sum(!is.nan(.)))

count.obs$count <- count.obs[[var.sel]]



#number of obs possible per year in spring/summer
obs.poss <- 10*6
#require 10% of observations in spring/summer
obs.req <- 0.5*10*6
#percentage required in each year in spring/summer
obs.per.req <- obs.req/obs.poss

#subset summer and spring data and count number of obs per year
sumspring.dat <- subset(data, month(Date) == 3 | month(Date) == 4 | month(Date) == 5 |month(Date) == 6 |month(Date) == 7 |month(Date) == 8)
#test plot sumspring for site and variable
with(sumspring.dat[sumspring.dat$SiteName == "Salmon NF",], plot(Date, get(var.sel)))
# sumspring.obs <- sumspring.dat %>%
#   group_by(year(Date), SiteName) %>%
#   summarise_at(vars(var.sel), ~sum(!is.nan(.)))

#count number of observations (any data entry that is not NA for selected parameter)
sumspring.obs <- sumspring.dat %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(var.sel), ~length(which(!is.na(.))))

#identify maximum gap in years in data
dat.lag <- sumspring.obs %>%
  group_by(SiteName) %>%
  mutate(Diff = `year(Date)` - lag(`year(Date)`)) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

#count how many years each sites meets criteria for number of obs in summer/spring combined
sumspring.obs$check <- sumspring.obs[[var.sel]] >= obs.req
year.check <- sumspring.obs %>%
  group_by(SiteName) %>%
  summarise_at(vars(check), ~sum(check))
year.check$diff <- dat.lag$Diff

#check if number of years that meet obs criteria - the number of years between years with data is at least 7 years
year.check$bestcons <- year.check$check - year.check$diff + 1
year.check$pick <- year.check$bestcons >= 7 & year.check$bestcons != Inf

#create sites vector with only sites that meet criteria
year.check.sub <- subset(year.check, pick == "TRUE")
sites <- unique(year.check.sub$SiteName)

#subset dat.sub to sites that only meet criteria
dat.sub <- data[data$SiteName %in% sites, ]
rm("data")

#check dat.sub by plotting selected site and var
with(dat.sub[dat.sub$SiteName == site.sel,], plot(Date, get(var.sel)))


#generate ascending years vector
years <- sort(unique(year(dat.sub$Date)), decreasing = FALSE)

#get quantile for each year at each site
dat.quant <- NULL
for (i in 1:length(q)) {
  #i=1
  tmp.dat <- dat.sub %>%
    group_by(year(Date), SiteName) %>%
    summarise_at(vars(var.sel), ~as.numeric(quantile(get(var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    dat.quant <- tmp.dat
    colnames(dat.quant)[i+2] <- q[i]
  } else {
    dat.quant[,i+2] <- tmp.dat[[var.sel]]
    colnames(dat.quant)[i+2] <- q[i]
  }
  print(paste0('finished ', q[i]))
}


#subset only obs in the chosen quantile for each site for each year
var.quant <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(dat.sub, year(Date) == years[i] & SiteName == sites[j])
    tmp.quant <- subset(dat.quant, `year(Date)` == years[i] & dat.quant$SiteName == sites[j])
    tmp.high <- subset(tmp.dat, get(var.sel) >= tmp.quant[[q.ind+2]])
    var.quant <- rbind(var.quant,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j]))
  }
}







#now read in climate data
setwd("~/Climate_data")

#open the nc file
ncin <- nc_open("4times_daily_vwnd.nc")

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
ncin <- nc_open("4times_daily_uwnd.nc")
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

