
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


# read in AERONET files
#set working directory
setwd("~/AERONET/AERONET_coarse")
file_list <- list.files()

#read in all files and combine into dat.bind. 
dat.bind <- NULL
for (i in seq_along(file_list)){
  #i=1
  fname <- file_list[i]
  input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T, select = c(1:53))
  header <- names(input)[2:53]
  input <- input[,1:52]
  colnames(input) <- header
  #change all -999 values to NA
  input[input == -999] <- NA
  #combine date and time
  input$time_utc <- as.POSIXct(paste(input$Date_.dd.mm.yyyy., input$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
  # reorder - put date/time at front 
  input2 <- input %>%
    dplyr::select(time_utc, everything())
  #apply 30 minute smoothing - average every 30 minutes
  input3 <- input2 %>%
    group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  #change time to posixct
  input3$time_utc <- as.POSIXct(input3$time_utc)
  #create sitename var
  input3$site_name <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$time_utc))
  #use rbind to add to dat.bind df
  dat.bind <- rbind(dat.bind, input3)
  print(paste0('finished ', fname))
}

#Combine fresno sites. Fresno and Fresno2 are same location
#dat.bind <- dat.bind %>% mutate(site_name = replace(site_name, site_name == "fresno_2", "fresno"))
#plot fresno site all data
#with(dat.bind[dat.bind$site_name == "fresno",], plot(time_utc, AOD_500nm))

colnames(dat.bind)[colnames(dat.bind)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.bind)[colnames(dat.bind)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.bind)[colnames(dat.bind)=="Total_AOD_500nm.tau_a."] <- "AOD_total"

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.98
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

#select from AOD_fine and AOD_coarse
var.choices <- c("AOD_coarse","AOD_fine","AOD_total")
var.sel <- "AOD_coarse"

#get var name for plot titles
if (var.sel == var.choices[1]) {
  var.string <- "Coarse AOD"
} else if (var.sel == var.choices[2]) {
  var.string <- "Fine AOD"
} else if (var.sel == var.choices[3]) {
  var.string <- "Total AOD"
}

#count number of observations per year by site
count.obs <- dat.bind %>%
  group_by(year(time_utc), site_name) %>%
  summarise_at(vars(var.sel), ~sum(!is.nan(.)))

count.obs$count <- count.obs[[var.sel]]

#create barplot stacking observations per year by site
ggplot(data=count.obs, aes(x = `year(time_utc)`, y = count,  fill = site_name)) +
  geom_bar(stat="identity") + 
  theme_bw() + xlab("Date") + ylab("Count") + 
  ggtitle("Observations Per Year by Site") +
  labs(fill='Site Name')

#get site names in vector
sites1 <- unique(dat.bind$site_name)

#generate dataframes with data only after 2000 and observation counts greater than 200 respectively
dat.bind.sub <- subset(dat.bind, year(dat.bind$time_utc) >= 1995 & year(dat.bind$time_utc) < 2019)
count.obs.sub <- subset(count.obs, count.obs$count >= 200)

#use above dataframes to create one subsetted dataframe with data after 2000 and more than 200 obs per year
dat.sub <- NULL
for (i in 1:nrow(count.obs.sub)) {
  tmp.sub <- subset(dat.bind.sub, dat.bind.sub$site_name == count.obs.sub$site_name[i] & 
                      year(dat.bind.sub$time_utc) == count.obs.sub$`year(time_utc)`[i])
  print(paste0('finished ', count.obs.sub$`year(time_utc)`[i], ' ', count.obs.sub$site_name[i]))
  dat.sub <- rbind(dat.sub,tmp.sub)
}

rm("tmp.sub")
rm("dat.bind.sub","input","input2","input3","count.obs","count.obs.sub","file_list","fname","header")

colnames(dat.sub)[colnames(dat.sub)=="Site_Latitude.Degrees."] <- "Latitude"
colnames(dat.sub)[colnames(dat.sub)=="Site_Longitude.Degrees."] <- "Longitude"


#number of obs possible after 30 min smoothing in spring and summer combined per year
obs.poss <- 2*24*30*6
#number of obs required in the same time period - average of 0.75 obs per day in each year
obs.req <- 0.75*30*6
#percentage required in each year
obs.per.req <- obs.req/obs.poss

#subset summer and spring data and count number of obs per year
sumspring.dat <- subset(dat.sub, month(time_utc) == 3 | month(time_utc) == 4 |month(time_utc) == 5 |month(time_utc) == 6 |month(time_utc) == 7 |month(time_utc) == 8)
#sumspring.dat <- subset(dat.sub, month(time_utc) == c(3,4,5,6,7,8))
with(sumspring.dat[sumspring.dat$site_name == "neon_cvalla",], plot(time_utc, get(var.sel)))
sumspring.obs <- sumspring.dat %>%
  group_by(year(time_utc), site_name) %>%
  summarise_at(vars(var.sel), ~sum(!is.nan(.)))

dat.lag <- sumspring.obs %>%
  group_by(site_name) %>%
  mutate(Diff = `year(time_utc)` - lag(`year(time_utc)`)) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

sumspring.obs$check <- sumspring.obs[[var.sel]] >= obs.req
year.check <- sumspring.obs %>%
  group_by(site_name) %>%
  summarise_at(vars(check), ~sum(check))
year.check$diff <- dat.lag$Diff

#check if number of years that meet obs criteria - the number of years between years with data is at least 7 years
year.check$bestcons <- year.check$check - year.check$diff + 1
year.check$pick <- year.check$bestcons >= 7

#create new sites vector with only sites that meet criteria
year.check.sub <- subset(year.check, pick == "TRUE")
sites <- unique(year.check.sub$site_name)

rm("sites1")

#subset dat.sub to sites that only meet criteria
dat.sub <- dat.sub[dat.sub$site_name %in% sites, ]

#generate ascending years vector
years <- sort(unique(year(dat.sub$time_utc)), decreasing = FALSE)

#get quantile for each year at each site. Enter desired quantile
dat.quant <- NULL
for (i in 1:length(q)) {
  tmp.dat <- dat.sub %>%
    group_by(year(time_utc), site_name) %>%
    summarise_at(vars(var.sel), ~as.numeric(quantile(get(var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    dat.quant <- tmp.dat
    colnames(dat.quant)[i+2] <- q[i]
  } else {
    dat.quant[,i+2] <- tmp.dat[[var.sel]]
    colnames(dat.quant)[i+2] <- q[i]
  }
}


#subset only obs in the qth quantile for each site for each year
var.quant <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(dat.sub, year(time_utc) == years[i] & site_name == sites[j])
    tmp.quant <- subset(dat.quant, `year(time_utc)` == years[i] & dat.quant$site_name == sites[j])
    tmp.high <- subset(tmp.dat, get(var.sel) >= tmp.quant[[q.ind + 2]])
    var.quant <- rbind(var.quant,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j]))
  }
}

rm("sumspring.dat","sumspring.obs","tmp.dat","tmp.high","tmp.quant","dat.lag")






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
aero.lonlat <- data.frame("lon" = tmp.long, "lat" = tmp.lat)
rm("tmp.lat","tmp.long")
aero.lonlat$lon <- 360 + aero.lonlat$lon 
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

climo.aero.lonlat <- lonlat[apply(aero.lonlat, 1, dist2b),]

for (i in 1:nrow(climo.aero.lonlat)) {
  #i = 1
  climo.var.lon <- which(lon == climo.aero.lonlat[i,1])
  climo.var.lat <- which(lat == climo.aero.lonlat[i,2])
  climo.var <- total_wind[climo.var.lon,climo.var.lat,]
  climo.var <- data.frame("Date" = timenew, "wspd" = climo.var)
  
  #site.var$Date <- as.Date(site.var$Date, "%Y/%m/%d")

  aero.site <- subset(var.quant, Longitude == unique(var.quant$Longitude)[i] & Latitude == unique(var.quant$Latitude)[i])
  climo.var <- subset(climo.var, year(Date) >= min(year(aero.site$time_utc)) & year(Date) <= max(year(aero.site$time_utc)))
  
  tmp.site.aero.climo <- NULL
  for (j in 1:nrow(aero.site)) {
    tmp.min <- min(abs(difftime(aero.site$time_utc[j],climo.var$Date,units = "hours")))
    tmp.var.time <- climo.var[which.min(abs(difftime(aero.site$time_utc[j],climo.var$Date,units = "hours"))),]
    tmp.aerovar.climovar <- data.frame("aerodate" = aero.site$time_utc[j],
                                       "aerovar" = aero.site[[var.sel]][j],
                                       "climodate" = tmp.var.time$Date,
                                       "climovar" = tmp.var.time$wspd,
                                       "timedif" = tmp.min)
    tmp.site.aero.climo <- rbind(tmp.site.aero.climo,tmp.aerovar.climovar)
  }
  
  tmp.site.name <- aero.site$site_name[1]
  
  # savePlot <- function(myPlot) {
  #   pdf(paste0(tmp.site,"_winds_quantile.png"))
  #   print(myPlot)
  #   dev.off()
  # }

    
  myPlot <- ggplot(climo.var, aes(wspd)) +
    geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
    geom_histogram(data = tmp.site.aero.climo, aes(climovar), binwidth = 0.5, fill = "red") +
    theme_bw() +
    labs(x = "Wind Speed (m/s)", y = "Count", title = paste0(tmp.site.name," - ",q.string,"th quantile ",var.string)) +
    scale_y_continuous(trans = 'log2')
  
  ggsave(filename = paste0(tmp.site.name,"_",q.string,"_",var.sel,"_winds_quantile.png"),plot = myPlot)
  #savePlot(myPlot)
  
  #combos <- sapply(climo.var$Date,function(x) abs(x-aero.site$time_utc))
  # combos <- abs(outer(aero.site$time_utc,climo.var$Date,FUN="-"))
  # min(combos)
  # test <- which(combos <= "180",arr.ind=TRUE)
}
rm("climo.var.lon","climo.var.lat")











































#get monthly average for every site
dat.sub.monthly <- var.quant %>%
  group_by(site_name, year(time_utc), month(time_utc)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

#dat.sub.monthly$Date <- as.yearmon(paste(dat.sub.monthly$`year(time_utc)`, dat.sub.monthly$`month(time_utc)`), "%Y %m")
day1 <- rep(01,length(timenew))
dat.sub.monthly$Date <- as.Date(paste(dat.sub.monthly$`year(time_utc)`, dat.sub.monthly$`month(time_utc)`, day1, sep="-"), "%Y-%m-%d")

p <- list()
climo.aero.cor <- NULL
# test for all sites
for (i in 1:nrow(climo.aero.lonlat)) {
  site.var.lon <- which(lon == climo.aero.lonlat[i,1])
  site.var.lat <- which(lat == climo.aero.lonlat[i,2])
  site.var <- var_array[site.var.lon,site.var.lat,]
  site.var <- data.frame("Date" = timenew, "wspd" = site.var)
  
  site.var$Date <- as.Date(site.var$Date, "%Y/%m/%d")
  
  #dat.sub.monthly$Date <- as.Date(dat.sub.monthly$Date,"%m/%d/%y")
  #dat.sub.monthly$Date <- as.POSIXct(paste0(as.character(dat.sub.monthly$Date),"-01"), format = "%Y-%m-%d")
  #dat.sub.monthly$Date <- as.Date( paste(dat.sub.monthly$`year(time_utc)`, dat.sub.monthly$`month(time_utc)`, sep = "." )  , format = "%y.%m" )
  
  dat.sub.monthly.site <- subset(dat.sub.monthly, dat.sub.monthly$Site_Latitude.Degrees. == tmp.lat[i] & dat.sub.monthly$Site_Longitude.Degrees. == tmp.long[i])
  
  #get same time period
  site.var.sub <- subset(site.var, Date <= max(dat.sub.monthly.site$Date) & Date >= min(dat.sub.monthly.site$Date))
  
  #plot vars
  par(mar = c(5,5,2,5))
  with(site.var.sub, plot(Date, wspd, type="l", col="red",
                           ylab = "Soil Moisture (mm)"))#, 
  #ylab=expression(-log[10](italic(p))),
  #ylim=c(0,3)))
  par(new = T)
  with(dat.sub.monthly.site, plot(Date, get(var.sel), type = "l", col = "green", axes=F, xlab=NA, ylab=NA))
  axis(side = 4)
  mtext(side = 4, line = 3, 'AOD at 500nm')
  # legend("topleft",
  #        legend=c(expression(-log[10](italic(p))), "N genes"),
  #        lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))
  
  climo.aero.combo <- left_join(site.var.sub, dat.sub.monthly.site, by = "Date")
  #test.vars <- data.frame("wspd" = test$wspd, "AOD_500nm" = test$AOD_500nm)
  
  tmp.cor <- data.frame("cor" = cor(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], use = "complete.obs", method = "kendall"))
  tmp.cor$site_name <- dat.sub.monthly.site$site_name[1]
  tmp.cor$lat <- tmp.lat[i]
  tmp.cor$lon <- tmp.long[i]
  climo.aero.cor <- rbind(climo.aero.cor, tmp.cor)
  ccf(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], na.action = na.pass)
  #p[[i]] <- ccf(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], na.action = na.pass)
}

#do.call(grid.arrange,p)








































#isolate obs in quantiles for each month and compare with climo

months <- c(1:12)
all.months.cor <- NULL
for (l in 1:length(months)) {
  #l = 1
  tmp.month <- subset(dat.sub, month(time_utc) == months[l])
  
  #get quantile for each year at each site. Enter desired quantile
  tmp.quant <- NULL
  for (i in 1:length(q)) {
    tmp.dat <- tmp.month %>%
      group_by(year(time_utc), site_name) %>%
      summarise_at(vars(var.sel), ~as.numeric(quantile(get(var.sel), na.rm = T, probs = q[i])))
    if (i == 1) {
      tmp.quant <- tmp.dat
      colnames(tmp.quant)[i+2] <- q[i]
    } else {
      tmp.quant[,i+2] <- tmp.dat[[var.sel]]
      colnames(tmp.quant)[i+2] <- q[i]
    }
  }
  
  
  # get obs only in chosen quantile
  tmp.quant.obs <- NULL
  for (j in 1:length(years)) {
    for (k in 1:length(sites)) {
      #i=18
      #j=5
      tmp.dat <- subset(tmp.month, year(time_utc) == years[j] & site_name == sites[k])
      tmp.yearquant <- subset(tmp.quant, `year(time_utc)` == years[j] & tmp.quant$site_name == sites[k])
      tmp.high <- subset(tmp.dat, get(var.sel) >= tmp.yearquant$`0.75`)
      tmp.quant.obs <- rbind(tmp.quant.obs,tmp.high)
      print(paste0('finished ', years[j],' ',sites[k]))
    }
  }
  # if (i = 1) {
  #   quant.obs.monthly <- data.frame(tmp.quant.obs$)
  # }
  tmp.quant.monthly.ave <- tmp.quant.obs %>%
    group_by(site_name, year(time_utc), month(time_utc)) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  tmp.day1 <- rep(01,nrow(tmp.quant.monthly.ave))
  tmp.quant.monthly.ave$Date <- as.Date(paste(tmp.quant.monthly.ave$`year(time_utc)`, tmp.quant.monthly.ave$`month(time_utc)`, tmp.day1, sep="-"), "%Y-%m-%d")
  tmp.quant.monthly.ave <- data.frame("site_name" = tmp.quant.monthly.ave$site_name,
                                      "Date" = tmp.quant.monthly.ave$Date,
                                      "lat" = tmp.quant.monthly.ave$Site_Latitude.Degrees.,
                                      "lon" = tmp.quant.monthly.ave$Site_Longitude.Degrees.,
                                      "AOD_total" = tmp.quant.monthly.ave$AOD_total,
                                      "AOD_fine" = tmp.quant.monthly.ave$AOD_fine,
                                      "AOD_coarse" = tmp.quant.monthly.ave$AOD_coarse)
  
  
  climo.aero.cor <- NULL
  climo.aero.combo <- NULL
  # run for all sites
  for (m in 1:nrow(climo.aero.lonlat)) {
    #m = 2
    site.var.lon <- which(lon == climo.aero.lonlat[m,1])
    site.var.lat <- which(lat == climo.aero.lonlat[m,2])
    site.var <- var_array[site.var.lon,site.var.lat,]
    site.var <- data.frame("Date" = timenew, "wspd" = site.var)
    
    site.var$Date <- as.Date(site.var$Date, "%Y/%m/%d")
    
    #dat.sub.monthly$Date <- as.Date(dat.sub.monthly$Date,"%m/%d/%y")
    #dat.sub.monthly$Date <- as.POSIXct(paste0(as.character(dat.sub.monthly$Date),"-01"), format = "%Y-%m-%d")
    #dat.sub.monthly$Date <- as.Date( paste(dat.sub.monthly$`year(time_utc)`, dat.sub.monthly$`month(time_utc)`, sep = "." )  , format = "%y.%m" )
    
    tmp.monthly.site <- subset(tmp.quant.monthly.ave, lat == tmp.lat[m] & lon == tmp.long[m])
    
     if (nrow(tmp.monthly.site) == 0 | nrow(tmp.monthly.site) < 5) {
       climo.aero.cor <- rbind(climo.aero.cor,data.frame("site_name" = sites1[m], "lat" = tmp.lat[m], "lon" = tmp.long[m],"cor" = NA))
     } else {
       #get same time period
       site.var.sub <- subset(site.var, Date <= max(tmp.monthly.site$Date) & Date >= min(tmp.monthly.site$Date))
       site.var.sub <- subset(site.var.sub, month(Date) == l)
       
       
       climo.aero.combo <- left_join(site.var.sub, tmp.monthly.site, by = "Date")
       #test.vars <- data.frame("wspd" = test$wspd, "AOD_500nm" = test$AOD_500nm)
       
       tmp.cor <- data.frame("cor" = cor(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], use = "complete.obs", method = "kendall"))
       tmp.cor$site_name <- tmp.monthly.site$site_name[1]
       tmp.cor$lat <- tmp.lat[m]
       tmp.cor$lon <- tmp.long[m]
       tmp.cor <- tmp.cor[,c(2:4,1)]
       climo.aero.cor <- rbind(climo.aero.cor, tmp.cor)
       #ccf(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], na.action = na.pass)
       #p[[i]] <- ccf(climo.aero.combo$wspd,climo.aero.combo[[var.sel]], na.action = na.pass)
     }
  }
  if (l == 1) {
    all.months.cor <- climo.aero.cor
  } else {
    all.months.cor <- cbind(all.months.cor,climo.aero.cor$cor)
  }
}
monthnames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
colnames(all.months.cor)[4:15] <- monthnames
all.months.cor.forplot <- all.months.cor[,-c(2,3)]
cor.df <- data.frame(t(all.months.cor.forplot[-1]))
colnames(cor.df) <- all.months.cor.forplot[, 1]
rownames(cor.df) <- 1:nrow(cor.df)
cor.df$month <- monthnames
cor.df <- cor.df[,c(ncol(cor.df),1:(ncol(cor.df)-1))]
# Convert data to long format
cor.df = melt(cor.df, id.var="month")
# cor.df$variable = factor(cor.df$variable, 
#                            levels=sort(unique(as.character(Probes.m$variable))))
cor.df$month <- factor(cor.df$month, levels=monthnames)


#plot
ggplot(data = cor.df, aes(month, value, col = variable)) +
  geom_point()
