
rm(list=ls())

library(ncdf4)
library(maps)
library(fields)
library(lubridate)
library(dplyr)
library(stringr)
library(RCurl)
library(chron)
library(lattice)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(data.table)
library(foreign)
library(ggplot2)
library(tidyr)
library(broom)
library(maps)
library(quantreg)
library(zoo)
library(boot)
library(infer)
library("EnvStats")
library(sp)
library(viridis)
library(rgdal)
#library(rasterVis)
library(mgcv)

setwd("/home/andylambert/MISR_url_data")

files <- list.files()

#Create date vector to use in directory name
date.string <- seq(as.Date("2000/01/01"), as.Date("2018/12/01"), "months")
date.string <- as.character(date.string)
date.string <- str_replace_all(date.string,"[-]",".")


years <- as.character(2000:2018)
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
prodyear <- paste0(years,"_F08_0031")
#prodmonth <- paste0("MISR_AM1_CGAS_0_5_DEG_",months)
#prodname <- data.frame(matrix(NA,nrow = length(years)*length(months),ncol = 1))
prodname <- NULL
for (k in 1:length(prodyear)) {
  #k = 2
  prodname <- append(prodname,paste0("MISR_AM1_CGAS_0_5_DEG_",months,"_",prodyear[k]), after = length(prodname))
}


url <- paste0(prodname,".nc.nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")

start <- 2
end <- which(url == "MISR_AM1_CGAS_0_5_DEG_DEC_2016_F08_0031.nc.nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")

url <- url[start:end]
date.string <- date.string[start:end]

bands <- read.csv("MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031.nc.ascii?Band_labels", header = FALSE)
bandnames <- c(bands[2,2:5])
#particletype <- read.csv("MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031.nc.ascii?ParticleType_labels", header = FALSE)

ncin <- nc_open(url[5])
dname <- "Optical_depth_average"

lat <- ncin$dim$latitude$vals
lon <- ncin$dim$longitude$vals
aod.array <- ncvar_get(ncin,dname)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")


#select band 1 = blue, 2 = green, 3 = red, 4 = IR
b = 2
aod.full <- array(numeric(),c(length(lon), length(lat), length(url)))
for (i in 1:length(url)) {
  #i = 2
  ncin <- nc_open(url[i])
  tmp.array <- ncvar_get(ncin,dname)
  tmp.array[tmp.array==fillvalue$value] <- NA
  #get only nonspherical aod (3rd column val = 6) , 4th column is bands - names are in bandnames, last column is AOD labels, we want 1 for the full range of AOD
  tmp.slice <- tmp.array[,,6,b,1]
  aod.full[,,i] <- tmp.slice
}

#image(lon,lat,aod.full[,,2], col = rev(brewer.pal(10,"RdBu")))
lonlat <- as.matrix(expand.grid(lon,lat))
aod.df <- data.frame(lonlat)
colnames(aod.df) <- c("lon","lat")
aod.df[,"slope"] <- NA
aod.df[,"pval"] <- NA
aod.df[,"normslope"] <- NA
dates <- as.Date(date.string, format = "%Y.%m.%d")
years <- unique(year(dates))
#get quantile for each year at each site
#trends.map <- array(numeric(),c(length(lon), length(lat),1,1))
for (j in 1:length(lon)) {
  for (k in 1:length(lat)) {
    #j = 5
    #k = 25
    site.quant.obs <- NULL
    for (i in 1:length(years)) {
      # i = 2
      tmp.year <- years[i]
      ind.year <- which(year(dates) == tmp.year)
      tmp.dat.year <- aod.full[j,k,ind.year]
      tmp.date <- dates[ind.year]
      tmp.quant <- quantile(tmp.dat.year, probs = 0.90, na.rm = TRUE)
      tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
      tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
      tmp.df <- data.frame("date" = tmp.date, "aod_quant" = tmp.quant.obs)
      site.quant.obs <- rbind(site.quant.obs, tmp.df)
      site.quant.obs <- site.quant.obs[complete.cases(site.quant.obs), ]
      #converts to number of days since 1970-01-01
      site.quant.obs$date <- as.numeric(site.quant.obs$date)
      #colnames(site.quant.obs)[colnames(site.quant.obs)=="tmp.quant.obs"] <- "aod_quant"
      # tmp.quant <- apply(tmp.dat.year, c(1,2), quantile, probs = 0.75, na.rm = TRUE)
      # tmp.quant.obs <- NULL
      # print(paste0("finished ",tmp.year))
    }
    if (nrow(site.quant.obs) >= 3) {
      tmp.ken <- kendallTrendTest(aod_quant ~ date, site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      aod.ind <- which(aod.df$lon == lon[j] & aod.df$lat == lat[k])
      aod.df[aod.ind,3] <- tmp.slope
      aod.df[aod.ind,4] <- tmp.p
      aod.df[aod.ind,5] <- (tmp.slope/median(site.quant.obs$aod_quant, na.rm = TRUE))*100
    } else {
      aod.df[aod.ind,3] <- NA
      aod.df[aod.ind,4] <- NA
      aod.df[aod.ind,5] <- NA
    }
    print(paste0('finished ', lon[j]," ",lat[k]))
  }
}

#get maps
states <- map_data("state")
canada = map_data("world","Canada")
mexico = map_data("world","Mexico")

#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = aod.df$lat,"long" = aod.df$lon)))

#filter to get only us tiles
misr.us.sub <- aod.df[point.filter,]

#make ss vector for statistical significane
misr.us.sub$ss <- misr.us.sub$pval <= 0.05
misr.us.sub$ss[!misr.us.sub$ss] <- NA

#remove all rows with pval >= 0.5
misr.us.sub <- subset(misr.us.sub, pval <= 0.25)

misr.plot <- ggplot(data = misr.us.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = misr.us.sub[!is.na(misr.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", guide = FALSE) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 
  #ggtitle(paste0("Trends in 75th Quantile MISR Non-Spherical AOD (555 nm) 2000-2018"))

misr.plot

#subset region and plot
misr.mw.sub <- subset(misr.us.sub, lat >= 32.5 & lat <= 50 & lon >= -105 & lon <= -90)

misr.plot.sub <- ggplot(data = misr.mw.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = misr.mw.sub[!is.na(misr.mw.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-105, -90),ylim = c(32.5,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in 90th Quantile MISR Non-Spherical AOD (555 nm) 2000-2018"))

misr.plot.sub

#another method of isolating dust, use angstrom exponent

#select band 1 = blue, 2 = green, 3 = red, 4 = IR
#get blue band
b = 1
blue.full <- array(numeric(),c(length(lon), length(lat), length(url)))
for (i in 1:length(url)) {
  #i = 2
  ncin <- nc_open(url[i])
  tmp.array <- ncvar_get(ncin,dname)
  tmp.array[tmp.array==fillvalue$value] <- NA
  #get aod for all particle types (3rd column val = 1) , 4th column is bands - names are in bandnames, last column is AOD labels, we want 1 for the full range of AOD
  tmp.slice <- tmp.array[,,1,b,1]
  blue.full[,,i] <- tmp.slice
  # if (i == 1) {
  #   aod.full <- tmp.slice
  # } else {
  #   aod.full[i,,] <- tmp.slice
  # }
}

#select band 1 = blue, 2 = green, 3 = red, 4 = IR
#get red band
b = 3
red.full <- array(numeric(),c(length(lon), length(lat), length(url)))
for (i in 1:length(url)) {
  #i = 2
  ncin <- nc_open(url[i])
  tmp.array <- ncvar_get(ncin,dname)
  tmp.array[tmp.array==fillvalue$value] <- NA
  #get aod for all particle types (3rd column val = 1) , 4th column is bands - names are in bandnames, last column is AOD labels, we want 1 for the full range of AOD
  tmp.slice <- tmp.array[,,1,b,1]
  red.full[,,i] <- tmp.slice
  # if (i == 1) {
  #   aod.full <- tmp.slice
  # } else {
  #   aod.full[i,,] <- tmp.slice
  # }
}

#select band 1 = blue, 2 = green, 3 = red, 4 = IR
#get green band
b = 2
green.full <- array(numeric(),c(length(lon), length(lat), length(url)))
for (i in 1:length(url)) {
  #i = 2
  ncin <- nc_open(url[i])
  tmp.array <- ncvar_get(ncin,dname)
  tmp.array[tmp.array==fillvalue$value] <- NA
  #get aod for all particle types (3rd column val = 1) , 4th column is bands - names are in bandnames, last column is AOD labels, we want 1 for the full range of AOD
  tmp.slice <- tmp.array[,,1,b,1]
  green.full[,,i] <- tmp.slice
  # if (i == 1) {
  #   aod.full <- tmp.slice
  # } else {
  #   aod.full[i,,] <- tmp.slice
  # }
}

#get angstrom exponent - used to isolate dust
angstrom <- -(log(blue.full/red.full))/(log(443/670))

#subset dust obs by setting all non dust obs equal to NA
dust.blue <- blue.full
dust.green <- green.full
dust.red <- red.full
for (i in 1:nrow(angstrom)) {
  for (j in 1:ncol(angstrom)) {
    #i = 15
    #j = 22
    tmp.dat <- angstrom[i,j,]
    tmp.dust.ind <- which(tmp.dat > 1)
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}




#get quantile obs and quantify trends for dust observations
dust.df <- data.frame(lonlat)
colnames(dust.df) <- c("lon","lat")
dust.df[,"slope"] <- NA
dust.df[,"pval"] <- NA
dust.df[,"normslope"] <- NA
dates <- as.Date(date.string, format = "%Y.%m.%d")
years <- unique(year(dates))
#get quantile for each year at each site
#trends.map <- array(numeric(),c(length(lon), length(lat),1,1))
for (j in 1:length(lon)) {
  for (k in 1:length(lat)) {
    #j = 5
    #k = 25
    site.quant.obs <- NULL
    for (i in 1:length(years)) {
      # i = 2
      tmp.year <- years[i]
      ind.year <- which(year(dates) == tmp.year)
      tmp.dat.year <- dust.green[j,k,ind.year]
      tmp.date <- dates[ind.year]
      tmp.quant <- quantile(tmp.dat.year, probs = 0.75, na.rm = TRUE)
      tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
      tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
      tmp.df <- data.frame("date" = tmp.date, "aod_quant" = tmp.quant.obs)
      site.quant.obs <- rbind(site.quant.obs, tmp.df)
      site.quant.obs <- site.quant.obs[complete.cases(site.quant.obs), ]
      #converts to number of days since 1970-01-01
      site.quant.obs$date <- as.numeric(site.quant.obs$date)
      #colnames(site.quant.obs)[colnames(site.quant.obs)=="tmp.quant.obs"] <- "aod_quant"
      # tmp.quant <- apply(tmp.dat.year, c(1,2), quantile, probs = 0.75, na.rm = TRUE)
      # tmp.quant.obs <- NULL
      # print(paste0("finished ",tmp.year))
    }
    if (nrow(site.quant.obs) >= 3) {
      tmp.ken <- kendallTrendTest(aod_quant ~ date, site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      aod.ind <- which(aod.df$lon == lon[j] & aod.df$lat == lat[k])
      dust.df[aod.ind,3] <- tmp.slope
      dust.df[aod.ind,4] <- tmp.p
      dust.df[aod.ind,5] <- (tmp.slope/median(site.quant.obs$aod_quant, na.rm = TRUE))*100
    } else {
      dust.df[aod.ind,3] <- NA
      dust.df[aod.ind,4] <- NA
      dust.df[aod.ind,5] <- NA
    }
    print(paste0('finished ', lon[j]," ",lat[k]))
  }
}


#get only tiles in US

#filter to get only us tiles
dust.us.sub <- dust.df[point.filter,]

#make ss vector for statistical significane
dust.us.sub$ss <- dust.us.sub$pval <= 0.05
dust.us.sub$ss[!dust.us.sub$ss] <- NA

#remove all rows with pval >= 0.5
dust.us.sub <- subset(dust.us.sub, pval <= 0.2)

dust.plot <- ggplot(data = dust.us.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = dust.us.sub[!is.na(dust.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
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
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in 75th Quantile MISR Non-Spherical AOD 2000-2018"))

dust.plot
