
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

setwd("/home/andylambert/misr_daily")

files <- list.files()

#Create date vector to use in directory name
date.string <- seq(as.Date("2000/01/01"), as.Date("2018/12/01"), "months")
date.string <- as.character(date.string)
date.string <- str_replace_all(date.string,"[-]",".")


#Create date vector to use in directory name
date.string <- seq(as.Date("2000/02/25"), as.Date("2016/12/31"), "days")
date.string.alt <- format(date.string,format = "%b-%d-%Y")
date.string.alt <- str_replace_all(date.string.alt,"[-]","_")
date.string.alt <- toupper(date.string.alt)
#date.string <- as.character(date.string)
#date.string <- str_replace_all(date.string,"[-]",".")



filename <- paste0("MISR_AM1_CGAS_0_5_DEG_",date.string.alt,"_F08_0031.nc.nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")
#filepath <- paste0(path,filename)
#filepathsub <- paste0(filepath,".nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")

bands <- read.csv("/home/andylambert/MISR_url_data/MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031.nc.ascii?Band_labels", header = FALSE)
bandnames <- c(bands[2,2:5])
#particletype <- read.csv("MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031.nc.ascii?ParticleType_labels", header = FALSE)

ncin <- nc_open(filename[5])
dname <- "Optical_depth_average"

lat <- ncin$dim$latitude$vals
lon <- ncin$dim$longitude$vals
aod.array <- ncvar_get(ncin,dname)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
nc_close(ncin)

realfiles <- NULL
realdates <- NULL
for (i in 1:length(filename)) {
  infile <- files == filename[i]
  if (sum(infile) > 0) {
    realfiles <- append(realfiles,filename[i])
    realdates <- append(realdates,date.string[i])
  }
}
#select band 1 = blue, 2 = green, 3 = red, 4 = IR
b = 2
aod.full <- array(numeric(),c(length(lon), length(lat), length(realfiles)))
for (i in 1:length(realfiles)) {
  ncin <- nc_open(realfiles[i])
  tmp.array <- ncvar_get(ncin,dname)
  tmp.array[tmp.array==fillvalue$value] <- NA
  #get only nonspherical aod (3rd column val = 6) , 4th column is bands - names are in bandnames, last column is AOD labels, we want 1 for the full range of AOD
  tmp.slice <- tmp.array[,,6,b,1]
  aod.full[,,i] <- tmp.slice
  nc_close(ncin)
  print(paste0("finished ",realfiles[i]))
}

#image(lon,lat,aod.full[,,2], col = rev(brewer.pal(10,"RdBu")))
lonlat <- as.matrix(expand.grid(lon,lat))
aod.df <- data.frame(lonlat)
colnames(aod.df) <- c("lon","lat")
aod.df[,"slope"] <- NA
aod.df[,"pval"] <- NA
aod.df[,"normslope"] <- NA
dates <- as.Date(realdates, format = "%Y.%m.%d")
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
      tmp.quant <- quantile(tmp.dat.year, probs = 0, na.rm = TRUE)
      #tmp.quant <- quantile(tmp.dat.year, probs = 0.90, na.rm = TRUE)
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
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +
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
  #ggtitle(paste0("Trends in 90th Quantile MISR Non-Spherical AOD (555 nm) 2000-2016"))

misr.plot

#subset region and plot
misr.mw.sub <- subset(misr.us.sub, lat >= 40 & lat <= 50 & lon >= -113 & lon <= -97)

misr.plot.sub <- ggplot(data = misr.mw.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = misr.mw.sub[!is.na(misr.mw.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-113, -97),ylim = c(40,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in 90th Quantile MISR Non-Spherical AOD (555 nm) 2000-2016"))

misr.plot.sub







#get trends for background or all MISR obs
aod.df.allobs <- data.frame(lonlat)
colnames(aod.df.allobs) <- c("lon","lat")
aod.df.allobs[,"slope"] <- NA
aod.df.allobs[,"pval"] <- NA
aod.df.allobs[,"normslope"] <- NA
dates <- as.Date(realdates, format = "%Y.%m.%d")
#get quantile for each year at each site
#trends.map <- array(numeric(),c(length(lon), length(lat),1,1))
for (j in 1:length(lon)) {
  for (k in 1:length(lat)) {
    #j = 5
    #k = 25
      point.obs <- aod.full[j,k,]
      tmp.ken <- kendallTrendTest(point.obs ~ dates, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      aod.ind <- which(aod.df$lon == lon[j] & aod.df$lat == lat[k])
      aod.df.allobs[aod.ind,3] <- tmp.slope
      aod.df.allobs[aod.ind,4] <- tmp.p
      aod.df.allobs[aod.ind,5] <- (tmp.slope/median(site.quant.obs$aod_quant, na.rm = TRUE))*100
    # } else {
    #   aod.df[aod.ind,3] <- NA
    #   aod.df[aod.ind,4] <- NA
    #   aod.df[aod.ind,5] <- NA
    # }
    print(paste0('finished ', lon[j]," ",lat[k]))
  }
}



#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = aod.df$lat,"long" = aod.df$lon)))

#filter to get only us tiles
misr.us.sub <- aod.df.allobs[point.filter,]

#make ss vector for statistical significane
misr.us.sub$ss <- misr.us.sub$pval <= 0.05
misr.us.sub$ss[!misr.us.sub$ss] <- NA

#remove all rows with pval >= 0.5
misr.us.sub <- subset(misr.us.sub, pval <= 0.25)

misr.plot <- ggplot(data = misr.us.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = misr.us.sub[!is.na(misr.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +
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
#ggtitle(paste0("Trends in 90th Quantile MISR Non-Spherical AOD (555 nm) 2000-2016"))

misr.plot

