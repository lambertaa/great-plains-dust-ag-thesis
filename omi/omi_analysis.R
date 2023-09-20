
#clear environments
rm(list=ls())

library(ggplot2)
library(dplyr)
library(raster)
library(maps)
library(rgdal)
library(ncdf4)
library(lubridate)


#function to pull out all characters x till n to get date from filename
# substrRight <- function(x, n){
#    substr(x, n, nchar(x))
#  }

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

#set working directory to omi
setwd("~/omi")
#list all files in wd
files <- list.files()
#exclude first two and last two read me files
#files <- files[3:length(files)-2]
#pull year out of filename
year <- substr(files,22,25)
#pull month out of filename
month <- substr(files,27,28)
#pull day out of filename
day <- substr(files,29,30)
#combine year month day to get date
date <- as.Date(paste(year,month,day,sep = "-"), "%Y-%m-%d")
#convert date to day of year
doy <- as.numeric(strftime(date, format = "%j"))
#subset to files in summer wildfire season (doy 170-220)
summer.ind <- which(doy >= 170 & doy <= 220)
date <- date[summer.ind]
doy <- doy[summer.ind]
files <- files[summer.ind]
#remove 2019
pre2019 <- which(year(date) < 2019)
date <- date[pre2019]
doy <- doy[pre2019]
files <- files[pre2019]

#open first file to get lat and long
ncin <- nc_open(files[1])
lat <- ncvar_get(ncin,"lat")
long <- ncvar_get(ncin,"lon")
ai <- ncvar_get(ncin,"FinalAerosolAbsOpticalDepth388")

#now read in all files and store in matrix
ai.array <- array(numeric(),c(length(long), 
                                     length(lat), 
                                     length(files)))
for (i in 1:length(files)) {
  #i = 1
  ncin <- nc_open(files[i])
  fillval <- ncatt_get(ncin,"FinalAerosolAbsOpticalDepth388","_FillValue")
  tmp.ai <- ncvar_get(ncin,"FinalAerosolAbsOpticalDepth388")
  tmp.ai[tmp.ai == fillval$value] <- NA
  ai.array[,,i] <- tmp.ai
  print(paste0("finished ",date[i]))
}

#get unique years from date vector
years <- unique(year(date))

#create df to store trend data
lonlat <- as.matrix(expand.grid(long,lat))
omi.trend.df <- data.frame(lonlat)
colnames(omi.trend.df) <- c("lon","lat")
omi.trend.df[,"slope"] <- NA
omi.trend.df[,"pval"] <- NA
omi.trend.df[,"normslope"] <- NA


# source('/home/andylambert/omi_trend.R')
# blah(long,lat,years)
#get quantile for each year at each gridpoint and trends in quantile
for (j in 1:length(long)) {
  for (k in 1:length(lat)) {
    #j = 5
    #k = 15
    point.quant.obs <- NULL
    for (i in 1:length(years)) {
      #i = 1
      tmp.year <- years[i]
      ind.year <- which(year(date) == tmp.year)
      tmp.dat.year <- ai.array[j,k,ind.year]
      tmp.date <- date[ind.year]
      tmp.quant <- quantile(tmp.dat.year, probs = q.sel, na.rm = TRUE)
      tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
      tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
      tmp.df <- data.frame("date" = tmp.date, "ai_quant" = tmp.quant.obs)
      point.quant.obs <- rbind(point.quant.obs, tmp.df)
      point.quant.obs <- point.quant.obs[complete.cases(point.quant.obs), ]
      #converts to number of days since 1970-01-01
      point.quant.obs$date <- as.numeric(point.quant.obs$date)
      #colnames(site.quant.obs)[colnames(site.quant.obs)=="tmp.quant.obs"] <- "aod_quant"
      # tmp.quant <- apply(tmp.dat.year, c(1,2), quantile, probs = 0.75, na.rm = TRUE)
      # tmp.quant.obs <- NULL
      # print(paste0("finished ",tmp.year))
    }
    if (nrow(point.quant.obs) >= 3) {
      tmp.ken <- kendallTrendTest(ai_quant ~ date, point.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      ai.ind <- which(omi.trend.df$lon == long[j] & omi.trend.df$lat == lat[k])
      omi.trend.df[ai.ind,3] <- tmp.slope
      omi.trend.df[ai.ind,4] <- tmp.p
      omi.trend.df[ai.ind,5] <- (tmp.slope/median(point.quant.obs$ai_quant, na.rm = TRUE))*100
    } else {
      ai.ind <- which(omi.trend.df$lon == long[j] & omi.trend.df$lat == lat[k])
      omi.trend.df[ai.ind,3] <- NA
      omi.trend.df[ai.ind,4] <- NA
      omi.trend.df[ai.ind,5] <- NA
    }
    print(paste0('finished ', long[j]," ",lat[k]))
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
                      as.matrix(data.frame("lat" = omi.trend.df$lat,"long" = omi.trend.df$lon)))

#filter to get only us tiles
omi.us.sub <- omi.trend.df[point.filter,]

#make ss vector for statistical significane
omi.us.sub$ss <- omi.us.sub$pval <= 0.05
omi.us.sub$ss[!omi.us.sub$ss] <- NA

ggplot(data = omi.us.sub, aes(x = lon, y = lat, fill = normslope)) + geom_tile() +
  geom_tile(data = omi.us.sub[!is.na(omi.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -101),ylim = c(31,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14))
#ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018"))
