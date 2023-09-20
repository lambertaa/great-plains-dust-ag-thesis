
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
q <- c(0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)


#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

plotRGB((rgb*0.0010000000474974513), 3,2,1)
plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- as.data.frame(rgb, xy = T)
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
  tmp.rgb <- brick(modis.file.path[i])
  #put data in dataframe
  modis.df.tmp <- as.data.frame(tmp.rgb, xy = T)
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

#get observations in desired quantile for each gridpoint
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.trend.df <- data.frame(modis.lonlat)
colnames(modis.trend.df) <- c("lon","lat")
modis.trend.df[,"slope"] <- NA
modis.trend.df[,"pval"] <- NA
modis.trend.df[,"normslope"] <- NA

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#subset to most recent years (since 2010)
# time.ind <- which(year(modis.date) >= 2010)
# modis.date <- modis.date[time.ind]
# dust.green <- dust.green[,,time.ind]

#get quantile for each year at each gridpoint and trends in quantile
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 5
    #k = 15
    point.quant.obs <- NULL
    for (i in 1:length(modis.years)) {
      #i = 1
      tmp.year <- modis.years[i]
      ind.year <- which(year(modis.date) == tmp.year)
      tmp.dat.year <- dust.green[j,k,ind.year]
      tmp.date <- modis.date[ind.year]
      tmp.quant <- quantile(tmp.dat.year, probs = q.sel, na.rm = TRUE)
      tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
      tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
      tmp.df <- data.frame("date" = tmp.date, "aod_quant" = tmp.quant.obs)
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
      tmp.ken <- kendallTrendTest(aod_quant ~ date, point.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
      modis.trend.df[aod.ind,3] <- tmp.slope
      modis.trend.df[aod.ind,4] <- tmp.p
      modis.trend.df[aod.ind,5] <- (tmp.slope/median(point.quant.obs$aod_quant, na.rm = TRUE))*100
    } else {
      aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
      modis.trend.df[aod.ind,3] <- NA
      modis.trend.df[aod.ind,4] <- NA
      modis.trend.df[aod.ind,5] <- NA
    }
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
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
                      as.matrix(data.frame("lat" = modis.trend.df$lat,"long" = modis.trend.df$lon)))

#filter to get only us tiles
modis.us.sub <- modis.trend.df[point.filter,]

#make ss vector for statistical significane
modis.us.sub$ss <- modis.us.sub$pval <= 0.05
modis.us.sub$ss[!modis.us.sub$ss] <- NA

modis.plot <- ggplot(data = modis.us.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.us.sub[!is.na(modis.us.sub$ss), ], aes(color = ss), size = 0.25) + 
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
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 
#ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot




#monthly analysis

#get monthly trend and CIs for each site for aeronet


#make df with lat, lon, month and columns to fill with sens slope model output
modis.trend.df.month <- data.frame(modis.lonlat)
colnames(modis.trend.df.month) <- c("lon","lat")
modis.trend.df.month[,"slope"] <- NA
modis.trend.df.month[,"pval"] <- NA
modis.trend.df.month[,"normslope"] <- NA
modis.trend.df.month[,"month"] <- NA

#create months numbered vector
months <- c(1:12)
#create empty df to store slope values
modis.all.months.sites.slope <- NULL
#create empty df to store monthly data
tmp.monthly <- NULL
#month names for plotting
monthnames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
for (l in 1:length(months)) {
  #l = 3
  tmp.month.ind <- which(month(modis.date) == l)
  tmp.date.month <- modis.date[tmp.month.ind]
  tmp.month.data <- dust.green[,,tmp.month.ind]
  #get quantile for each year at each gridpoint and trends in quantile within the month
  for (j in 1:length(modis.lat)) {
    for (k in 1:length(modis.lon)) {
      #j = 5
      #k = 15
      point.quant.obs <- NULL
      for (i in 1:length(modis.years)) {
        #i = 1
        tmp.year <- modis.years[i]
        ind.year <- which(year(tmp.date.month) == tmp.year)
        tmp.dat.year <- tmp.month.data[j,k,ind.year]
        tmp.date <- tmp.date.month[ind.year]
        tmp.quant <- quantile(tmp.dat.year, probs = q.sel, na.rm = TRUE)
        tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
        tmp.date.quant <- tmp.date[tmp.dat.year >= tmp.quant]
        tmp.df <- data.frame("date" = tmp.date.quant, "aod_quant" = tmp.quant.obs)
        point.quant.obs <- rbind(point.quant.obs, tmp.df)
        point.quant.obs <- point.quant.obs[complete.cases(point.quant.obs), ]
        #converts to number of days since 1970-01-01
        point.quant.obs$date <- as.numeric(point.quant.obs$date)
        #colnames(site.quant.obs)[colnames(site.quant.obs)=="tmp.quant.obs"] <- "aod_quant"
        # tmp.quant <- apply(tmp.dat.year, c(1,2), quantile, probs = 0.75, na.rm = TRUE)
        # tmp.quant.obs <- NULL
        # print(paste0("finished ",tmp.year))
      }
      if (nrow(point.quant.obs) >= 5) {
        tmp.ken <- kendallTrendTest(aod_quant ~ date, point.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
        tmp.slope <- tmp.ken$estimate[2] * 365
        tmp.p <- tmp.ken$p.value
        aod.ind <- which(modis.trend.df.month$lon == modis.lon[k] & modis.trend.df.month$lat == modis.lat[j])
        modis.trend.df.month[aod.ind,3] <- tmp.slope
        modis.trend.df.month[aod.ind,4] <- tmp.p
        modis.trend.df.month[aod.ind,5] <- (tmp.slope/median(point.quant.obs$aod_quant, na.rm = TRUE))*100
        modis.trend.df.month[aod.ind,6] <- monthnames[l]
      } else {
        aod.ind <- which(modis.trend.df.month$lon == modis.lon[k] & modis.trend.df.month$lat == modis.lat[j])
        modis.trend.df.month[aod.ind,3] <- NA
        modis.trend.df.month[aod.ind,4] <- NA
        modis.trend.df.month[aod.ind,5] <- NA
        modis.trend.df.month[aod.ind,6] <- monthnames[l]
        print(paste0('finished ', modis.lon[k]," ",modis.lat[j], " ", monthnames[l]))
      }
    }
  }
  modis.all.months.sites.slope <- rbind(modis.all.months.sites.slope,modis.trend.df.month)
}

#make empty list to store plots
p <- list()

#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #i = 1
  #store plots in list
  #temporary plot for one month
  modis.month <- subset(modis.all.months.sites.slope, month == monthnames[i])
  #get only tiles in US
  #filter to get only us tiles
  modis.month.sub <- modis.month[point.filter,]

  #make ss vector for statistical significane
  modis.month.sub$ss <- modis.month.sub$pval <= 0.05
  modis.month.sub$ss[!modis.month.sub$ss] <- NA

tmp.plot <- ggplot(data = modis.month.sub, aes(x = lon, y = lat, fill = normslope)) + geom_tile() +
  #geom_point(data = modis.us.sub[!is.na(modis.month.sub$sub), ], aes(color = ss), size = 0.5, color = "black") +
  geom_tile(data = modis.us.sub[!is.na(modis.month.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  #xlab("Longitude") +
  #ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0(monthnames[i])) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
p[[i]] <- tmp.plot
tmp.plot <- NULL
print(paste0("finished ",monthnames[i]))
graphics.off()
}


#find maximum slope to know shich legend to choose
maxslope <- modis.monthly.slope.sub %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

#function to pull out legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pull out legend
mylegend<-g_legend(p[[which.max(maxslope$normslope)]])

#arrange all monthly plots into one figure, use mylegend as legend
grid.arrange(arrangeGrob(p[[1]] + theme(legend.position = "none"),
                         p[[2]] + theme(legend.position = "none"),
                         p[[3]] + theme(legend.position = "none"),
                         p[[4]] + theme(legend.position = "none"),
                         p[[5]] + theme(legend.position = "none"),
                         p[[6]] + theme(legend.position = "none"),
                         p[[7]] + theme(legend.position = "none"),
                         p[[8]] + theme(legend.position = "none"),
                         p[[9]] + theme(legend.position = "none"),
                         p[[10]] + theme(legend.position = "none"),
                         p[[11]] + theme(legend.position = "none"),
                         p[[12]] + theme(legend.position = "none")),
             mylegend, widths = c(10,1.5),
             top = textGrob(paste0(q.string, "th Quantile Trends - ", "AOD"),gp=gpar(fontsize = 12, font = 3)))


#make empty list to store plots
p <- list()
#generate base plot to add data to
basemap <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-126, -90),ylim = c(27,50),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(aero.all.months.sites.slope, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                                 size = abs(normslope),
                                                                                                 color = ifelse(normslope > 0,"Increasing","Decreasing"),
                                                                                                 fill = ifelse(pval < 0.05,ifelse(normslope > 0, "Increasing","Decreasing"),"p-val < 0.1")),
                           shape = 21,
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend") +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_alpha_manual(values = c(0,1), guide = FALSE) +
                labs(size = "%/year", color = "Trend") +
                theme_bw() + 
                theme(legend.text = element_text(size=12)) +
                ggtitle(monthnames[i]) +
                theme(axis.text = element_text(size = 7), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=8)) + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()))
  
  p[[i]] <- tmp.plot
  tmp.plot <- NULL
  print(paste0("finished ",monthnames[i]))
  graphics.off()
}

#find maximum slope to know shich legend to choose
maxslope <- aero.all.months.sites.slope %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

#function to pull out legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pull out legend
mylegend<-g_legend(p[[which.max(maxslope$normslope)]])

#arrange all monthly plots into one figure, use mylegend as legend
grid.arrange(arrangeGrob(p[[1]] + theme(legend.position = "none"),
                         p[[2]] + theme(legend.position = "none"),
                         p[[3]] + theme(legend.position = "none"),
                         p[[4]] + theme(legend.position = "none"),
                         p[[5]] + theme(legend.position = "none"),
                         p[[6]] + theme(legend.position = "none"),
                         p[[7]] + theme(legend.position = "none"),
                         p[[8]] + theme(legend.position = "none"),
                         p[[9]] + theme(legend.position = "none"),
                         p[[10]] + theme(legend.position = "none"),
                         p[[11]] + theme(legend.position = "none"),
                         p[[12]] + theme(legend.position = "none")),
             mylegend, widths = c(10,1.5),
             top = textGrob(paste0(q.string, "th Quantile Trends - ", var.string),gp=gpar(fontsize = 12, font = 3)))


#below I average regions together to get regional changes
#get regular rownumbers for df
rownames(aero.all.months.sites.slope) <- 1:nrow(aero.all.months.sites.slope)

#remove all rows with NAs
aero.all.months.sites.slope <- na.omit(aero.all.months.sites.slope)
#remove all sites with lat above 49 to remove canadian sites
#aero.all.months.sites.slope <- subset(aero.all.months.sites.slope, lat < 49)

#assign sites to states
#create empty states column
aero.all.months.sites.slope$state <- rep(NA,nrow(aero.all.months.sites.slope))
for (i in 1:nrow(aero.all.months.sites.slope)) {
  #i = 20
  if (as.character(aero.all.months.sites.slope[i,1]) == "saturn_island" ) {
    aero.all.months.sites.slope$state[i] = "BC"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "trinidad_head" |
             as.character(aero.all.months.sites.slope[i,1]) == "monterey" |
             as.character(aero.all.months.sites.slope[i,1]) == "fresno" |
             as.character(aero.all.months.sites.slope[i,1]) == "ucsb") {
    aero.all.months.sites.slope$state[i] = "CA"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "maricopa" |
             as.character(aero.all.months.sites.slope[i,1]) == "tucson") {
    aero.all.months.sites.slope$state[i] = "AZ"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "hjandrews") {
    aero.all.months.sites.slope$state[i] = "OR"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "rimrock") {
    aero.all.months.sites.slope$state[i] = "ID"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "bozeman" |
             as.character(aero.all.months.sites.slope[i,1]) == "missoula") {
    aero.all.months.sites.slope$state[i] = "MT"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "railroad_valley") {
    aero.all.months.sites.slope$state[i] = "NV"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "sevilleta" |
             as.character(aero.all.months.sites.slope[i,1]) == "white_sands_helstf") {
    aero.all.months.sites.slope$state[i] = "NM"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "red_mountain_pass" |
             as.character(aero.all.months.sites.slope[i,1]) == "bsrn_bao_boulder" | 
             as.character(aero.all.months.sites.slope[i,1]) == "neon_cvalla" | 
             as.character(aero.all.months.sites.slope[i,1]) == "table_mountain") {
    aero.all.months.sites.slope$state[i] = "CO"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "cart_site") {
    aero.all.months.sites.slope$state[i] = "OK"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "konza_edc") {
    aero.all.months.sites.slope$state[i] = "KS"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "sioux_falls") {
    aero.all.months.sites.slope$state[i] = "SD"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "ames") {
    aero.all.months.sites.slope$state[i] = "IA"
  } else if (as.character(aero.all.months.sites.slope[i,1]) == "univ_of_houston") {
    aero.all.months.sites.slope$state[i] = "TX"
  }
}

#assign states to regions
MW <- c("ND", "SD" , "MN" , "KS" , "NE" , "IA" , "MO")
SW <- c("AZ" , "NM" , "UT" , "CO")
NW <- c("WA" , "OR" , "ID" , "MT" , "WY" , "BC")
W <- c("CA", "NV")
S <- c("TX","OK","LA","AR")
#create empty region column
aero.all.months.sites.slope$region <- rep(NA,nrow(aero.all.months.sites.slope))


for (i in 1:nrow(aero.all.months.sites.slope)) {
  if (sum(as.numeric(aero.all.months.sites.slope[i,10] == SW))==1) {
    aero.all.months.sites.slope$region[i] = "SW"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,10] == NW))==1) {
    aero.all.months.sites.slope$region[i] = "NW"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,10] == MW))==1) {
    aero.all.months.sites.slope$region[i] = "MW"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,10] == W))==1) {
    aero.all.months.sites.slope$region[i] = "W"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,10] == S))==1) {
    aero.all.months.sites.slope$region[i] = "S"
  }
}

region.trend <- aero.all.months.sites.slope %>%
  group_by(region, month) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

region.trend <- region.trend[order(region.trend$month),]
day <- c(as.factor(region.trend$region))
day[c(FALSE,TRUE, FALSE, FALSE, FALSE)] <- (day[c(FALSE,TRUE, FALSE, FALSE, FALSE)]*3) - 1
day[c(FALSE,FALSE, TRUE, FALSE, FALSE)] <- (day[c(FALSE,FALSE, TRUE, FALSE, FALSE)]*3) + 1
day[c(FALSE,FALSE, FALSE, TRUE, FALSE)] <- (day[c(FALSE,FALSE, FALSE, TRUE, FALSE)]*4) - 1 
day[c(FALSE,FALSE, FALSE, FALSE, TRUE)] <- (day[c(FALSE,FALSE, FALSE, FALSE, TRUE)]*4)
region.trend$Date <- as.Date(paste(c(region.trend$month),day,sep = "."), format = "%m.%d")

ggplot(data = region.trend, aes(Date, normslope, col = region)) +
  geom_point() + 
  scale_x_date(breaks = "1 month", date_labels = "%B") +
  theme_bw() +
  geom_errorbar(aes(ymin=normLCI, ymax=normUCI), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = c("Dark Green","Dark Blue","Dark Red","Dark Orange","Blue"), name = "Region", guide = FALSE) +
  xlab("Month") +
  ylab("%/Year") +
  #ggtitle(paste0(" Regional Monthly ", q.string, "th Quantile Normalized Trends - ", var.string)) +
  theme(
    plot.title = element_text(size=16, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) +
  geom_hline(yintercept = 0)