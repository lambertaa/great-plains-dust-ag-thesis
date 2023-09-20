
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

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.5,0.75,0.90,0.95)
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
    tmp.dust.ind <- which(tmp.dat > 0.5)
    dust.blue[i,j,tmp.dust.ind] <- NA
    dust.green[i,j,tmp.dust.ind] <- NA
    dust.red[i,j,tmp.dust.ind] <- NA
  }
}


#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

#create months numbered vector
months <- c(1:12)
#month names for plotting
monthnames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")


#get maps
us <- getData("GADM", country="USA", level=1)

modis.sp <- SpatialPoints(c(modis.lonlat.df[,1:2]))
proj4string(modis.sp) <- proj4string(us)
#use over to find where points overlap with states and get state name (NAME_1)
point.state <- as.character(over(modis.sp, us)$NAME_1)
modis.lonlat.df$state <- point.state

#get states map
states <- map_data("state")

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                       region == "colorado" | 
                       region == "montana" | 
                       region == "north dakota")

modis.lonlat.og <- subset(modis.lonlat.df , state == "Wyoming" |
                             state == "Colorado" |
                             state == "Montana" |
                             state == "North Dakota")

dust.lon <- colnames(dust.green)
dust.lat <- rownames(dust.green)


dust.green.og <- dust.green
for (i in 1:length(dust.lon)) {
  for (j in 1: length(dust.lat)) {
    #i = 22
    #j = 7
    lon.ind <- which(modis.lonlat.og$Var1 == as.numeric(dust.lon[i]))
    lat.ind <- which(modis.lonlat.og$Var2 == as.numeric(dust.lat[j]))
    check <- which(lon.ind %in% lat.ind)
    if (is.empty(check)) {
    #if (is.empty(lon.ind) | is.empty(lat.ind)) {
      dust.green.og[j,i,] <- NA
    }
  }
}

# lat.match <- match(modis.lonlat.og$Var2,dust.lat)
# lon.match <- match(modis.lonlat.og$Var1,dust.lon)
# 
# dust.green.og <- dust.green[lat.match,lon.match,]
# dust.green[lat.match[1],lon.match[1],1]

dust.green.og <- dust.green.og[,,which(year(modis.date) >= 2014)]
modis.date.sub <- modis.date[which(year(modis.date) >= 2014)]

#look at high percentile events
hist(dust.green.og)
limit <- quantile(dust.green.og, .99, na.rm = T)

#high <- which(dust.green.og[,,] > limit, arr.ind = TRUE)
high <- which(dust.green.og[,,] > 1 & dust.green.og[,,] < 2.5, arr.ind = TRUE)
#high <- which(dust.green.og[,,] > limit, arr.ind = TRUE)
high <- data.frame(high)

# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }

modes <- data.frame(sort(table(high$dim3), decreasing = TRUE)[1:25])

# getmode(high[,3])
pick <- 1
mode.date <- modes[pick,1]
modis.date.sub[as.numeric(as.character(mode.date))]
test <- dust.green.og[,,as.numeric(as.character(mode.date))]

ggplot(melt(test), aes(y = Var1, x = Var2, fill = value)) + geom_raster() +
  theme_bw() + 
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") 


#put data in dataframe
modis.df.tmp.test <- raster::as.data.frame(dust.green.og[,,6605], xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 25 & y <= 50 & x >= -130 & x <= -85)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","green")
#scale factor
#modis.scale.factor <- 0.00100000004749745
#multiply measurements by scale factor to get true AOD value
#modis.df.tmp.sub.test[,3:5] <- modis.df.tmp.sub.test[,3:5] * modis.scale.factor
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



#put data in dataframe
# test <- raster::as.data.frame(dust.green.og[,,5887], xy = T)
# for (i in 1:nrow(modis.lonlat.og)) {
#   tmp.lat <- modis.lonlat.og$Var2
#   tmp.lon <- modis.lonlat.og$Var1
#   tmp.dat <- stuff
# }


# ggplot() + geom_tile(data = dust.green.og[,,2733], aes(x = lon, y = lat, fill = green)) +
#   #scale_fill_manual(values = setNames(colors, levs))
#   #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "gray") +
#   theme_bw() + 
#   geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
#   coord_fixed(xlim = c(-130, -85),ylim = c(25,50),ratio = 1.3) +
#   labs(fill = "%/year" )+
#   xlab("Longitude") +
#   ylab("Latitude") + 
#   theme(legend.text = element_text(size=12)) +
#   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14)) +
#   ggtitle(paste0("Trends in 75th Quantile Non-Spherical AOD (865 nm) 2000-2016"))


#empty dataframe to store data
modis.monthly.trend.df <- NULL
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 1
    #k = 19
    #get temporary data for the date
    tmp.dat <- dust.green[j,k,]
    tmp.month.df <- NULL
    for (l in 1:length(months)) {
      #l = 1
      tmp.ind <- which(month(modis.date) == months[l])
      tmp.dates <- modis.date[tmp.ind]
      tmp.month.dat <- tmp.dat[tmp.ind]
      if (sum(!is.na(tmp.month.dat)) > 5) {
        tmp.ken <- kendallTrendTest(tmp.month.dat ~ tmp.dates, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
        tmp.slope <- tmp.ken$estimate[2] * 365
        tmp.lcl <- tmp.ken$interval$limits[1] * 365
        tmp.ucl <- tmp.ken$interval$limits[2] *365
        tmp.p <- tmp.ken$p.value
        #aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
        tmp.df <- data.frame("Longitude" = modis.lon[k],
                             "Latitude" = modis.lat[j],
                             "normslope" = tmp.slope/median(tmp.month.dat, na.rm = TRUE)*100,
                             "pval" = tmp.p,
                             "lcl" = tmp.lcl/median(tmp.month.dat, na.rm = TRUE)*100,
                             "ucl" = tmp.ucl/median(tmp.month.dat, na.rm = TRUE)*100,
                             "month" = months[l],
                             "monthname" = monthnames[l])
      } else {
        tmp.df <- data.frame("Longitude" = modis.lon[k],
                             "Latitude" = modis.lat[j],
                             "normslope" = NA,
                             "pval" = NA,
                             "lcl" = NA,
                             "ucl" = NA,
                             "month" = months[l],
                             "monthname" = monthnames[l])
      }
      tmp.month.df <- rbind(tmp.month.df, tmp.df)
      print(paste0("finished ", modis.lat[j], " ", modis.lon[k], " ", monthnames[l]))
    }
    #modis.trend.df[aod.ind,3] <- tmp.slope
    #modis.trend.df[aod.ind,4] <- tmp.p
    #modis.trend.df[aod.ind,5] <- (tmp.slope/median(point.quant.obs$aod_quant, na.rm = TRUE))*100
    #j = 5
    #k = 15
    modis.monthly.trend.df <- rbind(modis.monthly.trend.df,tmp.month.df)
  }
}

modis.monthly.trend.df
#get maps
us <- getData("GADM", country="USA", level=1)

modis.sp <- SpatialPoints(c(modis.monthly.trend.df[,1:2]))
proj4string(modis.sp) <- proj4string(us)
#use over to find where points overlap with states and get state name (NAME_1)
point.state <- as.character(over(modis.sp, us)$NAME_1)
modis.monthly.trend.df$state <- point.state

#get states map
states <- map_data("state")

#get selected states
states.sel <- subset(states, #region == "wyoming" | 
                       #region == "colorado" | 
                       #region == "montana" | 
                     region == "north dakota" |
                     region == "south dakota" |
                     region == "nebraska" |
                     region == "kansas" |
                     region == "oklahoma" |
                     region == "iowa" |
                     region == "missouri" |
                     region == "minnesota")

modis.monthly.og <- subset(modis.monthly.trend.df , #state == "Wyoming" |
       #state == "Colorado" |
       #state == "Montana" |
       state == "North Dakota" |
       state == "South Dakota" |
       state == "Nebraska" |
       state == "Kansas" |
       state == "Oklahoma" |
       state == "Iowa" |
       state == "Missouri" |
       state == "Minnesota")

state.monthly.trends <- modis.monthly.og %>%
  group_by(state, month) %>%
  summarize_all(~mean(., na.rm = T))



state.trend <- aero.all.months.sites.slope %>%
  group_by(region, month) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

state.trend <- state.monthly.trends[order(state.monthly.trends$month),]
day <- c(as.factor(state.trend$state))
day[c(FALSE,TRUE, FALSE, FALSE)] <- (day[c(FALSE,TRUE, FALSE, FALSE)]*2)
day[c(FALSE,FALSE, TRUE, FALSE)] <- (day[c(FALSE,FALSE, TRUE, FALSE)]*3) - 1
day[c(FALSE,FALSE, FALSE, TRUE)] <- (day[c(FALSE,FALSE, FALSE, TRUE)]*3)
#day[c(FALSE,FALSE, FALSE, FALSE, TRUE)] <- (day[c(FALSE,FALSE, FALSE, FALSE, TRUE)]*4)
state.trend$Date <- as.Date(paste(c(state.trend$month),day,sep = "."), format = "%m.%d")

ggplot(data = state.trend, aes(Date, normslope, col = state)) +
  geom_point() + 
  scale_x_date(breaks = "1 month", date_labels = "%B") +
  theme_bw() +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = c("Dark Green","Dark Blue","Dark Red","Dark Orange","Blue"), name = "Region", guide = "legend") +
  xlab("Month") +
  ylab("%/Year") +
  #ggtitle(paste0(" Regional Monthly ", q.string, "th Quantile Normalized Trends - ", var.string)) +
  theme(
    plot.title = element_text(size=16, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) +
  ylim(-10,10) +
  geom_hline(yintercept = 0)



#average dust obs by month and state
modis.sp <- SpatialPoints(c(modis.lonlat.df[,1:2]))
proj4string(modis.sp) <- proj4string(us)
#use over to find where points overlap with states and get state name (NAME_1)
point.state <- as.character(over(modis.sp, us)$NAME_1)
modis.lonlat.df$state <- point.state
colnames(modis.lonlat.df)[1] <- "Lon"
colnames(modis.lonlat.df)[2] <- "Lat"

#get only selected states
#states.pick <- c("Wyoming","Colorado","Montana","North Dakota")
states.pick <- c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Oklahoma", "Iowa", "Missouri", "Minnesota")
# modis.lonlat.statesel <- modis.lonlat.df[which(modis.lonlat.df$state == states.pick[1] | 
#         modis.lonlat.df$state == states.pick[2] | 
#         modis.lonlat.df$state == states.pick[3] | 
#         modis.lonlat.df$state == states.pick[4]),]

years <- 2000:2018
modis.state.monthly.yearly <- NULL
for (i in 1:length(states.pick)) {
  #i = 1
  tmp.state.lonlat <- modis.lonlat.df[which(modis.lonlat.df$state == states.pick[i]),]
  tmp.state.df <- NULL
  for(j in 1:nrow(tmp.state.lonlat)) {
    #j = 1
    lat.ind <- which(as.numeric(rownames(dust.green)) == tmp.state.lonlat$Lat[j]) 
    lon.ind <- which(as.numeric(colnames(dust.green)) == tmp.state.lonlat$Lon[j])
    tmp.dat <- dust.green[lat.ind,lon.ind,]
    tmp.month.df <- NULL
    for (k in 1:length(months)) {
      #k = 3
      tmp.ind <- which(month(modis.date) == months[k])
      tmp.dates <- modis.date[tmp.ind]
      tmp.month.dat <- tmp.dat[tmp.ind]
      tmp.year.df <- NULL
      for (l in 1:length(years)) {
        #l = 1
        tmp.dates.year <- tmp.dates[which(year(tmp.dates) == years[l])]
        tmp.month.year.dat <- tmp.month.dat[which(year(tmp.dates) == years[l])]
        tmp.mean <- mean(tmp.month.year.dat, na.rm = T)
        tmp.df <- data.frame("lon" = tmp.state.lonlat$Lon[j],
                             "lat" = tmp.state.lonlat$Lat[j],
                             "state" = states.pick[i],
                             "month" = months[k],
                             "year" = years[l],
                             "mean" = tmp.mean)
        tmp.year.df <- rbind(tmp.year.df,tmp.df)
        tmp.year.df$mean[is.nan(tmp.year.df$mean)] <- NA
      }
      tmp.month.df <- rbind(tmp.month.df,tmp.year.df)
    }
    tmp.state.df <- rbind(tmp.state.df,tmp.month.df)
  }
  tmp.state.mean <- tmp.state.df %>%
    group_by(state,month,year) %>%
    summarize_at(vars(mean), ~mean(., na.rm = T))
  modis.state.monthly.yearly <- rbind(modis.state.monthly.yearly,tmp.state.mean)
}
modis.state.monthly.yearly$mean[is.nan(modis.state.monthly.yearly$mean)] <- NA

for (i in 1:length(states.pick)) {
  modis.state.monthly.yearly[which(modis.state.monthly.yearly$state == states.pick[i]),]
}

trend.list <- dlply(modis.state.monthly.yearly, as.quoted(.(state,month)), failwith(NULL, function(df)
  #summary(rq(get(var) ~ datenum, tau=0.9, data = df))))
  kendallTrendTest(mean ~ years, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95, data = df)))

monthly.trends <- NULL
month.state.names <- names(trend.list)
for (i in 1:length(trend.list)) {
  #i = 1
  tmp.slope <- trend.list[[i]]$estimate[2]
  tmp.pval <- trend.list[[i]]$p.value
  tmp.lcl <- trend.list[[i]]$interval$limits[1]
  tmp.ucl <- trend.list[[i]]$interval$limits[2]
  tmp.df <- data.frame("month" = as.numeric(sub('.*\\.', '', month.state.names[i])),
                       "state" = sub('\\..*', '', month.state.names[i]),
                       "slope" = tmp.slope,
                       "pval" = tmp.pval,
                       "lcl" = tmp.lcl,
                       "ucl" = tmp.ucl)
  monthly.trends <- rbind(monthly.trends,tmp.df)
}

state.trend <- monthly.trends[order(monthly.trends$month),]
day <- c(as.factor(state.trend$state))
# day[c(FALSE,TRUE, FALSE, FALSE)] <- (day[c(FALSE,TRUE, FALSE, FALSE)]*2)
# day[c(FALSE,FALSE, TRUE, FALSE)] <- (day[c(FALSE,FALSE, TRUE, FALSE)]*3) - 1
# day[c(FALSE,FALSE, FALSE, TRUE)] <- (day[c(FALSE,FALSE, FALSE, TRUE)]*3)
#day[c(FALSE,FALSE, FALSE, FALSE, TRUE)] <- (day[c(FALSE,FALSE, FALSE, FALSE, TRUE)]*4)

day[c(F, T, F, F, F, F, F, F)] <- (day[c(F, T, F, F, F, F, F, F)] * 2) + 1
day[c(F, F, T, F, F, F, F, F)] <- (day[c(F, F, T, F, F, F, F, F)] * 2)
day[c(F, F, F, T, F, F, F, F)] <- (day[c(F, F, F, T, F, F, F, F)] * 2)
day[c(F, F, F, F, T, F, F, F)] <- (day[c(F, F, F, F, T, F, F, F)] * 2)
day[c(F, F, F, F, F, T, F, F)] <- (day[c(F, F, F, F, F, T, F, F)] * 2)
day[c(F, F, F, F, F, F, T, F)] <- (day[c(F, F, F, F, F, F, T, F)] * 2)
day[c(F, F, F, F, F, F, F, T)] <- (day[c(F, F, F, F, F, F, F, T)] * 2)

state.trend$Date <- as.Date(paste(c(state.trend$month),day,sep = "."), format = "%m.%d")

ggplot(data = state.trend, aes(Date, slope, col = state)) +
  geom_point() + 
  scale_x_date(breaks = "1 month", date_labels = "%B") +
  theme_bw() +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = c("Dark Green","Dark Blue","Dark Red","Dark Orange","Blue","Green","Dark Green","Orange"), name = "Region", guide = "legend") +
  xlab("Month") +
  ylab("AOD/Year") +
  #ggtitle(paste0(" Regional Monthly ", q.string, "th Quantile Normalized Trends - ", var.string)) +
  theme(
    plot.title = element_text(size=16, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) +
  #ylim(-10,10) +
  geom_hline(yintercept = 0)
