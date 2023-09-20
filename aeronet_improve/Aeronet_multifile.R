
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
library(ggmap)
#library(rgdal)
#library(rworldmap)


#set working directory
setwd("~/AERONET/AERONET_data")
file_list <- list.files()

#lapply(file_list, function(fname){

#read in all files and combine into dat.bind. 
dat.bind <- NULL
for (i in seq_along(file_list)){
  fname <- file_list[i]
  input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T)
  input[input == -999] <- NA
  input$time_utc <- as.POSIXct(paste(as.Date(input$Date.dd.mm.yyyy., format="%d:%m:%Y"), input$Time.hh.mm.ss.))
  input2 <- input %>%
    dplyr::select(time_utc, 
           AOD_1020nm:AOD_340nm, 
           X440.870_Angstrom_Exponent:X440.675_Angstrom_Exponent.Polar.,
           AERONET_Site_Name:Site_Elevation.m.) 
  input2$time_utc <- as.POSIXct(input2$time_utc)
  
  input3 <- input2 %>%
    group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  input3$time_utc <- as.POSIXct(input3$time_utc)
  input3$site_name <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$time_utc))
  
  #assign(gsub("[0-9 \\.\\_\\]", "", fname), value = input2, env = .GlobalEnv)
  #assign(paste(gsub("[0-9 \\.\\_\\]", "", fname),"group", sep = ""), value = input3, env = .GlobalEnv)
  #print('finished ', fname)
  #dat.bind <- dplyr::bind_rows(input3)
  
  dat.bind <- rbind(dat.bind, input3)
  print(paste0('finished ', fname))
  
  #return(dat.bind)
}

#Combine fresno sites. Fresno and Fresno2 are same location
dat.bind <- dat.bind %>% mutate(site_name = replace(site_name, site_name == "fresno_2", "fresno"))
with(dat.bind[dat.bind$site_name == "fresno",], plot(time_utc, AOD_500nm))

#count number of observations per year by site
count.obs <- dat.bind %>%
  group_by(year(time_utc), site_name) %>%
  summarise_at(vars(AOD_500nm), ~sum(!is.nan(.)))

count.obs$count <- count.obs$AOD_500nm

#create barplot stacking observations per year by site
ggplot(data=count.obs, aes(x = `year(time_utc)`, y = count,  fill = site_name)) +
  geom_bar(stat="identity") + 
  theme_bw() + xlab("Date") + ylab("Count") + 
  ggtitle("Observations Per Year by Site") +
  labs(fill='Site Name') 

#get site names in vector
sites <- unique(dat.bind$site_name)

#generate dataframes with data only after 2000 and observation counts greater than 200 respectively
dat.bind.sub <- subset(dat.bind, year(dat.bind$time_utc) >= 2000)
count.obs.sub <- subset(count.obs, count.obs$count >= 200)

#use above dataframes to create one subsetted dataframe with data after 2000 and more than 200 obs per year
dat.sub <- NULL
for (i in 1:nrow(count.obs.sub)) {
  tmp.sub <- subset(dat.bind.sub, dat.bind.sub$site_name == count.obs.sub$site_name[i] & 
                      year(dat.bind.sub$time_utc) == count.obs.sub$`year(time_utc)`[i])
  print(paste0('finished ', count.obs.sub$`year(time_utc)`[i], ' ', count.obs.sub$site_name[i]))
  dat.sub <- rbind(dat.sub,tmp.sub)
}

#test fresno site
with(dat.sub[dat.sub$site_name == "fresno",], plot(time_utc, AOD_500nm))

#generate ascending years vector
years <- sort(unique(year(dat.sub$time_utc)), decreasing = FALSE)

#get quantile for each year at each site. Enter desired quantile
q <- 0.98
quant.aod <- dat.sub %>%
  group_by(year(time_utc), site_name) %>%
  summarise_at(vars(AOD_500nm), ~as.numeric(quantile(AOD_500nm, na.rm = T, probs = q)))

#test quantile for site
with(quant.aod[quant.aod$site_name == sites[1],], plot(`year(time_utc)`, AOD_500nm))

#subset only obs in the 98th quantile for each site for each year
aod.high <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(dat.sub, year(time_utc) == years[i] & site_name == sites[j])
    tmp.quant <- subset(quant.aod, `year(time_utc)` == years[i] & quant.aod$site_name == sites[j])
    tmp.high <- subset(tmp.dat, AOD_500nm >= tmp.quant$AOD_500nm)
    aod.high <- rbind(aod.high,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j]))
  }
}

#check by plotting the 98th quantile obs
with(aod.high[aod.high$site_name == sites[1],], plot(time_utc, AOD_500nm))

#get quantile regression for a site
quant.sub <- subset(aod.high, site_name == sites[1])
plot(quant.sub$time_utc, quant.sub$AOD_500nm)
abline(lm(quant.sub$AOD_500nm ~ quant.sub$time_utc))
summ.one.site <- summary(lm(quant.sub$AOD_500nm ~ quant.sub$time_utc))
slope.one.site <- summ.one.site$coefficients[2,1]

#convert nan to NA
aod.high <- aod.high %>% mutate(site_name == is.nan(.), NA)

#get all slopes of quantile regressions for each site, store in quant.coef
summ.all.sites <- aod.high %>%
  group_by(site_name) %>%
  do(fit = summary(lm(AOD_500nm ~ time_utc, data = .)))
quant.coef = tidy(summ.all.sites,fit)
#pull out only slope
quant.coef <- quant.coef[seq(2,36,by=2),]

#create df with site name, lat and long
site.loc <- NULL
names = c("site_name","Site_Latitude.Degrees.","Site_Longitude.Degrees.")
for (i in 1:length(sites)) {
  #i=1
  tmp.dat <- subset(dat.bind, dat.bind$site_name == sites[i])
  tmp.frame <- tmp.dat[,names]
  site.loc <- rbind(site.loc,tmp.frame[1,])
}

#add lat and long to quant.coef
site.loc <- with(site.loc,  site.loc[order(site.loc$site_name) , ])
quant.coef$lat <- site.loc$Site_Latitude.Degrees.
quant.coef$long <- site.loc$Site_Longitude.Degrees.


#plot lm for all sites
plot(aod.high$time_utc, aod.high$AOD_500nm, type="n")
for (i in 1:length(sites)) {
  tmp.plot <- subset(aod.high, site_name == sites[i])
  abline(lm(tmp.plot$AOD_500nm ~ tmp.plot$time_utc))
}

#plot 98th quantile obs with regression line for site (bozeman)
site.pick <- 17
site.quant <- subset(aod.high, site_name == sites[site.pick])
ggplot(data = site.quant, aes(time_utc,AOD_500nm)) +
  geom_point() + #, 
            #color = "#09557f",
            #alpha = 0.6,
            #size = 0.6) +
  labs(x = "Date", 
       y = "AOD",
       #title = paste0(toupper(substring(sites[site.pick], 1, 1)), substring(sites[site.pick], 2), sep = ""),' Montana') +
       title = "Bozeman, Montana 98th Quantile AOD") +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2000-08-01 00:00:00 UTC"),
                 as.POSIXct("2018-12-31 23:59:59 UTC"), "2 years"))


#yearly average 98th quant for a site
yearly <- site.quant %>%
  group_by(year(time_utc)) %>%
    summarize_at(vars(AOD_500nm),mean,na.rm = T)

#plot yearly averaged 98th quantile with regression fit (bozeman)
fityearly <- lm(AOD_500nm ~ `year(time_utc)`, data = yearly)   
ggplot(data = yearly, aes(`year(time_utc)`,AOD_500nm)) +
  geom_point() + #, 
  #color = "#09557f",
  #alpha = 0.6,
  #size = 0.6) +
  labs(x = "Date", 
       y = "AOD",
       title = "Bozeman, Montana 98th Quantile AOD (Yearly)") +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE) +
  geom_label(aes(x=2010,y=1),
             label = paste("R2 = ",signif(summary(fityearly)$r.squared, 5),
                           #"\nIntercept =",signif(fityearly$coef[[1]],5 ),
                           " \nSlope =",signif(fityearly$coef[[2]], 5),
                           " \nP =",signif(summary(fityearly)$coef[2,4], 5))) +
  xlim(2007,2018)


#number of seconds in a year
year.sec <- 60*60*24*365
#convert aod/second to aod/year
site.lm <- lm(AOD_500nm ~ time_utc, data = site.quant)   
site.slope <- site.lm$coef[[2]]
site.slope <- site.slope * year.sec
site.slope

summ <- summary(lm(site.quant$AOD_500nm ~ site.quant$time_utc))
#get lm for yearly at a site
summ.yearly <- summary(lm(yearly$AOD_500nm ~ yearly$`year(time_utc)`))
f.yearly <- coef(summ.yearly)

#testing quantile regression package
test <- subset(dat.sub, site_name == sites[17])

#get quantile regressions for bozeman and plot
rq(test$AOD_500nm ~ test$time_utc,.98)
plot(test$time_utc,test$AOD_500nm, ylim=c(-2,5))
points(test$time_utc,test$AOD_500nm,cex=.1,col="black")
taus <- c(.05,.1,.25,.75,.9,.98)
xx <- seq(from = min(test$time_utc), to = max(test$time_utc), by = "1 month")
f <- coef(rq((test$AOD_500nm)~(test$time_utc),tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
  lines(xx,yy[,i],col = "blue")
}
abline(lm(test$AOD_500nm ~ test$time_utc),col="red",lty = 2)
abline(rq(test$AOD_500nm ~ test$time_utc), col="green")
legend(3000,500,c("mean (LSE) fit", "median (LAE) fit"),
       col = c("red","blue"),lty = c(2,1))

#another way to plot quantile regressions
p <-ggplot(data = test, aes(time_utc,AOD_500nm)) +
  geom_point(colour = "blue", size = 1) +
  labs(x = "Date", 
       y = "AOD",
       title = "Bozeman, Montana 98th Quantile AOD (Yearly)") +
  theme_minimal()
  
for(i in 1:length(taus)){
    p <- p + geom_abline(slope = f[2,i], intercept = f[1,i],
                na.rm = FALSE, show.legend)
}

p


#get quantile regressions for each site
taus <- c(.05,.1,.25,.75,.9,.98)
f.all <- NULL
for (i in 1:length(sites)) {
  dat.tmp <- subset(dat.sub,site_name == sites[i])
  f.tmp <- coef(rq((dat.tmp$AOD_500nm)~(dat.tmp$time_utc),tau=taus))
  f.tmp <- data.frame(f.tmp)
  f.tmp$site_name <- rep(sapply(sites[i], tolower), nrow(f.tmp))
  f.tmp$lat <- rep(sapply(dat.tmp$Site_Latitude.Degrees.[1], tolower), nrow(f.tmp))
  f.tmp$long <- rep(sapply(dat.tmp$Site_Longitude.Degrees.[1], tolower), nrow(f.tmp))
  f.all <- rbind(f.all,f.tmp)
  print(paste0('finished ',sites[i]))
}
f.all$lat <- as.numeric(f.all$lat)
f.all$long <- as.numeric(f.all$long)
f.slope <- f.all[seq(from = 2, to = nrow(f.all), by = 2),]

#convert quantile regressions from rq package from aod/second to aod/year
for (i in 1:6) {
  f.slope[,i] <- f.slope[,i]*year.sec
}

#plot quantlie regressions
states <- map_data("state")
map.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat,group=group), fill=NA, color="black") +
  coord_fixed(1.3)
map.plot +
  geom_point(data=f.slope, aes(x=long, y = lat, color="black"), cex=abs((f.slope$tau..0.98))*10000000000, color = ifelse(f.slope$tau..0.98 < 0,'blue','red'))


#yearly average 98th quant obs
aod.high.year <- aod.high %>%
  group_by(year(time_utc),site_name) %>%
  summarize_at(c("AOD_500nm","Site_Latitude.Degrees.","Site_Longitude.Degrees."),mean,na.rm = T)

#fit linear model to 98th quant yearly average

f.year <- NULL
for (i in 1:length(sites)) {
  tmp.dat <- subset(aod.high.year,site_name == sites[i])
  tmp.f <- coef(lm(tmp.dat$AOD_500nm ~ tmp.dat$`year(time_utc)`))
  tmp.f <- data.frame(tmp.f)
  tmp.f$site_name <- rep(sapply(sites[i], tolower), nrow(tmp.f))
  tmp.f$lat <- rep(as.numeric(sapply(tmp.dat$Site_Latitude.Degrees.[1], tolower)), nrow(tmp.f))
  tmp.f$long <- rep(as.numeric(sapply(tmp.dat$Site_Longitude.Degrees.[1], tolower)), nrow(tmp.f))
  f.year <- rbind(f.year,tmp.f)
}

f.year.slope <- f.year[seq(from = 2, to = nrow(f.all), by = 2),]

#map lm derived 98th quantile slope from yearly
states <- map_data("state")
year.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -100),ylim = c(30,50),ratio = 1.3)
year.plot +
  geom_point(data=f.year.slope, aes(x=long, y = lat,
             size = abs(f.year.slope$tmp.f),
             color = ifelse(f.year.slope$tmp.f > 0,"Increasing","Decreasing")),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_color_manual(values = c("Blue","Red")) + 
  scale_size_continuous(range = c(1,10)) +
  labs(size = "AOD/year", color = "Trend") +
  theme_bw() + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Trends in 98th Quantile AOD")

#convert quant.coef slopes from aod/sec to aod/year
quant.coef$estimate <- quant.coef$estimate*year.sec

#map quant.coef aod/year all obs 98th quant slopes
states <- map_data("state")
all.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -100),ylim = c(30,50),ratio = 1.3)
all.plot +
  geom_point(data=quant.coef, aes(x=long, y = lat,
                                    size = abs(quant.coef$estimate),
                                    color = ifelse(quant.coef$estimate > 0,"Increasing","Decreasing")),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_color_manual(values = c("Blue","Red")) + 
  scale_size_continuous(range = c(1,10)) +
  labs(size = "AOD/year", color = "Trend") +
  theme_bw() + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Trends in 98th Quantile AOD")


#get 75th quantile for each year at each site
quant.75 <- dat.sub %>%
  group_by(year(time_utc), site_name) %>%
  summarise_at(vars(AOD_500nm), ~as.numeric(quantile(AOD_500nm, na.rm = T, probs = .75)))

#test quantile for site
with(quant.75[quant.75$site_name == sites[1],], plot(`year(time_utc)`, AOD_500nm))

#subset only obs in the 75th quantile for each site for each year
aod.75 <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(dat.sub, year(time_utc) == years[i] & site_name == sites[j])
    tmp.quant <- subset(quant.75, `year(time_utc)` == years[i] & quant.75$site_name == sites[j])
    tmp.high <- subset(tmp.dat, AOD_500nm >= tmp.quant$AOD_500nm)
    aod.75 <- rbind(aod.high,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j]))
  }
}

#check by plotting the 75th quantile obs
with(aod.high[aod.high$site_name == sites[1],], plot(time_utc, AOD_500nm))

#get all slopes of quantile regressions for each site, store in quant.coef
summ.all.sites.75 <- aod.75 %>%
  group_by(site_name) %>%
  do(fit = summary(lm(AOD_500nm ~ time_utc, data = .)))
quant.coef.75 = tidy(summ.all.sites.75,fit)
#pull out only slope
quant.coef.75 <- quant.coef.75[seq(2,36,by=2),]

#add lat and long to quant.coef.75
site.loc <- with(site.loc,  site.loc[order(site.loc$site_name) , ])
quant.coef.75$lat <- site.loc$Site_Latitude.Degrees.
quant.coef.75$long <- site.loc$Site_Longitude.Degrees.

#convert quant.coef slopes from aod/sec to aod/year
quant.coef.75$estimate <- quant.coef.75$estimate*year.sec

#map quant.coef aod/year all obs 98th quant slopes
states <- map_data("state")
all.plot.75 <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-130, -100),ylim = c(30,50),ratio = 1.3)
all.plot.75 +
  geom_point(data=quant.coef.75, aes(x=long, y = lat,
                                  size = abs(quant.coef$estimate),
                                  color = ifelse(quant.coef$estimate > 0,"Increasing","Decreasing")),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_color_manual(values = c("Blue","Red")) + 
  scale_size_continuous(range = c(1,10)) +
  labs(size = "AOD/year", color = "Trend") +
  theme_bw() + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Trends in 75th Quantile AOD")











dat.dust <- subset(dat.bind, X440.870_Angstrom_Exponent <= 1.3) 

#with(dat.dust[dat.dust$site_name == sites[3],], plot(time_utc, AOD_870nm))
#with(dat.bind[dat.bind$site_name == sites[3],], plot(time_utc, AOD_870nm))

quant <- dat.dust %>%
  group_by(year(time_utc), site_name) %>%
  #summarise(n = n())
  summarise_at(vars(AOD_870nm), ~as.numeric(quantile(AOD_870nm, na.rm = T, probs = 0.98)))
  quant$year <- quant$`year(time_utc)`

with(quant[quant$site_name == sites[1],], plot(year, AOD_870nm))

events <- NULL
for (i in 1:length(year)) {
  for (j in 1:length(sites)) {
    #i=1
    #j=1
    tmp.dust <- subset(dat.dust, year(time_utc) == year[i] & site_name == sites[j])
    tmp.quant <- subset(quant, quant$year == year[i] & site_name == sites[j])
    tmp.event <- subset(tmp.dust, AOD_870nm >= tmp.quant$AOD_870nm)
    events <- rbind(events,tmp.event)
    print(paste0('finished ', year[i],' ',sites[j]))
  }
}

#stats <- data.frame(date = as.Date(events$time_utc), aod870 = as.numeric(events$AOD_870nm))

with(events[events$site_name == sites[8],], plot(time_utc, AOD_870nm))

test <- subset(events, site_name == sites[1])
plot(test$time_utc, test$AOD_870nm)
abline(lm(test$AOD_870nm ~ test$time_utc))
summ <- summary(lm(test$AOD_870nm ~ test$time_utc))
slope <- summ$coefficients[2,1]

test <- events %>%
  group_by(site_name) %>%
  
















# bozeman_obs <- Bozemanlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
#   #summarise_all(~sum(!is.na(.)))
# 
# boulder_obs <- BSRNBAOBoulderlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# fresno_obs <- Fresnolevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# hjandrews_obs <- HJAndrewslevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# maricopa_obs <- Maricopalevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# missoula_obs <- Missoulalevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# monterey_obs <- Montereylevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# neon_obs <- NEONCVALLAlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# railroadvalley_obs <- RailroadValleylevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# redmountainpass_obs <- RedMountainPasslevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# rimrock_obs <- Rimrocklevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# saturnisland_obs <- SaturnIslandlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# sevilleta_obs <- Sevilletalevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# tablemountain_obs <- TableMountainlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# trinidadhead_obs <- TrinidadHeadlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# tucson_obs <- Tucsonlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# ucsb_obs <- UCSBlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
# 
# whitesands_obs <- WhiteSandsHELSTFlevgroup %>% 
#   mutate(year = year(time_utc)) %>%
#   group_by(year) %>%
#   summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))

sites <- c("boulder_obs","bozeman_obs","fresno_obs","hjandrews_obs","maricopa_obs","missoula_obs","monterey_obs","neon_obs",
           "railroadvalley_obs","redmountainpass_obs","rimrock_obs","saturnisland_obs","sevilleta_obs","tablemountain_obs",
           "trinidadhead_obs","tucson_obs","ucsb_obs","whitesands_obs")
years <- seq(1990, 2018, by=1)
all_sites_obs <- data.frame(list(year=years))
for (i in 1:18){
  all_sites_obs <- merge(all_sites_obs, get(sites[i]),all=T)
  colnames(all_sites_obs)[i+1] <- sites[i]
}
matrix.allobs <- data.matrix(all_sites_obs)

plot(Bozemanlevgroup$AOD_870nm,Bozemanlevgroup$X440.870_Angstrom_Exponent)
bozeman.dust <- subset(Bozemanlevgroup, X440.870_Angstrom_Exponent <= 1.3) 
                  #select=c(X, Weight))

boxp <- boxplot(bozeman.dust$X440.870_Angstrom_Exponent)
sd(bozeman.dust$X440.870_Angstrom_Exponent, na.rm=T) * 4
hist(bozeman.dust$X440.870_Angstrom_Exponent)

tmp.year <- unique(year(bozeman.dust$time_utc))
test <- bozeman.dust %>%
  filter(year(bozeman.dust$time_utc) == tmp.year[1])
quant <- as.numeric(test$AOD_870nm, na.rm = T, probs = 0.75)
test <- test %>%
  filter(AOD_870nm >= quant)





df.sub <- data %>%
  select(time_utc, 
         AOD_1020nm:AOD_340nm, 
         X440.870_Angstrom_Exponent:X440.675_Angstrom_Exponent.Polar.,
         AERONET_Site_Name:Site_Elevation.m.) %>%
  group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
  summarise_at(vars(-AERONET_Site_Name, -time_utc), funs(mean(., na.rm=T))) 

df.sub$time_utc <- as.POSIXct(df.sub$time_utc)
plot(df.sub$time_utc, df.sub$X440.870_Angstrom_Exponent, type='o')
boxp <- boxplot(df.sub$X440.870_Angstrom_Exponent)
sd(df.sub$X440.870_Angstrom_Exponent, na.rm=T) * 4
hist(df.sub$X440.870_Angstrom_Exponent)

na <- df.sub %>% 
  mutate(year = year(time_utc)) %>%
  group_by(year) %>%
  #summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
  summarise_all(~sum(!is.na(.)))

year_dif <- tail(year(df.sub$time_utc), -1) - head(year(df.sub$time_utc), -1)
max_dif <- which.max(year_dif)


if (max(year_dif) > 2) {
  test <- df.sub[-c(1:max_dif),]
}

plot(test$time_utc,test$X440.870_Angstrom_Exponent, type='o')
