
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


#set working directory
setwd("~/AERONET/AERONET_coarse")
#get filenames
file_list <- list.files()
#new files are slightly different, read them in separately
new_files = file_list[which(file_list == "19960101_20131231_Bratts_Lake.ONEILL_lev20" | file_list == "20040101_20181231_Kelowna_UAS.ONEILL_lev20")]
file_list <- file_list[-(which(file_list == "19960101_20131231_Bratts_Lake.ONEILL_lev20" | file_list == "20040101_20181231_Kelowna_UAS.ONEILL_lev20"))]

#read in all files and combine into dat.bind. 
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
  input$time_utc <- as.POSIXct(paste(input$Date_.dd.mm.yyyy., input$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
  # reorder - put date/time at front 
  input2 <- input %>%
    dplyr::select(time_utc, everything())
  #apply 30 minute smoothing - average every 30 minutes
  # input3 <- input2 %>%
  #   group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
  #   summarise_if(is.numeric, mean, na.rm = TRUE)
  #change time to posixct
  # input3$time_utc <- as.POSIXct(input3$time_utc)
  #create sitename var
  # input3$site_name <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$time_utc))
  #use rbind to add to dat.bind df
  dat.aero <- rbind(dat.aero, input2)
  print(paste0('finished ', fname))
}

rm(input,input2)
rm(file_list,fname,header)

#read in new files. Only read in the first 52 columns since that column was removed to read in the other aeronet data
new_aero <- NULL
for (i in seq_along(new_files)){
  #i=1
  #pull out ith filename
  fname <- new_files[i]
  #read file, skip first 6 rows, read in first 53 columns, set 1st row as column names
  input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T, select = c(1:52))
  #change all -999 values to NA
  input[input == -999] <- NA
  #combine date and time
  input$time_utc <- as.POSIXct(paste(input$Date_.dd.mm.yyyy., input$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
  # reorder - put date/time at front 
  input2 <- input %>%
    dplyr::select(time_utc, everything())
  #apply 30 minute smoothing - average every 30 minutes
  # input3 <- input2 %>%
  #   group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
  #   summarise_if(is.numeric, mean, na.rm = TRUE)
  #change time to posixct
  # input3$time_utc <- as.POSIXct(input3$time_utc)
  #create sitename var
  # input3$site_name <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$time_utc))
  #use rbind to add to dat.bind df
  new_aero <- rbind(new_aero, input2)
  print(paste0('finished ', fname))
}

#combine all aeronet data
dat.aero <- rbind(dat.aero,new_aero)

#rename columns to easier names
colnames(dat.aero)[colnames(dat.aero)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.aero)[colnames(dat.aero)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.aero)[colnames(dat.aero)=="Total_AOD_500nm.tau_a."] <- "AOD_total"

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

#select from AOD_fine and AOD_coarse
aero.var.choices <- c("AOD_coarse","AOD_fine","AOD_total")
aero.var.sel <- "AOD_fine"

#get var name for plot titles
if (aero.var.sel == aero.var.choices[1]) {
  aero.var.string <- "Coarse AOD"
} else if (aero.var.sel == aero.var.choices[2]) {
  aero.var.string <- "Fine AOD"
} else if (aero.var.sel == aero.var.choices[3]) {
  aero.var.string <- "Total AOD"
}

#get obs only in summer fire season (day of year 170 to 220)
aero.summer <- subset(dat.aero, Day_of_Year >= 170 & Day_of_Year <= 220)

#subset aeronet sites west of -100 degrees longitude
aero.summer <- subset(aero.summer,Site_Longitude.Degrees. <= -100)

#require an average of one obs per day
aero.obs.req <- 220-170

# count number of non NA obs
aero.summer.obs <- aero.summer %>%
  group_by(year(time_utc), AERONET_Site_Name) %>%
  summarise_at(vars(aero.var.sel), ~sum(!is.nan(.)))

#get maxiumum year gap for each site
aero.dat.lag <- aero.summer %>%
  group_by(AERONET_Site_Name) %>%
  mutate(Diff = year(time_utc) - lag(year(time_utc))) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

#check plots for sites requiring further inspection here
test.site <- subset(aero.summer, AERONET_Site_Name == "Fresno")
ggplot(test.site, aes(x = time_utc, y = AOD_fine)) + geom_point() + theme_bw()

#Remove 1998 data from Table Mountain site since it is separated by an 11 year gap from the rest of the data
aero.summer <- aero.summer[!(aero.summer$AERONET_Site_Name == "Table_Mountain" & year(aero.summer$time_utc) == 1998), ]
#Remove 2002 and 2003 data from Monterey site since 2003 is separated by 4 year gap till 2007
aero.summer <- aero.summer[!(aero.summer$AERONET_Site_Name == "Monterey" & year(aero.summer$time_utc) <= 2003), ]

# count number of non NA obs again
aero.summer.obs <- aero.summer %>%
  group_by(year(time_utc), AERONET_Site_Name) %>%
  summarise_at(vars(aero.var.sel), ~sum(!is.nan(.)))

#get maximum year gap for each site again
aero.dat.lag <- aero.summer %>%
  group_by(AERONET_Site_Name) %>%
  mutate(Diff = year(time_utc) - lag(year(time_utc))) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

#check how many years satisfy observation number requirement
aero.summer.obs$check <- aero.summer.obs[[aero.var.sel]] >= aero.obs.req
aero.year.check <- aero.summer.obs %>%
  group_by(AERONET_Site_Name) %>%
  summarise_at(vars(check), ~sum(check))
aero.year.check$diff <- aero.dat.lag$Diff

#check if number of years that meet obs criteria - the number of years between years with data is at least 7 years
aero.year.check$bestcons <- aero.year.check$check - aero.year.check$diff + 1
aero.year.check$pick <- aero.year.check$bestcons >= 5

#create new sites vector with only sites that meet criteria
year.check.sub <- subset(aero.year.check, pick == "TRUE")
sites <- unique(year.check.sub$AERONET_Site_Name)

#subset aero.summer to sites that only meet criteria
aero.summer <- aero.summer[aero.summer$AERONET_Site_Name %in% sites, ]

#generate ascending years vector
years <- sort(unique(year(aero.summer$time_utc)), decreasing = FALSE)





#get quantile for each year at each site. Enter desired quantile
aero.dat.quant <- NULL
for (i in 1:length(q)) {
  tmp.dat <- aero.summer %>%
    group_by(year(time_utc), AERONET_Site_Name) %>%
    summarise_at(vars(aero.var.sel), ~as.numeric(quantile(get(aero.var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    aero.dat.quant <- tmp.dat
    colnames(aero.dat.quant)[i+2] <- q[i]
  } else {
    aero.dat.quant[,i+2] <- tmp.dat[[aero.var.sel]]
    colnames(aero.dat.quant)[i+2] <- q[i]
  }
}

#subset only obs in the desired quantile for each site for each year
aero.event.obs <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(aero.summer, year(time_utc) == years[i] & AERONET_Site_Name == sites[j])
    tmp.quant <- subset(aero.dat.quant, `year(time_utc)` == years[i] & aero.dat.quant$AERONET_Site_Name == sites[j])
    tmp.high <- subset(tmp.dat, get(aero.var.sel) >= tmp.quant[[q.ind+2]])
    aero.event.obs <- rbind(aero.event.obs,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j]))
  }
}

#number of seconds in a year (used to get slope in change/year rather than change/second)
year.sec <- 60*60*24*365

#get date/time into a numeric value to use for trend test and regression
aero.event.obs$time_utc_sec <- as.numeric(aero.event.obs$time_utc)
#create empty dataframe to store important values in for loop
aero.ken.sen.quant <- NULL
for (i in 1:length(sites)) {
  #i = 1
  #temporary site data for iteration of for loop
  tmp.site.quant.obs <- subset(aero.event.obs, AERONET_Site_Name == sites[i])
  #Use kendallTrendTest funciton to get sens slope, kendall tau, and others. Get 95% confidence intervals
  tmp.ken <- kendallTrendTest(get(aero.var.sel) ~ time_utc_sec, tmp.site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  tmp.slope <- tmp.ken$estimate[2] * year.sec
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits *year.sec
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.site.quant.obs[[aero.var.sel]], na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.site.quant.obs[[aero.var.sel]], na.rm, na.rm = TRUE)
  tmp.df <- data.frame("site_name" = sites[i],
                       "network" = "AERONET",
                       "lat" = tmp.site.quant.obs$Site_Latitude.Degrees.[1], 
                       "lon" = tmp.site.quant.obs$Site_Longitude.Degrees.[1], 
                       "slope" = tmp.slope,
                       "normslope" = tmp.normslope * 100,
                       "pval" = tmp.p,
                       "tau" = tmp.tau,
                       "intercept" = tmp.intercept,
                       "LCI" = tmp.ci[1],
                       "normLCI" = tmp.normci[1] * 100,
                       "UCI" = tmp.ci[2],
                       "normUCI" = tmp.normci[2] * 100)
  aero.ken.sen.quant <- rbind(aero.ken.sen.quant,tmp.df)
}

rm("test.site","tmp.dat","tmp.df","tmp.high","tmp.ken","tmp.quant","tmp.site.quant.obs","tmp.ci",
   "tmp.intercept","tmp.normci","tmp.normslope","tmp.p","tmp.slope","tmp.tau")

aero.ken.sen.quant$nonzero <- !(aero.ken.sen.quant$LCI <= 0 & aero.ken.sen.quant$UCI >= 0)

#map quant.coef aod/year all obs 98th quant slopes
states <- map_data("state")
#northamerica <- map_data("canada")

aero.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-126, -90),ylim = c(27,50),ratio = 1.3)
aero.plot <- aero.plot +
  geom_point(data=aero.ken.sen.quant, aes(x=lon, y = lat,
                                     size = abs(aero.ken.sen.quant$normslope),
                                     color = ifelse(aero.ken.sen.quant$normslope > 0,"Increasing","Decreasing"),
                                     fill = ifelse(aero.ken.sen.quant$pval < 0.1,ifelse(aero.ken.sen.quant$normslope > 0, "Increasing","Decreasing"),"p-val < 0.1")),
             shape = 24,
             #fill = "transparent",
             #alpha = ken.sen.quant$pval <= 0.1),#, no = ">",yes = "<")),
             # alpha = quant.coef$p.value),#, no = ">",yes = "<")),
             #fill = quant.coef$p.value),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_color_manual(values = c("Blue","Red")) +
  scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE) +
  scale_size_continuous(range = c(1,7)) +
  #scale_alpha_manual(values = c(.0001, .001), guide = FALSE) +
  #plot on values less than pvalue threshold
  scale_alpha_manual(values = c(0,1), guide = FALSE) +
  #scale_alpha_continuous(trans = "reverse") +
  #scale_alpha_continuous(breaks=c(.0001, .001, 0.01, 0.05,0.3, 1),trans = "reverse") +
  #scale_alpha_continuous(trans = "reverse") +
  labs(size = "%/year", color = "Trend", alpha = "P-val") +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste0("Trends in ", q.string, "th Quantile ", aero.var.string)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14))

aero.plot
#finished with AERONET portion

#begin IMPROVE portion

#set working directory as /IMPROVE
setwd("~/IMPROVE")

#read in data, keep header
data1 <- fread("IMPROVE.txt", stringsAsFactors = F, header = T, skip = 0)
data2 <- fread("IMPROVE2.txt", stringsAsFactors = F, header = T, skip = 0)

improve.dat <- rbind(data1,data2)
rm("data1","data2")

#call all -999 values NA
improve.dat[improve.dat == -999] <- NA

#rename columns for pm10, pm25, reconstructed, soil, coarse, total carbon
colnames(improve.dat)[colnames(improve.dat)=="SOILf:Value"] <- "soil_pm25"
colnames(improve.dat)[colnames(improve.dat)=="RCTM:Value"] <- "pm10_rec"
colnames(improve.dat)[colnames(improve.dat)=="MT:Value"] <- "pm10"
colnames(improve.dat)[colnames(improve.dat)=="TCf:Value"] <- "totalcarbon"
colnames(improve.dat)[colnames(improve.dat)=="MF:Value"] <- "pm25"
colnames(improve.dat)[colnames(improve.dat)=="CM_calculated:Value"] <- "coarse"
colnames(improve.dat)[colnames(improve.dat)=="RCFM:Value"] <- "pm25_rec"

#remove all obs with POC = 2
improve.dat <- improve.dat[!(improve.dat$POC==2),]

#get date vector into date format
improve.dat$Date <- as.Date(improve.dat$Date, "%m/%d/%Y")

#select desired variable out of soil_pm25,pm25,pm25_rec,coarse,pm10,pm10_rec, or totalcarbon
improve.var.choices <- c("soil_pm25","pm25","pm25_rec","coarse","pm10","pm10_rec","totalcarbon")
improve.var.sel <- "pm25"

#get var name for plot titles
if (improve.var.sel == improve.var.choices[1]) {
  improve.var.string <- "Fine Soil"
} else if (improve.var.sel == improve.var.choices[2]) {
  improve.var.string <- "Fine Mode"
} else if (improve.var.sel == improve.var.choices[3]) {
  improve.var.string <- "Reconstructed Fine"
} else if (improve.var.sel == improve.var.choices[4]) {
  improve.var.string <- "Coarse Mode"
} else if (improve.var.sel == improve.var.choices[5]) {
  improve.var.string <- "PM10"
} else if (improve.var.sel == improve.var.choices[6]) {
  improve.var.string <- "Reconstructed PM10"
} else if (improve.var.sel == improve.var.choices[7]) {
  improve.var.string <- "Total Carbon"
}

#create day of year vector frome date vector
improve.dat$doy <- as.numeric(strftime(improve.dat$Date, format = "%j"))

#get obs only in summer fire season (day of year 170 to 220)
improve.summer <- subset(improve.dat, doy >= 170 & doy <= 220)

#subset sites west of -100 degrees longitude
improve.summer <- subset(improve.summer,Longitude <= -100)


#require 50% of possible observations in summer
improve.obs.req <- 0.5*((220-170)/3)

# count number of non NA obs
improve.summer.obs <- improve.summer %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(improve.var.sel), ~sum(!is.nan(.)))

#get maxiumum year gap for each site
improve.dat.lag <- improve.summer %>%
  group_by(SiteName) %>%
  mutate(Diff = year(Date) - lag(year(Date))) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

#check plots for sites requiring further inspection here
test.site <- subset(improve.summer, SiteName == "Yellowstone NP 2")
ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()

#Remove 1998 data from Table Mountain site since it is separated by an 11 year gap from the rest of the data
improve.summer <- improve.summer[!(improve.summer$SiteName == "Meadview" & year(improve.summer$Date) < 2003), ]

# count number of non NA obs again
improve.summer.obs <- improve.summer %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(improve.var.sel), ~sum(!is.nan(.)))

#get maxiumum year gap for each site again
improve.dat.lag <- improve.summer %>%
  group_by(SiteName) %>%
  mutate(Diff = year(Date) - lag(year(Date))) %>%
  summarise_at(vars(Diff), ~max(Diff, na.rm = T))

#check how many years satisfy observation number requirement
improve.summer.obs$check <- improve.summer.obs[[improve.var.sel]] >= improve.obs.req
improve.year.check <- improve.summer.obs %>%
  group_by(SiteName) %>%
  summarise_at(vars(check), ~sum(check))
improve.year.check$diff <- improve.dat.lag$Diff

#check if number of years that meet obs criteria - the number of years between years with data is at least 7 years
improve.year.check$bestcons <- improve.year.check$check - improve.year.check$diff + 1
improve.year.check$pick <- improve.year.check$bestcons >= 7

#create new sites vector with only sites that meet criteria
improve.year.check.sub <- subset(improve.year.check, pick == "TRUE")
improve.sites <- unique(improve.year.check.sub$SiteName)

#subset improve.summer to sites that only meet criteria
improve.summer <- improve.summer[improve.summer$SiteName %in% improve.sites, ]

#generate ascending years vector
improve.years <- sort(unique(year(improve.summer$Date)), decreasing = FALSE)


#get quantile for each year at each site
improve.dat.quant <- NULL
for (i in 1:length(q)) {
  #i=1
  tmp.dat <- improve.summer %>%
    group_by(year(Date), SiteName) %>%
    summarise_at(vars(improve.var.sel), ~as.numeric(quantile(get(improve.var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    improve.dat.quant <- tmp.dat
    colnames(improve.dat.quant)[i+2] <- q[i]
  } else {
    improve.dat.quant[,i+2] <- tmp.dat[[improve.var.sel]]
    colnames(improve.dat.quant)[i+2] <- q[i]
  }
  print(paste0('finished ', q[i]))
}


#subset only obs in the chosen quantile for each site for each year
improve.var.quant <- NULL
for (i in 1:length(improve.years)) {
  for (j in 1:length(improve.sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(improve.summer, year(Date) == improve.years[i] & SiteName == improve.sites[j])
    tmp.quant <- subset(improve.dat.quant, `year(Date)` == improve.years[i] & improve.dat.quant$SiteName == improve.sites[j])
    tmp.high <- subset(tmp.dat, get(improve.var.sel) >= tmp.quant[[q.ind+2]])
    improve.var.quant <- rbind(improve.var.quant,tmp.high)
    print(paste0('finished ', improve.years[i],' ',improve.sites[j]))
  }
}

#number of days in a year to be used for change/day to change/year later
year.day <- 365

#get vector of date as numeric (in seconds)
improve.var.quant$Date_sec <- as.numeric(improve.var.quant$Date)


# count number of non NA obs in quantile for each site
improve.summer.quant.obs <- improve.var.quant %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(improve.var.sel), ~sum(!is.nan(.)))

#calculate slope coefficient using kendall and sens slope analysis
improve.ken.sen.quant <- NULL
for (i in 1:length(improve.sites)) {
  #i = 1
  tmp.site.quant.obs <- subset(improve.var.quant, SiteName == improve.sites[i])
  tmp.ken <- kendallTrendTest(get(improve.var.sel) ~ Date_sec, tmp.site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  tmp.slope <- tmp.ken$estimate[2] * year.day
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits *year.day
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.site.quant.obs[[improve.var.sel]], na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.site.quant.obs[[improve.var.sel]], na.rm, na.rm = TRUE)
  tmp.df <- data.frame("site_name" = improve.sites[i], 
                       "network" = "IMPROVE",
                       "lat" = tmp.site.quant.obs$Latitude[1], 
                       "lon" = tmp.site.quant.obs$Longitude[1],
                       #"state" = tmp.site.quant.obs$State[1],
                       "slope" = tmp.slope,
                       "normslope" = tmp.normslope * 100,
                       "pval" = tmp.p,
                       "tau" = tmp.tau,
                       "intercept" = tmp.intercept,
                       "LCI" = tmp.ci[1],
                       "normLCI" = tmp.normci[1] * 100,
                       "UCI" = tmp.ci[2],
                       "normUCI" = tmp.normci[2] * 100)
  improve.ken.sen.quant <- rbind(improve.ken.sen.quant,tmp.df)
}

rm(tmp.dat,tmp.df,tmp.high,tmp.ken,tmp.quant,tmp.site.quant.obs,tmp.ci,tmp.intercept,tmp.normci,tmp.normslope,tmp.p,tmp.slope,tmp.tau,i,j)

improve.ken.sen.quant$nonzero <- !(improve.ken.sen.quant$LCI <= 0 & improve.ken.sen.quant$UCI >= 0)

network.ken.sen.quant <- NULL
network.ken.sen.quant <- rbind(aero.ken.sen.quant,improve.ken.sen.quant)

for (i in 1:nrow(network.ken.sen.quant)) {
  if (network.ken.sen.quant$pval[i] <= 0.05) {
    network.ken.sen.quant$sig[i] <- "< 0.05"
  } else {
    network.ken.sen.quant$sig[i] <- "> 0.05"
  }
}

network.ken.sen.quant$networksig <- paste0(network.ken.sen.quant$network," ",network.ken.sen.quant$sig)

for (i in 1:nrow(network.ken.sen.quant)) {
  if (network.ken.sen.quant$normslope[i] >= 0) {
    network.ken.sen.quant$sign[i] <- "Increasing"
  } else {
    network.ken.sen.quant$sign[i] <- "Decreasing"
  }
}

all.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -100),ylim = c(31.5,50),ratio = 1.3)
all.plot <- all.plot +
  geom_point(data=network.ken.sen.quant, aes(x=lon, y = lat,
                                          size = abs(normslope),
                                          colour = sign,
                                          #colour = ifelse(normslope > 0,"Increasing","Decreasing"),
                                          fill = sign,
                                          shape = networksig),
             #shape = 24,
             #fill = "transparent",
             #alpha = ken.sen.quant$pval <= 0.1),#, no = ">",yes = "<")),
             # alpha = quant.coef$p.value),#, no = ">",yes = "<")),
             #fill = quant.coef$p.value),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_colour_manual(values = c("Blue","Red")) +
  scale_fill_manual(values = c("Blue","Red"), guide = FALSE) +
  scale_shape_manual(values = c(24,2,15,0), name = "Network & P-val") +
  scale_size_continuous(range = c(1,7)) +
  #scale_alpha_manual(values = c(.0001, .001), guide = FALSE) +
  #plot on values less than pvalue threshold
  #scale_alpha_manual(values = c(0,1), guide = FALSE) +
  #scale_alpha_continuous(trans = "reverse") +
  #scale_alpha_continuous(breaks=c(.0001, .001, 0.01, 0.05,0.3, 1),trans = "reverse") +
  #scale_alpha_continuous(trans = "reverse") +
  labs(size = "%/year", color = "Trend", alpha = "P-val") +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Fine Mode AOD and PM")) +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", aero.var.string)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14))

all.plot



#analyze nfan data
setwd("/home/andylambert/NFAN")
list.files()
nfan.dat <- fread("spl_2011_2018_H", stringsAsFactors = F, check.names = T)
nfan.dat$site <- rep("spl", length(ncol(nfan.dat)))
#create a day of year vector from date vector
nfan.dat$doy <- strftime(nfan.dat$DateTimeUTC, format = "%j")

#rename date and scattering columns
colnames(nfan.dat)[colnames(nfan.dat)=="DateTimeUTC"] <- "Date"
colnames(nfan.dat)[colnames(nfan.dat)=="BsB_S11"] <- "bluescat"
colnames(nfan.dat)[colnames(nfan.dat)=="BsG_S11"] <- "greenscat"
colnames(nfan.dat)[colnames(nfan.dat)=="BsR_S11"] <- "redscat"

#select variable for nfan
nfan.var.choice <- c("bluescat","greenscat","redscat")
nfan.var.sel <- "greenscat"

#get obs only in summer fire season (day of year 170 to 220)
nfan.summer <- subset(nfan.dat, doy >= 170 & doy <= 220)

#get years vector for nfan sites
nfan.years <- sort(unique(year(nfan.summer$Date)), decreasing = FALSE)

#get names of nfan sites
nfan.sites <- unique(nfan.summer$site)


#get quantile for each year at each site
nfan.dat.quant <- NULL
for (i in 1:length(q)) {
  #i=1
  tmp.dat <- nfan.summer %>%
    group_by(year(Date), site) %>%
    summarise_at(vars(nfan.var.sel), ~as.numeric(quantile(get(nfan.var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    nfan.dat.quant <- tmp.dat
    colnames(nfan.dat.quant)[i+2] <- q[i]
  } else {
    nfan.dat.quant[,i+2] <- tmp.dat[[nfan.var.sel]]
    colnames(nfan.dat.quant)[i+2] <- q[i]
  }
  print(paste0('finished ', q[i]))
}


#subset only obs in the chosen quantile for each site for each year
nfan.var.quant <- NULL
for (i in 1:length(nfan.years)) {
  for (j in 1:length(nfan.sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(nfan.summer, year(Date) == nfan.years[i] & site == nfan.sites[j])
    tmp.quant <- subset(nfan.dat.quant, `year(Date)` == nfan.years[i] & nfan.dat.quant$site == nfan.sites[j])
    tmp.high <- subset(tmp.dat, get(nfan.var.sel) >= tmp.quant[[q.ind+2]])
    nfan.var.quant <- rbind(nfan.var.quant,tmp.high)
    print(paste0('finished ', nfan.years[i],' ',nfan.sites[j]))
  }
}

#get vector of date as numeric (in seconds)
nfan.var.quant$Date_sec <- as.numeric(as.POSIXct(nfan.var.quant$Date))

#add temporary lat and lon from spl website
nfan.var.quant$lat <- rep(40.455, nrow(nfan.var.quant))
nfan.var.quant$lon <- rep(-106.744, nrow(nfan.var.quant))

#calculate slope coefficient using kendall and sens slope analysis
nfan.ken.sen.quant <- NULL
for (i in 1:length(nfan.sites)) {
  #i = 1
  tmp.site.quant.obs <- subset(nfan.var.quant, site == nfan.sites[i])
  tmp.ken <- kendallTrendTest(get(nfan.var.sel) ~ Date_sec, tmp.site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  tmp.slope <- tmp.ken$estimate[2] * year.sec
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits *year.sec
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.site.quant.obs[[nfan.var.sel]], na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.site.quant.obs[[nfan.var.sel]], na.rm, na.rm = TRUE)
  tmp.df <- data.frame("site_name" = nfan.sites[i], 
                       "network" = "Neph",
                       "lat" = tmp.site.quant.obs$lat[1], 
                       "lon" = tmp.site.quant.obs$lon[1],
                       #"state" = tmp.site.quant.obs$State[1],
                       "slope" = tmp.slope,
                       "normslope" = tmp.normslope * 100,
                       "pval" = tmp.p,
                       "tau" = tmp.tau,
                       "intercept" = tmp.intercept,
                       "LCI" = tmp.ci[1],
                       "normLCI" = tmp.normci[1] * 100,
                       "UCI" = tmp.ci[2],
                       "normUCI" = tmp.normci[2] * 100)
  nfan.ken.sen.quant <- rbind(nfan.ken.sen.quant,tmp.df)
}

rm(tmp.dat,tmp.df,tmp.high,tmp.ken,tmp.quant,tmp.site.quant.obs,tmp.ci,tmp.intercept,tmp.normci,tmp.normslope,tmp.p,tmp.slope,tmp.tau,i,j)

nfan.ken.sen.quant$nonzero <- !(nfan.ken.sen.quant$LCI <= 0 & nfan.ken.sen.quant$UCI >= 0)

network.ken.sen.quant <- NULL
network.ken.sen.quant <- rbind(aero.ken.sen.quant,improve.ken.sen.quant, nfan.ken.sen.quant)

for (i in 1:nrow(network.ken.sen.quant)) {
  if (network.ken.sen.quant$pval[i] <= 0.05) {
    network.ken.sen.quant$sig[i] <- "< 0.05"
  } else {
    network.ken.sen.quant$sig[i] <- "> 0.05"
  }
}

#make all network names uppercase for purposes of legends in figures
# network.ken.sen.quant$network < toupper(network.ken.sen.quant$network)

network.ken.sen.quant$networksig <- paste0(network.ken.sen.quant$network," ",network.ken.sen.quant$sig)


for (i in 1:nrow(network.ken.sen.quant)) {
  if (network.ken.sen.quant$normslope[i] >= 0) {
    network.ken.sen.quant$sign[i] <- "Increasing"
  } else {
    network.ken.sen.quant$sign[i] <- "Decreasing"
  }
}

#remove canada observations
#network.ken.sen.quant <- subset(network.ken.sen.quant, lat <= 49)
#get canada map
canada = map_data("world","Canada")
mexico = map_data("world","Mexico")

#make plot with aeronet, improve, and nfan data of different shapes

all.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -101),ylim = c(31.5,50),ratio = 1.3) + 
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black")
all.plot <- all.plot +
  geom_point(data=network.ken.sen.quant, aes(x=lon, y = lat,
                                             size = abs(normslope),
                                             colour = sign,
                                             #colour = ifelse(normslope > 0,"Increasing","Decreasing"),
                                             fill = sign,
                                             shape = networksig),
             #shape = 24,
             #fill = "transparent",
             #alpha = ken.sen.quant$pval <= 0.1),#, no = ">",yes = "<")),
             # alpha = quant.coef$p.value),#, no = ">",yes = "<")),
             #fill = quant.coef$p.value),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_colour_manual(values = c("Blue","Red"), guide = FALSE) +
  scale_fill_manual(values = c("Blue","Red"), guide = FALSE) +
  scale_shape_manual(values = c(24,2,15,0,23), name = "Network & P-val", guide = FALSE) +
  scale_size_continuous(range = c(1,7), guide = FALSE) +
  #scale_alpha_manual(values = c(.0001, .001), guide = FALSE) +
  #plot on values less than pvalue threshold
  #scale_alpha_manual(values = c(0,1), guide = FALSE) +
  #scale_alpha_continuous(trans = "reverse") +
  #scale_alpha_continuous(breaks=c(.0001, .001, 0.01, 0.05,0.3, 1),trans = "reverse") +
  #scale_alpha_continuous(trans = "reverse") +
  labs(size = "%/year", color = "Trend", alpha = "p-val") +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  theme(legend.position = "right") +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Fine Mode AOD and PM")) +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", aero.var.string)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))

all.plot







#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

plotRGB((rgb*0.0010000000474974513), 3,2,1)
plot(rgb*0.0010000000474974513)

#put data in dataframe
modis.df.tmp.test <- as.data.frame(rgb, xy = T)
#subset to desired region by lat and long
modis.df.tmp.sub.test <- subset(modis.df.tmp.test, y >= 30 & y <= 50 & x >= -130 & x <= -100)
#bands are blue - 470, green - 550, red - 660, name columns appropiately
names(modis.df.tmp.sub.test) <- c("lon","lat","blue","green","red")
#scale factor
modis.scale.factor <- 0.00100000004749745
#multiply measurements by scale factor to get true AOD value
modis.df.tmp.sub.test[,3:5] <- modis.df.tmp.sub.test[,3:5] * modis.scale.factor
#create xtabs matrix from df
#modis.matrix.tmp.test <- xtabs(green ~ lat + lon, data = modis.df.tmp.sub)



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
  tmp.summer.ind <- which(as.numeric(substrRight(tmp.yeardoy,5)) >= 170 & as.numeric(substrRight(tmp.yeardoy,5)) <= 220)
  tmp.yeardoy <- tmp.yeardoy[tmp.summer.ind]
  tmp.file.path <- tmp.file.path[tmp.summer.ind]
  
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
  modis.df.tmp.sub <- subset(modis.df.tmp, y >= 30 & y <= 50 & x >= -130 & x <= -100)
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

ggplot() + geom_tile(data = modis.df.tmp.sub, aes(x = lon, y = lat, fill = green)) +
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

which(modis.array.green[,,1] == 0.092)
#get observations in desired quantile for each gridpoint
modis.lonlat <- as.matrix(expand.grid(modis.lon,modis.lat))
modis.trend.df <- data.frame(modis.lonlat)
colnames(modis.trend.df) <- c("lon","lat")
modis.trend.df[,"slope"] <- NA
modis.trend.df[,"pval"] <- NA
modis.trend.df[,"normslope"] <- NA

#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))

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
      tmp.dat.year <- modis.array.green[j,k,ind.year]
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
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", guide = FALSE) +
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
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
  #ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018"))






#get separate trends for first and second ~decades of modis data separately

modis.trend.df.first10 <- data.frame(modis.lonlat)
colnames(modis.trend.df.first10) <- c("lon","lat")
modis.trend.df.first10[,"slope"] <- NA
modis.trend.df.first10[,"pval"] <- NA
modis.trend.df.first10[,"normslope"] <- NA
#get quantile for each year at each gridpoint and trends in quantile for first decade
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 5
    #k = 15
    point.quant.obs <- NULL
    for (i in 11:length(modis.years)) {
      #i = 1
      tmp.year <- modis.years[i]
      ind.year <- which(year(modis.date) == tmp.year)
      tmp.dat.year <- modis.array.green[j,k,ind.year]
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
      aod.ind <- which(modis.trend.df.first10$lon == modis.lon[k] & modis.trend.df.first10$lat == modis.lat[j])
      modis.trend.df.first10[aod.ind,3] <- tmp.slope
      modis.trend.df.first10[aod.ind,4] <- tmp.p
      modis.trend.df.first10[aod.ind,5] <- (tmp.slope/median(point.quant.obs$aod_quant, na.rm = TRUE))*100
    } else {
      aod.ind <- which(modis.trend.df.first10$lon == modis.lon[k] & modis.trend.df.first10$lat == modis.lat[j])
      modis.trend.df.first10[aod.ind,3] <- NA
      modis.trend.df.first10[aod.ind,4] <- NA
      modis.trend.df.first10[aod.ind,5] <- NA
    }
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
  }
}

#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = modis.trend.df.first10$lat,"long" = modis.trend.df.first10$lon)))

#filter to get only us tiles
modis.us.sub.first10 <- modis.trend.df.first10[point.filter,]

#make ss vector for statistical significane
modis.us.sub.first10$ss <- modis.us.sub.first10$pval <= 0.05
modis.us.sub.first10$ss[!modis.us.sub.first10$ss] <- NA

modis.decade.plot <- ggplot(data = modis.us.sub.first10, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.us.sub.first10[!is.na(modis.us.sub.first10$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", guide = FALSE) +
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
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018"))





#now omi analysis

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

omi <- ggplot(data = omi.us.sub, aes(x = lon, y = lat, fill = normslope)) + geom_tile() +
  geom_tile(data = omi.us.sub[!is.na(omi.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", guide = FALSE) +
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
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))

grid.arrange(all.plot,omi,modis.plot,modis.decade.plot,nrow = 2)
