#clear environments
rm(list=ls())

#load packages
library("EnvStats", lib.loc="/usr/local/lib/R/site-library")
library("lubridate", lib.loc="/usr/local/lib/R/site-library")
library("dplyr", lib.loc="/usr/local/lib/R/site-library")
library("data.table", lib.loc="/usr/local/lib/R/site-library")
library("tidyr", lib.loc="/usr/local/lib/R/site-library")
library("broom", lib.loc="/usr/local/lib/R/site-library")
library("quantreg", lib.loc="/usr/local/lib/R/site-library")
library("zoo", lib.loc="/usr/local/lib/R/site-library")
library("gridExtra", lib.loc="/usr/local/lib/R/site-library")
#library("mblm", lib.loc="/usr/local/lib/R/site-library")
library(sp)
library(maptools)
library(maps)
library(rgdal)
library(reshape2)
library(scales)

#set working directory
setwd("~/AERONET/aeronet_og")
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

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0.90
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

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
test.site <- subset(dat.aero, SiteName == "table_mountain")
ggplot(test.site, aes(x = Date, y = AOD_coarse)) + geom_point() + theme_bw()

#remove all data from before 2000 for table mountain because of 9 year gap
dat.aero <- dat.aero[!(dat.aero$SiteName == "table_mountain" & year(dat.aero$Date) <= 2000), ]

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

#call quant_reg_boot_map_functions.R file for mapping and quantile regression functions
# setwd("~/")
# source("quant_reg_boot_map_functions.R")
# 
# #use function on aeronet data to get quantile regression for desired variable
# aero.quant.reg <- site.quant.reg(df = dat.aero, 
#                                  network = "AERONET", 
#                                  sitevec = aero.sites, 
#                                  time.conv = year.sec, 
#                                  var = aero.var.sel,
#                                  R.num = 1000)

#kendall trend test does not work for large AERONET dataset
# aero.ken.list <- dlply(dat.aero, "SiteName", function(df)
#   kendallTrendTest(get(aero.var.sel) ~ datenum, data = df, na.action = na.pass, ci.slope = T, conf.level = 0.95))

#begin IMPROVE portion

#set working directory as /IMPROVE
setwd("~/IMPROVE")

#read in data, keep header. I downloaded them at separate times so have to read in both files separately and then combine them
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

#remove all obs with POC = 2, these are basically duplicate measurements from another instrument
improve.dat <- improve.dat[!(improve.dat$POC==2),]

#get date vector into date format
improve.dat$Date <- as.Date(improve.dat$Date, "%m/%d/%Y")

#select desired variable out of soil_pm25,pm25,pm25_rec,coarse,pm10,pm10_rec, or totalcarbon
improve.var.choices <- c("soil_pm25","pm25","pm25_rec","coarse","pm10","pm10_rec","totalcarbon")
improve.var.sel <- "coarse"

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

#require 50% of all possible observations each year
improve.obs.req <- 0.5*(365/3)

#run function to check if network meets annual observational requirements for at least 7 years
check.yearly.obs(improve.dat,improve.obs.req,improve.var.sel)

#check plots for sites requiring further inspection here
test.site <- subset(improve.dat, SiteName == "Meadview")
ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()

#Remove 1998 data from Table Mountain site since it is separated by an 11 year gap from the rest of the data
improve.dat <- improve.dat[!(improve.dat$SiteName == "Meadview" & year(improve.dat$Date) < 2003), ]

#run function to check if network meets annual observational requirements for at least 7 years again
check.yearly.obs(improve.dat,improve.obs.req,improve.var.sel)

#create new sites vector with only sites that meet criteria
improve.year.check.sub <- subset(year.check, pick == "TRUE")
improve.sites <- unique(improve.year.check.sub$SiteName)

#subset improve.summer to sites that only meet criteria
improve.dat <- improve.dat[improve.dat$SiteName %in% improve.sites, ]

#generate ascending years vector
improve.years <- sort(unique(year(improve.dat$Date)), decreasing = FALSE)

improve.dat <- subset(improve.dat, State == "ND" |
                   State == "MT" |
                   State == "WY" |
                   State == "CO")

improve.dat$datenum <- as.numeric(improve.dat$Date)
 
improve.ken.list <- dlply(improve.dat, "SiteName", function(df)
  kendallTrendTest(get(improve.var.sel) ~ datenum, data = df, na.action = na.pass, ci.slope = T, conf.level = 0.95))

improve.info <- improve.dat %>%
  summarise_at(vars(Latitude,Longitude), ~unique(.))

improve.median <- improve.dat %>%
  group_by(SiteName) %>%
  summarise_at(vars(all_of(improve.var.sel)), ~median(., na.rm = T))

improve.ken.sen.trends <- data.frame("SiteName" = names(improve.ken.list),
                   "Latitude" = improve.info$Latitude,
                   "Longitude" = improve.info$Longitude,
                   "pval" = sapply(improve.ken.list, function(x) x$p.value),
                   "normslope" = (((sapply(improve.ken.list, function(x) x$estimate[2]))*365)/improve.median$coarse)*100,
                   "LCL" = ((unlist(sapply(improve.ken.list, function(x) x$interval$limits[[1]]), use.names = FALSE)*365)/improve.median$coarse)*100,
                   "UCL" = ((unlist(sapply(improve.ken.list, function(x) x$interval$limits[[2]]), use.names = FALSE)*365)/improve.median$coarse)*100
)


setwd("~/")
#export table to csv so we don't have to go through this ridciulously long process again
write.csv(improve.ken.sen.trends,"improve_ken_sen_oilandgas_coarse_trends.csv", row.names = TRUE)

improve.ken.sen.trends <- na.omit(improve.ken.sen.trends)

for (i in 1:nrow(improve.ken.sen.trends)) {
  if (improve.ken.sen.trends$normslope[i] >= 0) {
    improve.ken.sen.trends$sign[i] <- "Increasing"
  } else {
    improve.ken.sen.trends$sign[i] <- "Decreasing"
  }
}

for (i in 1:nrow(improve.ken.sen.trends)) {
  if (improve.ken.sen.trends$pval[i] <= 0.05) {
    improve.ken.sen.trends$sig[i] <- "< 0.05"
  } else {
    improve.ken.sen.trends$sig[i] <- "> 0.05"
  }
}

#get map for states to plot
states <- map_data("state")

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                       region == "colorado" | 
                       region == "montana" | 
                       region == "north dakota")

#first plot improve by itself, then aero and improve together
ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-116, -96.5),ylim = c(37,49),ratio = 1.3) +
  geom_point(data=improve.ken.sen.trends, aes(x=Longitude, y = Latitude,
                                             size = abs(normslope),
                                             colour = sign,
                                             fill = sign,
                                             shape = sig),
             group = FALSE) +
  scale_color_manual(values = c("Blue","Red")) +
  #1 is for open circle (not significant), 21 is filled circle (significant)
  scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05")) +
  scale_fill_manual(values = c("Blue","Red"), 
                    guide = FALSE) +
  #setting the range of the possible sizes of circles
  scale_size_continuous(range = c(1,10)) +
  #titles for each legend
  labs(size = "%/year", 
       color = "Trend",
       shape = "P-Val") +
  #have to override aesthetics for legend to manually fill shape 21 for statistical significance
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black"), order = 1),
         size = guide_legend(order = 2),
         color = guide_legend(order = 3)) +
  guides(fill = FALSE) +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Course AOD and PM")) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 





#check high quantile events
dat.aero.2017 <- subset(dat.aero, year(Date) > 2017)
limit <- quantile(dat.aero.2017$AOD_coarse, 0.98, na.rm = T)

high <- which(dat.aero.2017$AOD_coarse > limit, arr.ind = TRUE)

aero.high <- dat.aero.2017[high,]

#aero.high <- subset(aero.high, year(Date) > 2017)

# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }

modes <- data.frame(sort(table(aero.high$Day_of_Year), decreasing = TRUE)[1:25])

# getmode(high[,3])
pick <- 1
mode.date <- modes[pick,1]
modis.date.sub[as.numeric(as.character(mode.date))]
test <- dust.green.og[,,as.numeric(as.character(mode.date))]