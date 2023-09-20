
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
library(htmlTable)
library(officer)
library(flextable)
library(magrittr)

#set working directory
#setwd("~/AERONET/AERONET_coarse")
setwd("~/AERONET/aeronet_og")
#get filenames
file_list <- list.files()
#new files are slightly different, read them in separately
#new_files = file_list[which(file_list == "19960101_20131231_Bratts_Lake.ONEILL_lev20" | file_list == "20040101_20181231_Kelowna_UAS.ONEILL_lev20")]
#file_list <- file_list[-(which(file_list == "19960101_20131231_Bratts_Lake.ONEILL_lev20" | file_list == "20040101_20181231_Kelowna_UAS.ONEILL_lev20"))]

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

#read in new files. Only read in the first 52 columns since that column was removed to read in the other aeronet data
# new_aero <- NULL
# for (i in seq_along(new_files)){
#   #i=1
#   #pull out ith filename
#   fname <- new_files[i]
#   #read file, skip first 6 rows, read in first 53 columns, set 1st row as column names
#   input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T, select = c(1:52))
#   #change all -999 values to NA
#   input[input == -999] <- NA
#   #combine date and time
#   input$time_utc <- as.POSIXct(paste(input$Date_.dd.mm.yyyy., input$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
#   # reorder - put date/time at front 
#   input2 <- input %>%
#     dplyr::select(time_utc, everything())
#   #apply 30 minute smoothing - average every 30 minutes
#   # input3 <- input2 %>%
#   #   group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
#   #   summarise_if(is.numeric, mean, na.rm = TRUE)
#   #change time to posixct
#   # input3$time_utc <- as.POSIXct(input3$time_utc)
#   #create sitename var
#   # input3$site_name <- rep(sapply(input2$AERONET_Site_Name[1], tolower), length(input3$time_utc))
#   #use rbind to add to dat.bind df
#   new_aero <- rbind(new_aero, input2)
#   print(paste0('finished ', fname))
# }

#combine all aeronet data
#dat.aero <- rbind(dat.aero,new_aero)

rm(new_files)

#rename columns to easier names
colnames(dat.aero)[colnames(dat.aero)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.aero)[colnames(dat.aero)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.aero)[colnames(dat.aero)=="Total_AOD_500nm.tau_a."] <- "AOD_total"
#only need next line if not averaging over time
#colnames(dat.aero)[colnames(dat.aero)=="AERONET_Site_Name"] <- "SiteName"

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

#2019 data is incomplete, subset to all years before 2019
dat.aero <- subset(dat.aero, year(dat.aero$Date) < 2019)


#get climo of aeronet data
dat.aero.clean <- dat.aero[!is.na(dat.aero$AOD_total),]
dat.aero.vars <- dat.aero.clean[,c(4:6,ncol(dat.aero.clean))]

#check data availability for aeronet data
aero.site.info <- dat.aero.clean %>%
  group_by(SiteName) %>%
  summarise_all(~mean(.))

obscount <- dat.aero.clean %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(AOD_total,AOD_fine,AOD_coarse), ~sum(!is.na(.)))

obsmax <- dat.aero.clean %>%
  group_by(SiteName) %>%
  summarise_at(vars(Date), ~max(.))

obsmin <- dat.aero.clean %>%
  group_by(SiteName) %>%
  summarise_at(vars(Date), ~min(.))

aveann <- obscount %>%
  group_by(SiteName) %>%
  summarise_all(~mean(.))

aveall <- dat.aero.vars %>%
  group_by(SiteName) %>%
  summarise_all(list(~mean(.,na.rm = T), ~sd(.,na.rm = T)))

aveall[,2:7] <- signif(aveall[,2:7], digits = 2)

aveall$totalchar <- as.character(paste0(aveall$AOD_total_mean, " ± ", aveall$AOD_total_sd))
aveall$finechar <- as.character(paste0(aveall$AOD_fine_mean, " ± ", aveall$AOD_fine_sd))
aveall$coarsechar <- as.character(paste0(aveall$AOD_coarse_mean, " \u00B1 ", aveall$AOD_coarse_sd))



aero.climo.obs <- data.frame("Site" = aveall$SiteName,
                             "Latitude" = aero.site.info$Site_Latitude.Degrees.,
                             "Longitude" = aero.site.info$Site_Longitude.Degrees.,
                             "Data begins" = obsmin$Date, 
                             "Data ends" = obsmax$Date,
                             "Average annual obs frequency" = aveann$AOD_total,
                             "AOD fine mean" = aveall$finechar,
                             "AOD coarse mean" = aveall$coarsechar,
                             "AOD total mean" = aveall$totalchar)

aero.climo.obs$AOD.fine.mean <- as.character(aero.climo.obs$AOD.fine.mean)
aero.climo.obs$AOD.coarse.mean <- as.character(aero.climo.obs$AOD.coarse.mean)
aero.climo.obs$AOD.total.mean <- as.character(aero.climo.obs$AOD.total.mean)

#clean the data - make it look nice
#aero.climo.obs[,6:9] <- signif(aero.climo.obs[,6:9], digits = 4)
aero.climo.obs$Average.annual.obs.frequency <- round(aero.climo.obs$Average.annual.obs.frequency)
aero.climo.obs$Data.begins <- format(as.Date(aero.climo.obs$Data.begins))
aero.climo.obs$Data.ends <- format(as.Date(aero.climo.obs$Data.ends))

#sort from largest to smallest coarse AOD



# Create flextable object to output as a table in word
aero.ft <- flextable(data = aero.climo.obs) %>% 
  theme_zebra %>% 
  autofit
# See flextable in RStudio viewer
aero.ft

# Create a temp file
tmp <- tempfile(tmpdir = "/home/andylambert/grad_deliverables", fileext = ".docx")

# Create a docx file
read_docx() %>% 
  body_add_flextable(aero.ft) %>% 
  print(target = tmp)

# open word document
#browseURL(tmp)

#names(aero.climo.obs)<-str_replace_all(names(aero.climo.obs), c("." = " "))
write.csv(aero.climo.obs,"/home/andylambert/grad_deliverables/aeronet_data_availability_climo.csv")
write.table(aero.climo.obs,"/home/andylambert/grad_deliverables/aeronet_data_availability_climo.txt")




#count number of years of data for each site
aero.sites <- unique(dat.aero.clean$SiteName)
aero.sites.years <- NULL
for (i in 1:length(aero.sites)) {
  #i=1
  tmp.site <- aero.sites[i]
  tmp.dat <- subset(dat.aero.clean, SiteName == tmp.site)
  tmp.years <- length(unique(year(tmp.dat$Date)))
  tmp.df <- data.frame("SiteName" = tmp.site,
                       "Years" = tmp.years)
  aero.sites.years <- rbind(aero.sites.years,tmp.df)
}
#convert sitename from factor to character to order
aero.sites.years$SiteName <- as.character(aero.sites.years$SiteName)

#order alphabetically to match other dataframe
aero.sites.years <- aero.sites.years %>%
  arrange(SiteName)

#add years column to improve.climo.obs
aero.climo.obs$Years <- aero.sites.years$Years

#create factor vector for sets of years
aero.climo.obs$range <- NA
for (i in 1:nrow(aero.climo.obs)) {
  #i=1
  if (aero.climo.obs$Years[i] < 10) {
    aero.climo.obs$range[i] <- "< 10"
  } else if (aero.climo.obs$Years[i] >= 10 & aero.climo.obs$Years[i] < 15) {
    aero.climo.obs$range[i] <- ">= 10, < 15" 
  } else if (aero.climo.obs$Years[i] >= 15) {
    aero.climo.obs$range[i] <- ">= 15"
  }
}
aero.climo.obs$range <- as.factor(aero.climo.obs$range)


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

#get climo of aeronet data
#dat.aero.clean <- dat.aero[!is.na(dat.aero$AOD_total),]
improve.dat.vars <- improve.dat[,c(5,19,33,40,54)]

# ag.imp <- subset(improve.dat, State == "ND" |
#                    State == "SD" |
#                    State == "NE" |
#                    State == "KS" |
#                    State == "OK" |
#                    State == "IA" |
#                    State == "MO" |
#                    State == "MN")
# 
# improve.pm25 <- ag.imp[!is.na(ag.imp$coarse)]

improve.pm25 <- improve.dat[!is.na(improve.dat$coarse)]

#check data availability for aeronet data
improve.site.info <- improve.pm25 %>%
  group_by(SiteName) %>%
  summarise_all(~mean(.))

improve.obscount <- improve.dat %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(soil_pm25,coarse,pm25,pm10), ~sum(!is.na(.)))

improve.obsmax <- improve.pm25 %>%
  group_by(SiteName) %>%
  summarise_at(vars(Date), ~max(.))

improve.obsmin <- improve.pm25 %>%
  group_by(SiteName) %>%
  summarise_at(vars(Date), ~min(.))

improve.aveann <- improve.obscount %>%
  group_by(SiteName) %>%
  summarise_all(~mean(.))

improve.aveall <- improve.dat.vars %>%
  group_by(SiteName) %>%
  summarise_all(list(~mean(.,na.rm = T), ~sd(., na.rm = T)))

improve.aveall[,2:9] <- signif(improve.aveall[,2:9], digits = 2)

improve.aveall$pm10char <- as.character(paste0(improve.aveall$pm10_mean, " ± ", improve.aveall$pm10_sd))
improve.aveall$pm25char <- as.character(paste0(improve.aveall$pm25_mean, " ± ", improve.aveall$pm25_sd))
improve.aveall$coarsechar <- as.character(paste0(improve.aveall$coarse_mean, " ± ", improve.aveall$coarse_sd))
improve.aveall$soilchar <- as.character(paste0(improve.aveall$soil_pm25_mean, " ± ", improve.aveall$soil_pm25_sd))




improve.climo.obs <- data.frame("Site" = improve.aveall$SiteName,
                             "Latitude" = improve.site.info$Latitude,
                             "Longitude" = improve.site.info$Longitude,
                             "Data begins" = improve.obsmin$Date, 
                             "Data ends" = improve.obsmax$Date,
                             #"Average annual pm25 obs frequency" = improve.aveann$pm25,
                             #"Average annual fine soil obs frequency" = improve.aveann$soil_pm25,
                             #"Average annual pm10 obs frequency" = improve.aveann$pm10,
                             "Average annual coarse pm obs frequency" = improve.aveann$coarse,
                             "pm25 mean" = improve.aveall$pm25char,
                             "fine soil mean" = improve.aveall$soilchar,
                             "pm10 mean" = improve.aveall$pm10char,
                             "coarse pm mean" = improve.aveall$coarsechar)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

improve.climo.obs[is.nan(improve.climo.obs)] <- NA

improve.climo.obs[improve.climo.obs == 0] <- NA

improve.climo.obs$Average.annual.coarse.pm.obs.frequency <- round(improve.climo.obs$Average.annual.coarse.pm.obs.frequency)

#improve.climo.obs[,6:13] <- signif(improve.climo.obs[,6:13], digits = 4)

#convert nan +- na to na
na.ind <- which(is.na(improve.climo.obs$Average.annual.coarse.pm.obs.frequency) == TRUE)

improve.climo.obs$pm10.mean[na.ind] <- NA
improve.climo.obs$coarse.pm.mean[na.ind] <- NA

# Create flextable object to output as a table in word
improve.ft <- flextable(data = improve.climo.obs) %>% 
  theme_zebra %>% 
  autofit
# See flextable in RStudio viewer
improve.ft

# Create a temp file
tmp <- tempfile(tmpdir = "/home/andylambert/grad_deliverables", fileext = ".docx")

# Create a docx file
read_docx() %>% 
  body_add_flextable(improve.ft) %>% 
  print(target = tmp)

# open word document
#browseURL(tmp)


write.csv(improve.climo.obs,"/home/andylambert/grad_deliverables/improve_data_availability_climo.csv")
write.table(improve.climo.obs,"/home/andylambert/grad_deliverables/improve_data_availability_climo.txt")

#count number of years of data for each site
improve.sites <- unique(improve.pm25$SiteName)
improve.sites.years <- NULL
for (i in 1:length(improve.sites)) {
  #i=1
  tmp.site <- improve.sites[i]
  tmp.dat <- subset(improve.pm25, SiteName == tmp.site)
  tmp.years <- length(unique(year(tmp.dat$Date)))
  tmp.df <- data.frame("SiteName" = tmp.site,
                       "Years" = tmp.years)
  improve.sites.years <- rbind(improve.sites.years,tmp.df)
}
#convert sitename from factor to character to order
improve.sites.years$SiteName <- as.character(improve.sites.years$SiteName)

#order alphabetically to match other dataframe
improve.sites.years <- improve.sites.years %>%
                          arrange(SiteName)

#add years column to improve.climo.obs
improve.climo.obs$Years <- improve.sites.years$Years

#create factor vector for sets of years
improve.climo.obs$range <- NA
for (i in 1:nrow(improve.climo.obs)) {
  #i=1
  if (improve.climo.obs$Years[i] < 10) {
    improve.climo.obs$range[i] <- "< 10"
  } else if (improve.climo.obs$Years[i] >= 10 & improve.climo.obs$Years[i] < 15) {
    improve.climo.obs$range[i] <- ">= 10, < 15" 
  } else if (improve.climo.obs$Years[i] >= 15) {
    improve.climo.obs$range[i] <- ">= 15"
  }
}
improve.climo.obs$range <- as.factor(improve.climo.obs$range)

#count number of sites satisfying length of time of observations
z <- improve.climo.obs$Years > 20
z.l <- length(z[z == TRUE])
print(z.l)

#map data availability for improve and aeronet
#get map for states to plot
states <- map_data("state")

#set color scale. assign factors to colors
scale_fill_assign <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(c('blue', 'orange', 'red'), c("< 10", ">= 10, < 15", ">= 15")), 
    ...
  )
}
#set order for legend
improve.climo.obs$range <- factor(improve.climo.obs$range, levels = c("< 10", ">= 10, < 15", ">= 15"))
aero.climo.obs$range <- factor(aero.climo.obs$range, levels = c("< 10", ">= 10, < 15", ">= 15"))


#Create a custom color scale
myColors <- brewer.pal(3,"Set1")
names(myColors) <- levels(aero.climo.obs$range)
colScale <- scale_colour_manual(name = "range",values = myColors)

aero.obs <- aero.climo.obs[,c(1:3,10,11)]
aero.obs$network <- "AERONET"
improve.obs <- improve.climo.obs[,c(1:3,14,15)]
improve.obs$network <- "IMPROVE"

network.combo <- rbind(improve.obs, aero.obs)
network.combo$network <- as.factor(network.combo$network)



#map aeronet trends
p <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-126, -90),ylim = c(27,50),ratio = 1.3)
p <- p +
  #plot improve data
  geom_point(data=network.combo, aes(x=Longitude,
                                         y = Latitude,
                                         color = range,
                                     shape = network),
             size = 2,
             group = FALSE) +
  scale_colour_manual(drop = TRUE, limits = levels(aero.climo.obs$range), values = c("blue", "orange","red")) +
  scale_shape_manual(name = "Network", values = c(17,15), limits = levels(network.combo$network)) +
  #scale_fill_assign() +
  theme_bw() +
  labs(color = "# Years with Obs") +
  xlab("Longitude") +
  ylab("Latitude")
  #ggtitle(paste0("IMPROVE and AERONET sites and data availability"))

p


aero.p <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-126, -90),ylim = c(27,50),ratio = 1.3)
aero.p <- aero.p +
  #plot improve data
  # geom_point(data=improve.climo.obs, aes(x=Longitude,
  #                                        y = Latitude,
  #                                        color = range),
  #            shape = 15,
  #            size = 2,
  #            group = FALSE) +
  #plot aeronet data
  geom_point(data=aero.climo.obs, aes(x=Longitude, 
                                      y = Latitude,
                                      color = as.character(range)),
             shape = 17,
             size = 3,
             group = FALSE) +
  scale_colour_manual(drop = TRUE, limits = levels(aero.climo.obs$range), values = c("blue", "orange","red")) +
  #colScale +
  #scale_fill_manual(values = c("blue", "green" ,"red")) +
  #scale_fill_assign() +
  theme_bw() +
  labs(color = "# Years with Obs") +
  xlab("Longitude") +
  ylab("Latitude")
  #ggtitle(paste0("IMPROVE and AERONET sites and data availability"))
aero.p


improve.p <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-126, -90),ylim = c(27,50),ratio = 1.3)
improve.p <- improve.p +
  #plot improve data
  geom_point(data=improve.climo.obs, aes(x=Longitude,
                                         y = Latitude,
                                         color = range),
             shape = 15,
             size = 2,
             group = FALSE) +
  #plot aeronet data
  # geom_point(data=aero.climo.obs, aes(x=Longitude, 
  #                                     y = Latitude,
  #                                     color = as.character(range)),
  #            shape = 17,
  #            size = 3,
  #            group = FALSE) +
  scale_colour_manual(drop = TRUE, limits = levels(aero.climo.obs$range), values = c("blue", "orange","red")) +
  #colScale +
  #scale_fill_manual(values = c("blue", "green" ,"red")) +
  #scale_fill_assign() +
  theme_bw() +
  labs(color = "# Years with Obs") +
  xlab("Longitude") +
  ylab("Latitude")
  #ggtitle(paste0("IMPROVE and AERONET sites and data availability"))
improve.p
