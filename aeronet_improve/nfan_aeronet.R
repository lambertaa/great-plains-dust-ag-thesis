
rm(list=ls())

library(dplyr)
library(lubridate)
library(tidyr)
library(ggpubr)

#set path for nfan data and get filenames
path <- "NFAN/"
filenames <- list.files(path)

#run loop through filenames and store in dat.nfan
dat.nfan <- NULL
for (i in 1:length(filenames)) {
  #i = 5
  #get site name
  tmp.site <- substr(filenames[i], start = 1, stop = 3)
  input <- fread(paste0(path,filenames[i]), stringsAsFactors = F, check.names = T)
  #store site name in column
  input$site <- rep(tmp.site, length(ncol(input)))
  dat.nfan <- rbind(dat.nfan,input, use.names = FALSE)
}

rm(input)

#set path to aeronet data and get filenames
path.aero <- "aero_col_nfan/not_tot/"
filenames.aero <- list.files(path.aero)

#run loop through filenames and store data in dat.aero
dat.aero <- NULL
for (i in 1:length(filenames.aero)) {
  input <- fread(paste0(path.aero,filenames.aero[i]), stringsAsFactors = F, check.names = T)
  #change -999 values to NA
  input[input == -999] <- NA
  #make new time column with new format
  input$time_utc <- as.POSIXct(paste(input$Date.dd.mm.yyyy., input$Time.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
  #hourly average data, store in input2
  #create site name column
  #input2$site_name <- rep(sapply(input$AERONET_Site_Name[1], tolower), nrow(input2))
  dat.aero <- rbind(dat.aero,input)
  print(paste0('finished ', filenames.aero[i]))
}

rm(input)

#remove columns with only NA values
dat.aero <- dat.aero %>% 
  select_if(colSums(!is.na(.)) > 0)

#select desired columns (time,site,location,aod,angstrom)
dat.aero <- dat.aero[,c(51,31,32,33,34,5:12,23:27)]

#create hourly averaged dataset and store in aero.hour
aero.hour <- dat.aero %>%
  group_by(AERONET_Site_Name, time_utc = cut(time_utc, breaks="1 hour")) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

#keep only nfan data observed at the same time as AERONET for each site using aero.hour
#corresponding sites are appalachian state - app, arm_sgp - sgp, bondville - bnd, steamboat_springs - spl, trinidad_head - thd
#create colocated name in aero.hour df
aero.hour$coname <- rep(NA,nrow(aero.hour))
aerositename <- unique(aero.hour$AERONET_Site_Name)
for (i in 1:nrow(aero.hour)) {
  if (aero.hour$AERONET_Site_Name[i] == aerositename[1]) {
    aero.hour[i,19] <- "app"
  } else if (aero.hour$AERONET_Site_Name[i] == aerositename[2]) {
    aero.hour[i,19] <- "bnd"
  } else if (aero.hour$AERONET_Site_Name[i] == aerositename[3]) {
    aero.hour[i,19] <- "sgp"
  } else if (aero.hour$AERONET_Site_Name[i] == aerositename[4]) {
    aero.hour[i,19] <- "spl"
  } else if (aero.hour$AERONET_Site_Name[i] == aerositename[5]) {
    aero.hour[i,19] <- "thd"
  }  
}

#change time variable name to time_utc in nfan to create common variable name for merging
colnames(dat.nfan)[colnames(dat.nfan)=="DateTimeUTC"] <- "time_utc"
#get vector of site names for subsetting in for loop below
sites <- unique(aero.hour$coname)

#run for loop that subsets aero and nfan data for one site at a time, merges them together for common time_utc, and stores in aero.nfan.merge
aero.nfan.merge <- NULL
for (i in 1:length(sites)) {
  tmp.aero <- subset(aero.hour,coname == sites[i])
  tmp.nfan <- subset(dat.nfan,site == sites[i])
  tmp.combo <- merge(x = tmp.aero,y = tmp.nfan, by = "time_utc", all = FALSE)
  aero.nfan.merge <- rbind(aero.nfan.merge,tmp.combo)
} 

#monthly average merged data for each site
merge.monthly <- aero.nfan.merge %>%
  group_by(coname, year(time_utc), month(time_utc)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
merge.monthly$date <- as.Date(paste0(merge.monthly$`year(time_utc)`,"-",merge.monthly$`month(time_utc)`,"-01"))

#monthly median merged data for each site
merge.median <- aero.nfan.merge %>%
  group_by(coname, year(time_utc), month(time_utc)) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)
merge.median$date <- as.Date(paste0(merge.median$`year(time_utc)`,"-",merge.median$`month(time_utc)`,"-01"))


for (i in 1:length(sites)) {
  site.sub <- subset(merge.monthly, coname == sites[2])
  site.sub.cor <- cor.test(site.sub$AOD_440nm,site.sub$BsB_S11, method = "pearson", use = "complete.obs")
  
  p <- ggplot(site.sub, aes(x = date)) +
    geom_line(aes(y = BsB_S11, colour = "Total Scattering")) +
    geom_line(aes(y = AOD_440nm*100, colour = "AOD")) +
    scale_y_continuous(sec.axis = sec_axis(~./100, name = "AOD at 440nm")) +
    theme_bw() +
    labs(y = paste0("Total Scattering at 450 nm (", expression(m^-1),")"),
         x = "Date",
         color = "Parameter") +
    scale_colour_manual(values = c("blue","red")) +
    annotate("text", x = max(site.sub$date, na.rm = T), y = max(site.sub$BsB_S11, na.rm = T),label = paste0("r = ",as.character(site.sub.cor$estimate))) +
    ggtitle(paste0(site.sub$coname[1]," AOD and Total Scattering"))
  
  print(p)
}

#investigate correlation for bnd before and after gap
bnd.sub <- subset(merge.monthly, coname == sites[2])
bnd.gap <- which.max(bnd.sub$`year(time_utc)` - lag(bnd.sub$`year(time_utc)`))
bnd.before.gap <- bnd.sub[1:(bnd.gap -1),]
bnd.after.gap <- bnd.sub[bnd.gap:nrow(bnd.sub),]

bnd.before.gap.cor <- cor.test(bnd.before.gap$AOD_440nm,bnd.before.gap$BsB_S11, method = "pearson", use = "complete.obs")
bnd.after.gap.cor <- cor.test(bnd.after.gap$AOD_440nm,bnd.after.gap$BsB_S11, method = "pearson", use = "complete.obs")
