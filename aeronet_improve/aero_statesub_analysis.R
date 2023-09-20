
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
library(scales)
library(maptools)


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

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.05,0.5,0.75,0.90,0.95,0.98)
q.sel <- 0
q.ind <- match(q.sel,q)
q.string <- sprintf("%.2f", q.sel)
q.string <- substr(toString(q.string), start = 3, stop = 4)

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

#require at least 1 observation per day per year
obs.req <- 365

#subset summer and spring data and count number of obs per year
# sumspring.dat <- subset(dat.sub, month(Date) == 3 | month(Date) == 4 |month(Date) == 5 |month(Date) == 6 |month(Date) == 7 |month(Date) == 8)
# #sumspring.dat <- subset(dat.sub, month(Date) == c(3,4,5,6,7,8))
# with(sumspring.dat[sumspring.dat$SiteName == "neon_cvalla",], plot(Date, get(var.sel)))

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

check.yearly.obs(dat.aero, obs.req,var.sel)

#check on sites with large gaps
test.site <- subset(dat.aero, SiteName == "monterey")
ggplot(test.site, aes(x = Date, y = AOD_coarse)) + geom_point() + theme_bw()


#remove all data from before 1996 for tucson since there is a 4 year gap between 1995 and the next group of data
dat.aero <- dat.aero[!(dat.aero$SiteName == "tucson" & year(dat.aero$Date) <= 1997), ]
#remove all data from before 2000 for table mountain because of 9 year gap
dat.aero <- dat.aero[!(dat.aero$SiteName == "table_mountain" & year(dat.aero$Date) <= 2000), ]
#remove all data from before 2005 for Monterey because of 4 year data gap
dat.aero <- dat.aero[!(dat.aero$SiteName == "monterey" & year(dat.aero$Date) <= 2005), ]


#check on site observations again to see subsetted data meet criteria
check.yearly.obs(dat.aero, obs.req, var.sel)


#create new sites vector with only sites that meet criteria
year.check.sub <- subset(year.check, pick == "TRUE")
sites <- unique(year.check.sub$SiteName)

rm(year.check,year.check.sub,test.site)

#subset dat.sub to sites that only meet criteria
dat.aero <- dat.aero[dat.aero$SiteName %in% sites, ]


#generate df with set time period averaged data
aero.averaged.dat <- dat.aero %>%
  group_by(Date = cut(Date, breaks="14 days"),SiteName) %>%
  summarise_all(mean, na.rm = TRUE)
aero.averaged.dat$Date <- as.POSIXct(aero.averaged.dat$Date)


#generate ascending years vector
years <- sort(unique(year(dat.aero$Date)), decreasing = FALSE)


#subset to only desired states
#assign sites to states
#create empty states column
dat.aero$state <- rep(NA,nrow(dat.aero))
for (i in 1:nrow(dat.aero)) {
  #i = 20
  if (as.character(dat.aero[i,1]) == "saturn_island" ) {
    dat.aero$state[i] = "BC"
  } else if (as.character(dat.aero[i,1]) == "trinidad_head" |
             as.character(dat.aero[i,1]) == "monterey" |
             as.character(dat.aero[i,1]) == "fresno" |
             as.character(dat.aero[i,1]) == "ucsb") {
    dat.aero$state[i] = "CA"
  } else if (as.character(dat.aero[i,1]) == "maricopa" |
             as.character(dat.aero[i,1]) == "tucson") {
    dat.aero$state[i] = "AZ"
  } else if (as.character(dat.aero[i,1]) == "hjandrews") {
    dat.aero$state[i] = "OR"
  } else if (as.character(dat.aero[i,1]) == "rimrock") {
    dat.aero$state[i] = "ID"
  } else if (as.character(dat.aero[i,1]) == "bozeman" |
             as.character(dat.aero[i,1]) == "missoula") {
    dat.aero$state[i] = "MT"
  } else if (as.character(dat.aero[i,1]) == "railroad_valley") {
    dat.aero$state[i] = "NV"
  } else if (as.character(dat.aero[i,1]) == "sevilleta" |
             as.character(dat.aero[i,1]) == "white_sands_helstf") {
    dat.aero$state[i] = "NM"
  } else if (as.character(dat.aero[i,1]) == "red_mountain_pass" |
             as.character(dat.aero[i,1]) == "bsrn_bao_boulder" | 
             as.character(dat.aero[i,1]) == "neon_cvalla" | 
             as.character(dat.aero[i,1]) == "table_mountain") {
    dat.aero$state[i] = "CO"
  } else if (as.character(dat.aero[i,1]) == "cart_site") {
    dat.aero$state[i] = "OK"
  } else if (as.character(dat.aero[i,1]) == "konza_edc") {
    dat.aero$state[i] = "KS"
  } else if (as.character(dat.aero[i,1]) == "sioux_falls") {
    dat.aero$state[i] = "SD"
  } else if (as.character(dat.aero[i,1]) == "ames") {
    dat.aero$state[i] = "IA"
  } else if (as.character(dat.aero[i,1]) == "univ_of_houston") {
    dat.aero$state[i] = "TX"
  }
}

#name modis.lonlat.df columns
coord.df <- data.frame("Lon" = dat.aero$Site_Longitude.Degrees., "Lat" = dat.aero$Site_Latitude.Degrees.)
#use getData to get USA shapefile
states <- getData("GADM", country="USA", level=1)
#have to convert modis.lonlat.df to spatialpoints data
points <- SpatialPoints(coord.df)
#match projection for states and points
proj4string(points) <- proj4string(states)
#use over to find where points overlap with states and get state name (NAME_1)
point.state <- as.character(over(points, states)$NAME_1)
#add state names as a new column to modis coordinates
dat.aero$state <- point.state

aero.state.sub <- subset(dat.aero, state == "Montana" | 
                           state == "South Dakota" |
                           state == "Colorado" |
                           state == "Oklahoma" |
                           state == "Kansas" |
                           state == "Iowa")

#number of seconds in a year - need this to convert from change/second to change/year later on
year.sec <- 60*60*24*365

#new sites vector
sitesnew <- unique(aero.state.sub$SiteName)

#in order to perform trend analysis, we need to get the date/time into a numeric value (in seconds for this case)
aero.state.sub$Date_sec <- as.numeric(aero.state.sub$Date)
#run kendall/sens slope analysis and store results in aero.ken.sen.quant
aero.ken.sen.quant <- NULL
for (i in 1:length(sitesnew)) {
  i = 1
  #isolate one site at a time
  tmp.site.quant.obs <- subset(aero.state.sub, SiteName == sitesnew[i])
  #run kendall trend test - the output gives the sens slope, kendall's tau, pvalue based on kendalls tau, and confidence intervals based on gilbert's method
  tmp.ken <- kendallTrendTest(get(var.sel) ~ Date_sec, tmp.site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  #multiply slope by year.sec to go from change/sec to change/year
  tmp.slope <- tmp.ken$estimate[2] * year.sec
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits *year.sec
  #normalize slope and confidence intervals by dividing by the median value for that site within the quantile
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.site.quant.obs[[var.sel]], na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.site.quant.obs[[var.sel]], na.rm, na.rm = TRUE)
  #store all of these values in a temporary dataframe
  tmp.df <- data.frame("site_name" = sitesnew[i], 
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
  #put it all together in aero.ken.sen.quant
  aero.ken.sen.quant <- rbind(aero.ken.sen.quant,tmp.df)
}
