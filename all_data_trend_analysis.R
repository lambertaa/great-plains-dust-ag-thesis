
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
q.sel <- 0.90
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
test.site <- subset(dat.aero, SiteName == "cart_site")
test.site <- test.site[!is.na(test.site$AOD_coarse), ]
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


#get quantile for each year at each site. Enter desired quantile
dat.quant <- NULL
for (i in 1:length(q)) {
  #group dat.aero by year and site and get each quantile value in q
  tmp.dat <- dat.aero %>%
    group_by(year(Date), SiteName) %>%
    summarise_at(vars(var.sel), ~as.numeric(quantile(get(var.sel), na.rm = T, probs = q[i])))
  # if it's the first iteration in the for loop use the full tmp.dat df for dat.quant and name 3rd column the quantile
  if (i == 1) {
    dat.quant <- tmp.dat
    colnames(dat.quant)[i+2] <- q[i]
    # otherwise only add quantile data as the last column
  } else {
    dat.quant[,i+2] <- tmp.dat[[var.sel]]
    colnames(dat.quant)[i+2] <- q[i]
  }
}

#subset only obs in the chosen quantile for each site for each year
aero.quant <- NULL
for (i in 1:length(years)) {
  for (j in 1:length(sites)) {
    #i=18
    #j=5
    tmp.dat <- subset(dat.aero, year(Date) == years[i] & SiteName == sites[j])
    tmp.quant <- subset(dat.quant, `year(Date)` == years[i] & dat.quant$SiteName == sites[j])
    tmp.high <- subset(tmp.dat, get(var.sel) >= tmp.quant[[q.ind+2]])
    aero.quant <- rbind(aero.quant,tmp.high)
    print(paste0('finished ', years[i],' ',sites[j])) 
  }
}

#number of seconds in a year - need this to convert from change/second to change/year later on
year.sec <- 60*60*24*365

#in order to perform trend analysis, we need to get the date/time into a numeric value (in seconds for this case)
aero.quant$Date_sec <- as.numeric(aero.quant$Date)
#run kendall/sens slope analysis and store results in aero.ken.sen.quant
aero.ken.sen.quant <- NULL
for (i in 1:length(sites)) {
  #i = 1
  #isolate one site at a time
  tmp.site.quant.obs <- subset(aero.quant, SiteName == sites[i])
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
  #put it all together in aero.ken.sen.quant
  aero.ken.sen.quant <- rbind(aero.ken.sen.quant,tmp.df)
  print(paste0("finished ",sites[i]))
}

#check to see if upper and lower confidence intervals are the same sign, meaning there is a 95% chance that the trend is nonzero
aero.ken.sen.quant$nonzero <- !(aero.ken.sen.quant$LCI <= 0 & aero.ken.sen.quant$UCI >= 0)

#check statistical significance
aero.ken.sen.quant$ss <- as.factor(aero.ken.sen.quant$pval <= 0.05)

#map quant.coef aod/year all obs 98th quant slopes
#get map for states to plot
states <- map_data("state")

#get selected states
states.ag <- subset(states, #region == "wyoming" | 
                     #region == "colorado" | 
                     #region == "montana" | 
                     region == "north dakota" |
                       region == "south dakota" |
                       region == "nebraska" |
                       region == "kansas" |
                       region == "oklahoma" |
                       #region == "arkansas" |
                       region == "missouri" |
                       region == "iowa" |
                       region == "minnesota")

states.oil <- subset(states, region == "north dakota" |
                     region == "montana" |
                     region == "wyoming" |
                     region == "colorado")

#map aeronet trends
aero.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill="green", size = 3, alpha = 0.5) +
  geom_polygon(data = states.oil, aes(x=long,y=lat, group = group), fill="orange", size = 3, alpha = 0.5)
aero.plot <- aero.plot +
  #size depends on the absolute value of the normalized slope, color on positive or negative, fill is the same, shape depends on statistical significance
  geom_point(data=aero.ken.sen.quant, aes(x=lon, 
                                          y = lat,
                                          size = abs(aero.ken.sen.quant$normslope),
                                          color = ifelse(aero.ken.sen.quant$normslope > 0,
                                                    "Increasing","Decreasing"),
                                          shape = ss,
                                          fill = ifelse(aero.ken.sen.quant$normslope > 0,
                                                   "Increasing","Decreasing")),
             group = FALSE) +
  #blue is decreasing, red is increasing
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
         shape = guide_legend(override.aes = list(fill = "black"))) +
  #now have to get rid of fill legend
  guides(fill = FALSE) +
  #set theme to simple black and white
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", var.string)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 

aero.plot

#assign sites to states
#create empty states column
aero.ken.sen.quant$state <- rep(NA,nrow(aero.ken.sen.quant))
for (i in 1:nrow(aero.ken.sen.quant)) {
  #i = 20
  if (as.character(aero.ken.sen.quant[i,1]) == "saturn_island" ) {
    aero.ken.sen.quant$state[i] = "BC"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "trinidad_head" |
             as.character(aero.ken.sen.quant[i,1]) == "monterey" |
             as.character(aero.ken.sen.quant[i,1]) == "fresno" |
             as.character(aero.ken.sen.quant[i,1]) == "ucsb") {
    aero.ken.sen.quant$state[i] = "CA"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "maricopa" |
             as.character(aero.ken.sen.quant[i,1]) == "tucson") {
    aero.ken.sen.quant$state[i] = "AZ"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "hjandrews") {
    aero.ken.sen.quant$state[i] = "OR"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "rimrock") {
    aero.ken.sen.quant$state[i] = "ID"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "bozeman" |
             as.character(aero.ken.sen.quant[i,1]) == "missoula") {
    aero.ken.sen.quant$state[i] = "MT"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "railroad_valley") {
    aero.ken.sen.quant$state[i] = "NV"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "sevilleta" |
             as.character(aero.ken.sen.quant[i,1]) == "white_sands_helstf") {
    aero.ken.sen.quant$state[i] = "NM"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "red_mountain_pass" |
             as.character(aero.ken.sen.quant[i,1]) == "bsrn_bao_boulder" | 
             as.character(aero.ken.sen.quant[i,1]) == "neon_cvalla" | 
             as.character(aero.ken.sen.quant[i,1]) == "table_mountain") {
    aero.ken.sen.quant$state[i] = "CO"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "cart_site") {
    aero.ken.sen.quant$state[i] = "OK"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "konza_edc") {
    aero.ken.sen.quant$state[i] = "KS"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "sioux_falls") {
    aero.ken.sen.quant$state[i] = "SD"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "ames") {
    aero.ken.sen.quant$state[i] = "IA"
  } else if (as.character(aero.ken.sen.quant[i,1]) == "univ_of_houston") {
    aero.ken.sen.quant$state[i] = "TX"
  }
}

#make another aeronet plot subsetted by states
aero.ken.sen.quant.states <- subset(aero.ken.sen.quant, state == "MT" |
                                         state == "WY" |
                                         state == "CO" |
                                         state == "ND" |
                                         state == "SD" |
                                         state == "NE" |
                                         state == "KS" |
                                         state == "OK" |
                                         state == "AR" |
                                         state == "MO" |
                                         state == "IA" |
                                         state == "MN")

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                       region == "colorado" | 
                       region == "montana" | 
                       region == "north dakota" |
                       region == "south dakota" |
                       region == "nebraska" |
                       region == "kansas" |
                       region == "oklahoma" |
                       region == "arkansas" |
                       region == "missouri" |
                       region == "iowa" |
                       region == "minnesota")

#map aeronet trends for subset region
aero.plot.sub <- ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-115.5, -90),ylim = c(33.25,49),ratio = 1.3)
aero.plot.sub <- aero.plot.sub +
  #size depends on the absolute value of the normalized slope, color on positive or negative, fill is the same, shape depends on statistical significance
  geom_point(data=aero.ken.sen.quant.states, aes(x=lon, 
                                          y = lat,
                                          size = abs(aero.ken.sen.quant.states$normslope),
                                          color = ifelse(aero.ken.sen.quant.states$normslope > 0,
                                                         "Increasing","Decreasing"),
                                          shape = ss,
                                          fill = ifelse(aero.ken.sen.quant.states$normslope > 0,
                                                        "Increasing","Decreasing")),
             group = FALSE) +
  #blue is decreasing, red is increasing
  scale_color_manual(values = c("Blue","Red")) +
  #1 is for open circle (not significant), 21 is filled circle (significant)
  scale_shape_manual(values = c(21), labels = c("< 0.05")) +
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
         shape = guide_legend(override.aes = list(fill = "black"))) +
  #now have to get rid of fill legend
  guides(fill = FALSE) +
  #set theme to simple black and white
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", var.string)) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), 
        legend.title = element_text(size=18), 
        title = element_text(size=20),
        legend.text = element_text(size = 16))

aero.plot.sub

rm(aero.quant,dat.quant,tmp.dat,tmp.df,tmp.high,tmp.ken,tmp.quant,tmp.site.quant.obs,tmp.ci,tmp.intercept,tmp.normci,
   tmp.normslope,tmp.p,tmp.slope,tmp.tau,)
#finished with AERONET portion







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

#next 6 lines useful for subsetting months or seasons
#create day of year vector frome date vector
#improve.dat$doy <- as.numeric(strftime(improve.dat$Date, format = "%j"))

#get obs only in summer fire season (day of year 170 to 220)
#improve.summer <- subset(improve.dat, doy >= 170 & doy <= 220)

#subset sites west of -100 degrees longitude
#improve.summer <- subset(improve.summer,Longitude <= -100)



ag.imp <- subset(improve.dat, State == "ND" |
                   State == "SD" |
                   State == "NE" |
                   State == "KS" |
                   State == "OK" |
                   State == "IA" |
                   State == "MO" |
                   State == "MN")

ag.sitename <- unique(ag.imp$SiteName)

ag.imp <- ag.imp[!is.na(ag.imp$coarse), ]

# ag.minmax.date <- ag.imp %>%
#   group_by(SiteName) %>%
#   summarize_at(vars(Date), funs(min(.,na.rm = T),max(.,na.rm = T)))

ag.minmax.date <- NULL
for (i in 1:length(ag.sitename)) {
  #i = 1
  tmp.dat <- subset(ag.imp, SiteName == ag.sitename[i])
  min.date <- min(tmp.dat$Date)
  max.date <- max(tmp.dat$Date)
  tmp.df <- data.frame("SiteName" = tmp.dat$SiteName[1],
                       "state" = tmp.dat$State[1],
                       "lat" = tmp.dat$Latitude[1],
                       "long" = tmp.dat$Longitude[1],
                       "mindate" = min.date,
                       "maxdate" = max.date
                       )
  ag.minmax.date <- rbind(ag.minmax.date,tmp.df)
}

ag.minmax.date$maxdate <- as.POSIXlt(ag.minmax.date$maxdate)
ag.minmax.date$mindate <- as.POSIXlt(ag.minmax.date$mindate)

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


#get quantile for each year at each site
improve.dat.quant <- NULL
for (i in 1:length(q)) {
  #i=1
  tmp.dat <- improve.dat %>%
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
    tmp.dat <- subset(improve.dat, year(Date) == improve.years[i] & SiteName == improve.sites[j])
    tmp.quant <- subset(improve.dat.quant, `year(Date)` == improve.years[i] & improve.dat.quant$SiteName == improve.sites[j])
    tmp.high <- subset(tmp.dat, get(improve.var.sel) >= tmp.quant[[q.ind+2]])
    improve.var.quant <- rbind(improve.var.quant,tmp.high)
    print(paste0('finished ', improve.years[i],' ',improve.sites[j]))
  }
}

#number of days in a year to be used for change/day to change/year later
year.day <- 365

#get vector of date as numeric (in days) to use kendallTrendTest function
improve.var.quant$Date_day <- as.numeric(improve.var.quant$Date)


# count number of non NA obs in quantile for each site
improve.summer.quant.obs <- improve.var.quant %>%
  group_by(year(Date), SiteName) %>%
  summarise_at(vars(improve.var.sel), ~sum(!is.nan(.)))

#calculate slope coefficient using kendall and sens slope analysis
improve.ken.sen.quant <- NULL
for (i in 1:length(improve.sites)) {
  #i = 1
  tmp.site.quant.obs <- subset(improve.var.quant, SiteName == improve.sites[i])
  #only run trend test if more than 10 observations exist within the quantile for site
  if (nrow(tmp.site.quant.obs) >= 10) {
    tmp.ken <- kendallTrendTest(get(improve.var.sel) ~ Date_day, tmp.site.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
    #convert from change/day to change/year
    tmp.slope <- tmp.ken$estimate[2] * year.day
    tmp.p <- tmp.ken$p.value
    tmp.tau <- tmp.ken$estimate[1]
    tmp.intercept <- tmp.ken$estimate[3]
    #convert from change/day to change/year
    tmp.ci <- tmp.ken$interval$limits *year.day
    #normalize slope and CIs by dividing by median of observations in quantile
    tmp.normslope <- as.numeric(tmp.slope)/median(tmp.site.quant.obs[[improve.var.sel]], na.rm = TRUE)
    tmp.normci <- tmp.ci/median(tmp.site.quant.obs[[improve.var.sel]], na.rm, na.rm = TRUE)
    #store all values in tmp.df
    tmp.df <- data.frame("site_name" = improve.sites[i], 
                         "network" = "IMPROVE",
                         "lat" = tmp.site.quant.obs$Latitude[1], 
                         "lon" = tmp.site.quant.obs$Longitude[1],
                         #"state" = tmp.site.quant.obs$State[1],
                         "slope" = tmp.slope,
                         #multiply by 100 to get percent change
                         "normslope" = tmp.normslope * 100,
                         "pval" = tmp.p,
                         "tau" = tmp.tau,
                         "intercept" = tmp.intercept,
                         "LCI" = tmp.ci[1],
                         "normLCI" = tmp.normci[1] * 100,
                         "UCI" = tmp.ci[2],
                         "normUCI" = tmp.normci[2] * 100,
                         "state" = tmp.site.quant.obs$State[1])
    # fill with NA values if less than 10 observations within quantile
  } else {
    tmp.df <- data.frame("site_name" = improve.sites[i], 
                         "network" = "IMPROVE",
                         "lat" = tmp.site.quant.obs$Latitude[1], 
                         "lon" = tmp.site.quant.obs$Longitude[1],
                         #"state" = tmp.site.quant.obs$State[1],
                         "slope" = NA,
                         "normslope" = NA,
                         "pval" = NA,
                         "tau" = NA,
                         "intercept" = NA,
                         "LCI" = NA,
                         "normLCI" = NA,
                         "UCI" = NA,
                         "normUCI" = NA,
                         "state" = tmp.site.quant.obs$State[1])
  }
  improve.ken.sen.quant <- rbind(improve.ken.sen.quant,tmp.df)
  print(paste0("finished ", improve.sites[i]))
}

rm(tmp.dat,tmp.df,tmp.high,tmp.ken,tmp.quant,tmp.site.quant.obs,tmp.ci,tmp.intercept,tmp.normci,tmp.normslope,tmp.p,tmp.slope,tmp.tau,i,j)

improve.ken.sen.quant$nonzero <- !(improve.ken.sen.quant$LCI <= 0 & improve.ken.sen.quant$UCI >= 0)

#check statistical significance
improve.ken.sen.quant$ss <- as.factor(improve.ken.sen.quant$pval <= 0.05)

network.ken.sen.quant <- NULL
network.ken.sen.quant <- rbind(aero.ken.sen.quant,improve.ken.sen.quant)

#remove rows with NA for slope
network.ken.sen.quant <- na.omit(network.ken.sen.quant)

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

#get maps
# canada = map_data("world","Canada")
# mexico = map_data("world","Mexico")

improve.ken.sen.quant <- na.omit(improve.ken.sen.quant)

for (i in 1:nrow(improve.ken.sen.quant)) {
  if (improve.ken.sen.quant$normslope[i] >= 0) {
    improve.ken.sen.quant$sign[i] <- "Increasing"
  } else {
    improve.ken.sen.quant$sign[i] <- "Decreasing"
  }
}

#first plot improve by itself, then aero and improve together
improve.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3)
  #geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill="green", size = 3, alpha = 0.5) +
  #geom_polygon(data = states.oil, aes(x=long,y=lat, group = group), fill="orange", size = 3, alpha = 0.5)
improve.plot <- improve.plot +
  geom_point(data=improve.ken.sen.quant, aes(x=lon, y = lat,
                                             size = abs(normslope),
                                             colour = sign,
                                             fill = sign,
                                             shape = ss),
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

improve.plot

#make another improve plot subsetted by states
improve.ken.sen.quant.states <- subset(improve.ken.sen.quant, state == "MT" |
                                         state == "WY" |
                                         state == "CO" |
                                         state == "ND" |
                                         state == "SD" |
                                         state == "NE" |
                                         state == "KS" |
                                         state == "OK" |
                                         state == "AR" |
                                         state == "MO" |
                                         state == "IA" |
                                         state == "MN")

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                      region == "colorado" | 
                      region == "montana" | 
                      region == "north dakota" |
                      region == "south dakota" |
                      region == "nebraska" |
                      region == "kansas" |
                      region == "oklahoma" |
                      region == "arkansas" |
                      region == "missouri" |
                      region == "iowa" |
                      region == "minnesota")

rm(improve.plot.sub)
#first plot improve by itself, then aero and improve together
improve.plot.sub <- ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-115.5, -90),ylim = c(33.25,49),ratio = 1.3)
improve.plot.sub <- improve.plot.sub +
  geom_point(data=improve.ken.sen.quant.states, aes(x=lon, y = lat,
                                             size = abs(normslope),
                                             colour = sign,
                                             fill = sign,
                                             shape = ss),
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
  #ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Coarse PM")) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), 
        legend.title = element_text(size=18), 
        title = element_text(size=20),
        legend.text = element_text(size = 16))
  #theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 

improve.plot.sub


#plot improve and aero together
all.plot <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3)
all.plot <- all.plot +
  geom_point(data=network.ken.sen.quant, aes(x=lon, y = lat,
                                             size = abs(normslope),
                                             colour = sign,
                                             fill = sign,
                                             shape = networksig),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_colour_manual(values = c("Blue","Red")) +
  scale_fill_manual(values = c("Blue","Red"), guide = FALSE) +
  scale_shape_manual(values = c(24,2,15,0), name = "Network & P-val") +
  scale_size_continuous(range = c(1,7)) +
  # geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  # geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #have to override aesthetics for legend to manually fill shape 21 for statistical significance
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black"), order = 1),         
         size = guide_legend(order = 2),
         color = guide_legend(order = 3)) +
  guides(fill = FALSE) +
  labs(size = "%/year", color = "Trend") +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Course AOD and PM")) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 

all.plot


#plot improve and aero together again but subset
all.plot.sub <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104, -92.5),ylim = c(36,43),ratio = 1.3)
all.plot.sub <- all.plot.sub +
  geom_point(data=subset(network.ken.sen.quant, lat >= 36 & lat <= 43 & lon >= -104 & lon <= -92.5), aes(x=lon, y = lat,
                                             size = abs(normslope),
                                             colour = sign,
                                             fill = sign,
                                             shape = networksig),
             group = FALSE) +
  #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_colour_manual(values = c("Blue","Red")) +
  scale_fill_manual(values = c("Blue","Red"), guide = FALSE) +
  scale_shape_manual(values = c(24,15,0), name = "Network & P-val") +
  scale_size_continuous(range = c(1,7)) +
  # geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  # geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #have to override aesthetics for legend to manually fill shape 21 for statistical significance
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black"), order = 1),         
         size = guide_legend(order = 2),
         color = guide_legend(order = 3)) +
  guides(fill = FALSE) +
  labs(size = "%/year", color = "Trend") +
  theme_bw() + 
  theme(legend.text = element_text(size=12)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Course AOD and PM")) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 

all.plot.sub



#get monthly trend and CIs for each site for aeronet

#create months numbered vector
months <- c(1:12)
#create empty df to store slope values
aero.all.months.sites.slope <- NULL
#create empty df to store monthly data
tmp.monthly <- NULL
#month names for plotting
monthnames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
for (l in 1:length(months)) {
  #l = 3
  tmp.month <- subset(dat.aero, month(Date) == months[l])
  
  #get quantile for each year at each site for the month
  tmp.quant <- NULL
  for (i in 1:length(q)) {
    tmp.dat <- tmp.month %>%
      group_by(year(Date), SiteName) %>%
      summarise_at(vars(var.sel), ~as.numeric(quantile(get(var.sel), na.rm = T, probs = q[i])))
    if (i == 1) {
      tmp.quant <- tmp.dat
      colnames(tmp.quant)[i+2] <- q[i]
    } else {
      tmp.quant[,i+2] <- tmp.dat[[var.sel]]
      colnames(tmp.quant)[i+2] <- q[i]
    }
  }
  
  
  # get obs only in chosen quantile
  tmp.quant.obs <- NULL
  for (j in 1:length(years)) {
    for (k in 1:length(sites)) {
      #i=18
      #j=5
      tmp.dat <- subset(tmp.month, year(Date) == years[j] & SiteName == sites[k])
      tmp.yearquant <- subset(tmp.quant, `year(Date)` == years[j] & tmp.quant$SiteName == sites[k])
      tmp.high <- subset(tmp.dat, get(var.sel) >= tmp.yearquant[[q.ind+2]])
      tmp.quant.obs <- rbind(tmp.quant.obs,tmp.high)
      print(paste0('finished ', years[j],' ',sites[k]))
    }
  }
  
  #conver date to number
  tmp.quant.obs$Date_sec <- as.numeric(tmp.quant.obs$Date)
  
  #create empty df to store slope info for all sites for each month
  tmp.all.sites <- NULL
  for (m in 1:length(sites)) {
    #m = 5
    tmp.monthly.site <- subset(tmp.quant.obs, SiteName == sites[m])
    # if there are 0 observations or less than 25 then do not calculate slope. If the data gap is larger than 3 and the number of years of data is less than 7 do not calculate slope
    if (nrow(tmp.monthly.site) == 0 | nrow(tmp.monthly.site) < 25 | (max(year(tmp.monthly.site$Date) - lag(year(tmp.monthly.site$Date)), na.rm = TRUE) >= 3 & length(unique(year(tmp.monthly.site$Date))) < 7)) {
      tmp.all.sites <- rbind(tmp.all.sites,data.frame("site_name" = sites[m],
                                                      "month" = monthnames[l],
                                                      "lat" = as.numeric(dat.aero[which(dat.aero$SiteName == sites[m])[1],ncol(dat.aero) - 3]), 
                                                      "lon" = as.numeric(dat.aero[which(dat.aero$SiteName == sites[m])[1],ncol(dat.aero) - 2]),
                                                      "normslope" = NA,
                                                      "normLCI" = NA,
                                                      "normUCI" = NA,
                                                      "pval" = NA,
                                                      "Date" = as.Date(paste(l,m,sep = "."), format = "%m.%d")))
      #run for loop and get sen's slope
    } else {
      tmp.ken <- kendallTrendTest(get(var.sel) ~ Date_sec, tmp.monthly.site, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.slope <- tmp.ken$estimate[2] * year.sec
      tmp.p <- tmp.ken$p.value
      tmp.tau <- tmp.ken$estimate[1]
      tmp.intercept <- tmp.ken$estimate[3]
      tmp.ci <- tmp.ken$interval$limits *year.sec
      tmp.normslope <- as.numeric(tmp.slope)/median(tmp.quant.obs[[var.sel]], na.rm = TRUE)
      tmp.normci <- tmp.ci/median(tmp.quant.obs[[var.sel]], na.rm, na.rm = TRUE)
      tmp.df <- data.frame("site_name" = sites[m], 
                           "month" = monthnames[l],
                           "lat" = tmp.monthly.site$Site_Latitude.Degrees.[1], 
                           "lon" = tmp.monthly.site$Site_Longitude.Degrees.[1], 
                           "normslope" = tmp.normslope * 100,
                           "normLCI" = tmp.normci[1] * 100,
                           "normUCI" = tmp.normci[2] * 100,
                           "pval" = tmp.p,
                           "Date" = as.Date(paste(l,m,sep = "."), format = "%m.%d"))
      tmp.all.sites <- rbind(tmp.all.sites,tmp.df)
    }
  }
  aero.all.months.sites.slope <- rbind(aero.all.months.sites.slope,tmp.all.sites)
}


aero.all.months.sites.slope$nonzero <- !(aero.all.months.sites.slope$normLCI <= 0 & aero.all.months.sites.slope$normUCI >= 0)

#check statistical significance
aero.all.months.sites.slope$ss <- as.factor(aero.all.months.sites.slope$pval <= 0.05)

#remove rows with NA for slope
aero.all.months.sites.slope <- na.omit(aero.all.months.sites.slope)

for (i in 1:nrow(aero.all.months.sites.slope)) {
  if (aero.all.months.sites.slope$pval[i] <= 0.05) {
    aero.all.months.sites.slope$sig[i] <- "< 0.05"
  } else {
    aero.all.months.sites.slope$sig[i] <- "> 0.05"
  }
}

for (i in 1:nrow(aero.all.months.sites.slope)) {
  if (aero.all.months.sites.slope$normslope[i] >= 0) {
    aero.all.months.sites.slope$sign[i] <- "Increasing"
  } else {
    aero.all.months.sites.slope$sign[i] <- "Decreasing"
  }
}



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
                                                                                                 colour = sign,
                                                                                                 fill = sign,
                                                                                                 shape = ss),
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE) +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05"), guide = FALSE, drop = FALSE) +
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


# do the same but for subset region
#make empty list to store plots
p.sub <- list()
#generate base plot to add data to
aero.monthly.sub <- subset(aero.all.months.sites.slope, lat >= 32.5 & lat <= 50 & lon >= -105 & lon <= -90)
aero.monthly.sub$sign <- as.factor(aero.monthly.sub$sign)
basemap <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-105, -90),ylim = c(32.5,49),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(aero.monthly.sub, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                      size = abs(normslope),
                                                                                      colour = sign,
                                                                                      fill = sign,
                                                                                      shape = ss),
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE, limits = levels(aero.monthly.sub$sign)) +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05"), guide = FALSE, drop = FALSE) +
                labs(size = "%/year", color = "Trend") +
                theme_bw() + 
                theme(legend.text = element_text(size=12)) +
                ggtitle(monthnames[i]) +
                theme(axis.text = element_text(size = 7), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=8)) + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()))
  
  p.sub[[i]] <- tmp.plot
  tmp.plot <- NULL
  print(paste0("finished ",monthnames[i]))
  graphics.off()
}



#find maximum slope to know shich legend to choose
maxslope <- aero.monthly.sub %>%
#maxslope <- aero.all.months.sites.slope %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

#function to pull out legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pull out legend
mylegend<-g_legend(p.sub[[which.max(maxslope$normslope)]])

#arrange all monthly plots into one figure, use mylegend as legend
grid.arrange(arrangeGrob(p.sub[[1]] + theme(legend.position = "none"),
                         p.sub[[2]] + theme(legend.position = "none"),
                         p.sub[[3]] + theme(legend.position = "none"),
                         p.sub[[4]] + theme(legend.position = "none"),
                         p.sub[[5]] + theme(legend.position = "none"),
                         p.sub[[6]] + theme(legend.position = "none"),
                         p.sub[[7]] + theme(legend.position = "none"),
                         p.sub[[8]] + theme(legend.position = "none"),
                         p.sub[[9]] + theme(legend.position = "none"),
                         p.sub[[10]] + theme(legend.position = "none"),
                         p.sub[[11]] + theme(legend.position = "none"),
                         p.sub[[12]] + theme(legend.position = "none")),
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


#get monthly maps for states subset
aero.all.months.sites.slope.statesub <- subset(aero.all.months.sites.slope, #state == "MT" |
                                                 #state == "WY" |
                                                 #state == "CO" |
                                                 state == "ND" |
                                                 state == "SD" |
                                                 state == "NE" |
                                                 state == "KS" |
                                                 state == "OK" |
                                                 #state == "AR" |
                                                 state == "MO" |
                                                 state == "IA" |
                                                 state == "MN")

#get selected states
states.sel <- subset(states, #region == "wyoming" | 
                       #region == "colorado" | 
                       #region == "montana" | 
                       region == "north dakota" |
                       region == "south dakota" |
                       region == "nebraska" |
                       region == "kansas" |
                       region == "oklahoma" |
                       #region == "arkansas" |
                       region == "missouri" |
                       region == "iowa" |
                       region == "minnesota")


#generate base plot to add data to
basemap.sub <- ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap.sub +
                geom_point(data=subset(aero.all.months.sites.slope.statesub, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                                 size = abs(normslope),
                                                                                                 colour = sign,
                                                                                                 fill = sign,
                                                                                                 shape = ss),
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE) +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05"), guide = FALSE, drop = FALSE) +
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
maxslope <- aero.all.months.sites.slope.statesub %>%
  #maxslope <- aero.all.months.sites.slope %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

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


#assign states to regions
GP <- c("ND", "SD" , "MN" , "KS" , "NE" , "IA" , "MO")
SW <- c("AZ" , "NM" , "UT" , "CO")
NW <- c("WA" , "OR" , "ID" , "MT" , "WY" , "BC")
W <- c("CA", "NV")
S <- c("TX","OK","LA","AR")
#create empty region column
aero.all.months.sites.slope$region <- rep(NA,nrow(aero.all.months.sites.slope))


for (i in 1:nrow(aero.all.months.sites.slope)) {
  if (sum(as.numeric(aero.all.months.sites.slope[i,14] == SW))==1) {
    aero.all.months.sites.slope$region[i] = "SW"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,14] == NW))==1) {
    aero.all.months.sites.slope$region[i] = "NW"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,14] == GP))==1) {
    aero.all.months.sites.slope$region[i] = "GP"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,14] == W))==1) {
    aero.all.months.sites.slope$region[i] = "W"
  } else if (sum(as.numeric(aero.all.months.sites.slope[i,14] == S))==1) {
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
  scale_color_manual(values = c("Dark Green","Dark Blue","Dark Red","Dark Orange","Blue"), name = "Region", guide = "legend") +
  xlab("Month") +
  ylab("%/Year") +
  #ggtitle(paste0(" Regional Monthly ", q.string, "th Quantile Normalized Trends - ", var.string)) +
  theme(
    plot.title = element_text(size=16, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) +
  geom_hline(yintercept = 0)
#end of aeronet monthly analysis






#do monthly analysis for improve
#create empty all.months.sites.slope df to store slope info for each month for each site
improve.all.months.sites.slope <- NULL
#create empty tmp.monthly df
tmp.monthly <- NULL
for (l in 1:length(months)) {
  #l = 2
  #subset dat.sub by month in for loop and store as tmp.month
  tmp.month <- subset(improve.dat, month(Date) == months[l])
  
  #get quantile for each year at each site. Enter desired quantile
  #create empty tmp.quant df to store quantile values
  tmp.quant <- NULL
  # run for loop through length of q (desired quantiles)
  for (i in 1:length(q)) {
    # pipe tmp.month into grouping by the year and site
    tmp.dat <- tmp.month %>%
      group_by(year(Date), SiteName) %>%
      #get the quantile for desired variable and remove NAs
      summarise_at(vars(improve.var.sel), ~as.numeric(quantile(get(improve.var.sel), na.rm = T, probs = q[i])))
    # if this is the first run in for loop, create new df
    if (i == 1) {
      tmp.quant <- tmp.dat
      #name the column with quantile
      colnames(tmp.quant)[i+2] <- q[i]
      #if not the first run, add columns onto tmp.quant df
    } else {
      tmp.quant[,i+2] <- tmp.dat[[improve.var.sel]]
      #name column with quantile
      colnames(tmp.quant)[i+2] <- q[i]
    }
  }
  
  
  # get obs only in chosen quantile
  #create empty tmp.quant.obs df to store observations within quantile
  tmp.quant.obs <- NULL
  # run through each year for each site
  for (j in 1:length(improve.years)) {
    for (k in 1:length(improve.sites)) {
      #k=1
      #j=1
      #store data from one year from one site at a time in tmp.dat
      tmp.dat <- subset(tmp.month, year(Date) == improve.years[j] & SiteName == improve.sites[k])
      #pull out quantile value for that year and that site
      tmp.yearquant <- subset(tmp.quant, `year(Date)` == improve.years[j] & tmp.quant$SiteName == improve.sites[k])
      #pull out only data within that quantile (greater than quantile value)
      tmp.high <- subset(tmp.dat, get(improve.var.sel) >= tmp.yearquant[[q.ind+2]])
      #add temporary data from each site and each year to tmp.quant.obs df
      tmp.quant.obs <- rbind(tmp.quant.obs,tmp.high)
      print(paste0('finished ', improve.years[j],' ',improve.sites[k]))
    }
  }
  
  #convert Date vector from date format to seconds in order to perform trend analysis
  tmp.quant.obs$Date_sec <- as.numeric(tmp.quant.obs$Date)
  
  #create empty df to store slope info for month for this run of for loop
  tmp.all.sites <- NULL
  for (m in 1:length(improve.sites)) {
    #m = 5
    #subset by one site at a time
    tmp.monthly.site <- subset(tmp.quant.obs, SiteName == improve.sites[m])
    # if number of observations for this site is less than 25, or if the gap in years of data is larger than 2 and data covers less than 7 years, do not calculate a slope for this site
    if (nrow(tmp.monthly.site) == 0 | nrow(tmp.monthly.site) < 15 | (max(year(tmp.monthly.site$Date) - lag(year(tmp.monthly.site$Date)), na.rm = TRUE) >= 3 & length(unique(year(tmp.monthly.site$Date))) < 7)) {
      #add site info to tmp.all.sites df
      tmp.all.sites <- rbind(tmp.all.sites,data.frame("site_name" = improve.sites[m],
                                                      "month" = monthnames[l],
                                                      "lat" = as.numeric(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"Latitude"]), 
                                                      "lon" = as.numeric(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"Longitude"]),
                                                      "state" = as.character(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"State"]),
                                                      "normslope" = NA,
                                                      "normLCI" = NA,
                                                      "normUCI" = NA,
                                                      "pval" = NA,
                                                      "Date" = as.Date(paste(l,1,sep = "."), format = "%m.%d")))
    } else {
      #run kendall trend test for desired variable to get slope, and 95% confidence intervals for slope
      tmp.ken <- kendallTrendTest(get(improve.var.sel) ~ Date_sec, tmp.monthly.site, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      #multiply by number of days in the year to get change per year
      tmp.slope <- tmp.ken$estimate[2] * year.day
      #store pvalue
      tmp.p <- tmp.ken$p.value
      #store kendalls tau
      tmp.tau <- tmp.ken$estimate[1]
      #store y intercept
      tmp.intercept <- tmp.ken$estimate[3]
      #myltiply cis by days per year to get change per year
      tmp.ci <- tmp.ken$interval$limits *year.day
      #normalize the clope and cis by dividing by the median value within the quantile for the site
      tmp.normslope <- as.numeric(tmp.slope)/median(tmp.quant.obs[[improve.var.sel]], na.rm = TRUE)
      tmp.normci <- tmp.ci/median(tmp.quant.obs[[improve.var.sel]], na.rm, na.rm = TRUE)
      #add site info to tmp.all.sites df
      tmp.df <- data.frame("site_name" = improve.sites[m], 
                           "month" = monthnames[l],
                           "lat" = as.numeric(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"Latitude"]), 
                           "lon" = as.numeric(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"Longitude"]),
                           "state" = as.character(improve.dat[which(improve.dat$SiteName == improve.sites[m])[1],"State"]),
                           "normslope" = tmp.normslope * 100,
                           "normLCI" = tmp.normci[1] * 100,
                           "normUCI" = tmp.normci[2] * 100,
                           "pval" = tmp.p,
                           "Date" = as.Date(paste(l,1,sep = "."), format = "%m.%d"))
      tmp.all.sites <- rbind(tmp.all.sites,tmp.df)
    }
  }
  #add slope info from tmp.all.sites for each run (month) to all.months.sites.slope df
  improve.all.months.sites.slope <- rbind(improve.all.months.sites.slope,tmp.all.sites)
}

#get regular rownumbers for df
rownames(improve.all.months.sites.slope) <- 1:nrow(improve.all.months.sites.slope)

#reorder improve columns to match aeronet
improve.all.months.sites.slope <- improve.all.months.sites.slope[,c(1:4,6:10,5)]

improve.all.months.sites.slope$nonzero <- !(improve.all.months.sites.slope$normLCI <= 0 & improve.all.months.sites.slope$normUCI >= 0)

#check statistical significance
improve.all.months.sites.slope$ss <- as.factor(improve.all.months.sites.slope$pval <= 0.05)

#remove rows with NA for slope
improve.all.months.sites.slope <- na.omit(improve.all.months.sites.slope)

for (i in 1:nrow(improve.all.months.sites.slope)) {
  if (improve.all.months.sites.slope$pval[i] <= 0.05) {
    improve.all.months.sites.slope$sig[i] <- "< 0.05"
  } else {
    improve.all.months.sites.slope$sig[i] <- "> 0.05"
  }
}

for (i in 1:nrow(improve.all.months.sites.slope)) {
  if (improve.all.months.sites.slope$normslope[i] >= 0) {
    improve.all.months.sites.slope$sign[i] <- "Increasing"
  } else {
    improve.all.months.sites.slope$sign[i] <- "Decreasing"
  }
}

#create improve monthly plots
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
                geom_point(data=subset(improve.all.months.sites.slope, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                                 size = abs(normslope),
                                                                                                 colour = sign,
                                                                                                 fill = sign,
                                                                                                 shape = ss),
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE) +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05"), guide = FALSE, drop = FALSE) +
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


# do the same but for subset region
#make empty list to store plots
p.sub <- list()
#generate base plot to add data to
improve.monthly.sub <- subset(improve.all.months.sites.slope, lat >= 32.5 & lat <= 50 & lon >= -105 & lon <= -90)
improve.monthly.sub$sign <- as.factor(improve.monthly.sub$sign)
basemap <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-105, -90),ylim = c(32.5,49),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(improve.monthly.sub, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                      size = abs(normslope),
                                                                                      colour = sign,
                                                                                      fill = sign,
                                                                                      shape = ss),
                           group = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE, limits = levels(improve.monthly.sub$sign)) +
                scale_fill_manual(values = c("Blue","Red","White"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05"), guide = FALSE, drop = FALSE) +
                labs(size = "%/year", color = "Trend") +
                theme_bw() + 
                theme(legend.text = element_text(size=12)) +
                ggtitle(monthnames[i]) +
                theme(axis.text = element_text(size = 7), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=8)) + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()))
  
  p.sub[[i]] <- tmp.plot
  tmp.plot <- NULL
  print(paste0("finished ",monthnames[i]))
  graphics.off()
}



#find maximum slope to know shich legend to choose
maxslope <- improve.all.months.sites.slope %>%
  #maxslope <- aero.all.months.sites.slope %>%
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
             top = textGrob(paste0(q.string, "th Quantile Trends - Coarse PM"),gp=gpar(fontsize = 12, font = 3)))








#create empty region column
improve.all.months.sites.slope$region <- rep(NA,nrow(improve.all.months.sites.slope))


for (i in 1:nrow(improve.all.months.sites.slope)) {
  if (sum(as.numeric(improve.all.months.sites.slope[i,10] == SW))==1) {
    improve.all.months.sites.slope$region[i] = "SW"
  } else if (sum(as.numeric(improve.all.months.sites.slope[i,10] == NW))==1) {
    improve.all.months.sites.slope$region[i] = "NW"
  } else if (sum(as.numeric(improve.all.months.sites.slope[i,10] == MW))==1) {
    improve.all.months.sites.slope$region[i] = "MW"
  } else if (sum(as.numeric(improve.all.months.sites.slope[i,10] == W))==1) {
    improve.all.months.sites.slope$region[i] = "W"
  } else if (sum(as.numeric(improve.all.months.sites.slope[i,10] == S))==1) {
    improve.all.months.sites.slope$region[i] = "S"
  }
}

# #add columns for significance, sign of trend
# aero.all.months.sites.slope$sig <- aero.all.months.sites.slope$pval <= 0.05
# aero.all.months.sites.slope$sign <- aero.all.months.sites.slope$normslope > 0
# #get rid of rows with NA slopes
# aero.all.months.sites.slope <- na.omit(aero.all.months.sites.slope)
# #create network column
aero.all.months.sites.slope$network <- "aero"
# #add columns for signifcan, sign of trend, omit NA rows, add improve strings to network column
# improve.all.months.sites.slope$sig <- improve.all.months.sites.slope$pval <= 0.05
# improve.all.months.sites.slope <- na.omit(improve.all.months.sites.slope)
# improve.all.months.sites.slope$sign <- improve.all.months.sites.slope$normslope > 0
improve.all.months.sites.slope$network <- "improve"


#rearrange aero columns to match improve
aero.all.months.sites.slope.rearrange <- subset(aero.all.months.sites.slope, select = c(1:9,14,10:13,15))

#bind aeronet and improve
#aero.all.months.sites.slope <- aero.all.months.sites.slope[,c(1:9,15,10:14)]
#aero.all.months.sites.slope <- aero.all.months.sites.slope[,c(1:9,15,11:14,10)]
network.all.months.sites.slope <- rbind(aero.all.months.sites.slope.rearrange,improve.all.months.sites.slope)#[,-c(10)])

#create a string showing network and significance
network.all.months.sites.slope$netsig <- paste0(network.all.months.sites.slope$network," ",network.all.months.sites.slope$sig)
network.all.months.sites.slope$sign <- as.factor(network.all.months.sites.slope$sign)

#make empty list to store plots
p <- list()
#generate base plot to add data to
basemap <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(network.all.months.sites.slope, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                                    size = abs(normslope),
                                                                                                    color = sign,
                                                                                                    fill = sign,
                                                                                                    #color = ifelse(normslope > 0,"Increasing","Decreasing"),
                                                                                                    shape = netsig),
                                                                                                    #shape = ifelse(pval < 0.05,"p-val < 0.05","p-val > 0.05")),
                           #shape = 21,
                           group = FALSE) +
                           #show.legend = FALSE) +
                scale_color_manual(values = c("Blue","Red"), guide = "legend", drop = FALSE, 
                                   limits = levels(network.all.months.sites.slope$sign)) +
                scale_shape_manual(values = c(24,2,15,0), labels = c("AERO < 0.05", "AERO > 0.05", "IMPROVE < 0.05","IMPROVE > 0.05"), guide = "legend", drop = FALSE) +
                scale_fill_manual(values = c("Blue","Red"), guide = FALSE, drop = FALSE) +
                scale_size_identity(trans = "sqrt", guide = "legend") +
                theme(legend.text = element_text(size=12)) +
                labs(shape = "Network + P-val", size = "%/year", color = "Trend") +
                guides(fill = guide_legend(override.aes = list(shape = 21)),
                       shape = guide_legend(override.aes = list(fill = "black"), order = 1),         
                       size = guide_legend(order = 2),
                       color = guide_legend(order = 3)) +
                guides(fill = FALSE) +
                theme_bw() + 
                theme(axis.text = element_text(size = 7), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=8)) + 
                ggtitle(monthnames[i]) +
                theme(legend.text = element_text(size=12)) +
                  xlab("Longitude") +
                  ylab("Latitude") +
                  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))) +
                theme(axis.title.x = element_blank(),
                axis.title.y = element_blank())
  
  p[[i]] <- tmp.plot
  tmp.plot <- NULL
  print(paste0("finished ",monthnames[i]))
  graphics.off()
}


# ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
#   coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
#   geom_point(data=subset(network.all.months.sites.slope, month == monthnames[3]), aes(x=lon, y = lat,
#                                              size = abs(normslope),
#                                              colour = sign,
#                                              #colour = ifelse(normslope > 0,"Increasing","Decreasing"),
#                                              fill = sign,
#                                              shape = netsig),
#              #shape = 24,
#              #fill = "transparent",
#              #alpha = ken.sen.quant$pval <= 0.1),#, no = ">",yes = "<")),
#              # alpha = quant.coef$p.value),#, no = ">",yes = "<")),
#              #fill = quant.coef$p.value),
#              group = FALSE) +
#   #theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
#   scale_colour_manual(values = c("Blue","Red"), guide = "legend") +
#   scale_fill_manual(values = c("Blue","Red"), guide = FALSE) +
#   scale_shape_manual(values = c("aero FALSE" = 2,"aero TRUE" = 24, "improve FALSE" = 0, "improve TRUE" = 15), guide = "legend") +
#   scale_size_continuous(range = c(1,7), guide = "legend") +
#   #scale_alpha_manual(values = c(.0001, .001), guide = FALSE) +
#   #plot on values less than pvalue threshold
#   #scale_alpha_manual(values = c(0,1), guide = FALSE) +
#   #scale_alpha_continuous(trans = "reverse") +
#   #scale_alpha_continuous(breaks=c(.0001, .001, 0.01, 0.05,0.3, 1),trans = "reverse") +
#   #scale_alpha_continuous(trans = "reverse") +
#   labs(size = "%/year", color = "Trend", alpha = "P-val") +
#   theme_bw() + 
#   theme(legend.text = element_text(size=12)) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   #ggtitle(paste0("Trends in ", q.string, "th Quantile ", "Course AOD and PM")) +
#   #ggtitle(paste0("Trends in ", q.string, "th Quantile ", aero.var.string)) +
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
#   theme(plot.margin=unit(c(0,0,0,0.5),"cm"))


#get monthly maps for states subset
network.all.months.sites.slope.statesub <- subset(network.all.months.sites.slope, #state == "MT" |
                                                 #state == "WY" |
                                                 #state == "CO" |
                                                 state == "ND" |
                                                 state == "SD" |
                                                 state == "NE" |
                                                 state == "KS" |
                                                 state == "OK" |
                                                 #state == "AR" |
                                                 state == "MO" |
                                                 state == "IA" |
                                                 state == "MN")

#get selected states
states.sel <- subset(states, #region == "wyoming" | 
                       #region == "colorado" | 
                       #region == "montana" | 
                       region == "north dakota" |
                       region == "south dakota" |
                       region == "nebraska" |
                       region == "kansas" |
                       region == "oklahoma" |
                       #region == "arkansas" |
                       region == "missouri" |
                       region == "iowa" |
                       region == "minnesota")

#subset region to plot
#make empty list to store plots
p.sub <- list()
#subset network slopes
# network.monthly.sub <- subset(network.all.months.sites.slope, lat >= 36 & lat <= 43 & lon >= -104 & lon <= -92.5)
# network.monthly.sub$sign <- as.factor(network.monthly.sub$sign)
# network.monthly.sub$netsig <- as.factor(network.monthly.sub$netsig)
#generate base plot to add data to
basemap <- ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(network.all.months.sites.slope.statesub, month == monthnames[i]), aes(x=lon, y = lat,
                                                                                                    size = abs(normslope),
                                                                                                    color = sign,
                                                                                                    fill = sign,
                                                                                                    #color = ifelse(normslope > 0,"Increasing","Decreasing"),
                                                                                                    shape = netsig),
                           #shape = ifelse(pval < 0.05,"p-val < 0.05","p-val > 0.05")),
                           #shape = 21,
                           group = FALSE) +
                #show.legend = FALSE) +
                scale_color_manual(values = c("Blue","Red"), 
                                   guide = "legend", 
                                   drop = FALSE, 
                                   limits = levels(network.all.months.sites.slope.statesub$sign)) +
                scale_shape_manual(values = c(24,2,15,0), 
                                   labels = c("AERO < 0.05", "AERO > 0.05", "IMPROVE < 0.05","IMPROVE > 0.05"), 
                                   guide = "legend", 
                                   drop = FALSE, 
                                   limits = levels(as.factor(network.all.months.sites.slope.statesub$netsig))) +
                scale_fill_manual(values = c("Blue","Red"), guide = FALSE, drop = FALSE) +
                scale_size_continuous(range = c(3,9), guide = "legend", breaks = c(5,10,15,20), limits = c(0,25)) +
                #scale_size_identity(trans = "sqrt", range = c(1,7), guide = "legend", breaks = c(5,10,15,20)) +
                theme(legend.text = element_text(size=12)) +
                labs(shape = "Network + P-val", size = "%/year", color = "Trend") +
                guides(fill = guide_legend(override.aes = list(shape = 21)),
                       shape = guide_legend(override.aes = list(fill = "black"), order = 1),         
                       size = guide_legend(order = 2),
                       color = guide_legend(order = 3)) +
                guides(fill = FALSE) +
                theme_bw() + 
                theme(axis.text = element_text(size = 10), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=8)) + 
                ggtitle(monthnames[i]) +
                theme(legend.text = element_text(size=12)) +
                xlab("Longitude") +
                ylab("Latitude") +
                theme(plot.margin=unit(c(0,0,0,0.5),"cm"))) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  p.sub[[i]] <- tmp.plot
  tmp.plot <- NULL
  print(paste0("finished ",monthnames[i]))
  graphics.off()
}





#find maximum slope to know which legend to choose
maxslope <- network.all.months.sites.slope %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

#pull out legend
mylegend<-g_legend(p[[which.max(maxslope$normslope)]])

#plot all plots in one figure
#do.call(grid.arrange,p)

# grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
#              top = textGrob("98th Quantile Trends - PM2.5",gp=gpar(fontsize=12,font=3)))

grid.arrange(arrangeGrob(p.sub[[1]] + theme(legend.position = "none"),
                         p.sub[[2]] + theme(legend.position = "none"),
                         p.sub[[3]] + theme(legend.position = "none"),
                         p.sub[[4]] + theme(legend.position = "none"),
                         p.sub[[5]] + theme(legend.position = "none"),
                         p.sub[[6]] + theme(legend.position = "none"),
                         p.sub[[7]] + theme(legend.position = "none"),
                         p.sub[[8]] + theme(legend.position = "none"),
                         p.sub[[9]] + theme(legend.position = "none"),
                         p.sub[[10]] + theme(legend.position = "none"),
                         p.sub[[11]] + theme(legend.position = "none"),
                         p.sub[[12]] + theme(legend.position = "none")),
             mylegend, widths = c(10,1.5),
             top = textGrob(paste0(q.string, "th Quantile Trends - ", improve.var.string, " PM + AOD"),gp=gpar(fontsize = 12, font = 3)))

MW <- c("ND", "SD" , "MN" , "KS" , "NE" , "IA" , "MO")
SW <- c("AZ" , "NM" , "UT" , "CO")
NW <- c("WA" , "OR" , "ID" , "MT" , "WY")
W <- c("CA", "NV")
S <- c("TX","OK","LA","AR")
improve.all.months.sites.slope$state <- as.character(improve.all.months.sites.slope$state)
improve.all.months.sites.slope$region <- rep(NA,nrow(improve.all.months.sites.slope))

for (i in 1:nrow(improve.all.months.sites.slope)) {
  if (sum(as.numeric(improve.all.months.sites.slope$state[i] == SW))==1) {
    improve.all.months.sites.slope$region[i] = "SW"
  } else if (sum(as.numeric(improve.all.months.sites.slope$state[i] == NW))==1) {
    improve.all.months.sites.slope$region[i] = "NW"
  } else if (sum(as.numeric(improve.all.months.sites.slope$state[i] == MW))==1) {
    improve.all.months.sites.slope$region[i] = "MW"
  } else if (sum(as.numeric(improve.all.months.sites.slope$state[i] == W))==1) {
    improve.all.months.sites.slope$region[i] = "W"
  } else if (sum(as.numeric(improve.all.months.sites.slope$state[i] == S))==1) {
    improve.all.months.sites.slope$region[i] = "S"
  }
}

#all.months.sites.slope$Date <- as.Date(paste(all.months.sites.slope$month))

#plot
# ggplot(data = all.months.sites.slope, aes(Date, normslope, col = region)) +
#   geom_jitter(position = position_jitter(width = 0.5)) +
#   theme_bw()
# 
# ggplot(data = all.months.sites.slope, aes(Date, normslope, col = region)) +
#   geom_point() + 
#   scale_x_date(breaks = "1 month", date_labels = "%B") +
#   theme_bw()

region.trend <- improve.all.months.sites.slope %>%
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
  #ggtitle(paste0(" Regional Monthly ", q.string, "th Quantile Normalized Trends - ", improve.var.string, "PM")) +
  theme(
    plot.title = element_text(size=16, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  )






#analyze modis data
setwd("/home/andylambert/MODIS_tif")

#rast <- raster("MODIS_tif/2000/MOD08_D3.A2000.061.2017276160246_ocean_land_aod_mean.tif")
rgb <- raster::brick("2000/MOD08_D3.A2000170.061.2017276022644_land_aod_mean_3bands.tif")

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

#get maps
states <- map_data("state")
canada = map_data("world","Canada")
mexico = map_data("world","Mexico")



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

#subsetting dates for modis data
#change yeardoy to Date format
modis.date <- as.Date(as.numeric(substrRight(modis.yeardoy,5))-1,origin = paste0(substr(modis.yeardoy,start = 1, stop = 4),"-01-01"))
# min.i <- which(modis.date == "2008-01-01")
# max.i <- which(modis.date == "2018-12-31")
# modis.file.path <- modis.file.path[min.i:max.i]
# modis.yeardoy <- modis.yeardoy[min.i:max.i]


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
#ut.map <- subset(states, region == "utah")

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

#get ag states by themselves
us <- getData("GADM", country="USA", level=1)
agstates = us[match(toupper(c("North Dakota","South Dakota","Nebraska","Kansas","Oklahoma","Missouri","Iowa","Minnesota")),toupper(us$NAME_1)),]
spdf <- SpatialPointsDataFrame(coords = modis.us.sub[, c("lon", "lat")], data = modis.us.sub,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# spdf <- SpatialPointsDataFrame(coords = modis.us.sub[, c("lon", "lat")], data = modis.us.sub.allobs,
#                                proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
modis.ut <- spdf[!is.na(over(spdf, as(agstates, "SpatialPolygons"))), ]
modis.ut <- as.data.frame(modis.ut)

modis.plot <- ggplot(data = modis.us.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.us.sub[!is.na(modis.us.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, limits = c(-1,1), colours = c("blue","red")) +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent", limits = c(-10,10)) +#, oob = squish) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  #geom_polygon(data = ut.map, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
  #ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot

#subset and plot region
modis.mw.sub <- subset(modis.us.sub, lat >= 40 & lat <= 50 & lon >= -113 & lon <= -97)

modis.plot.sub <- ggplot(data = modis.mw.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.mw.sub[!is.na(modis.mw.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-113, -97),ylim = c(40,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0(q.string,"th Quantile Trends - MODIS AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot.sub


#get lakes shapefile for GSL
#lakes <- readOGR("/home/andylambert/ne_10m_lakes/ne_10m_lakes.shp")
# get GSL separate from lakes shapefile
#plot(lakes[which(lakes$name == "Great Salt Lake"),])
#gsl <-lakes[which(lakes$name == "Great Salt Lake"),]
geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3)
modis.ut.sub <- ggplot(data = modis.ut, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.ut[!is.na(modis.ut$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  #geom_polygon(data = ut, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  #geom_polygon(data = gsl, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.8) +
  coord_fixed(xlim = c(-104, -89),ylim = c(34,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#  ggtitle(paste0(q.string,"th Quantile Trends - MODIS AOD (550 nm) 2000-2018 for Dust Events"))

modis.ut.sub

modis.ut.ss <- subset(modis.ut, ss == "TRUE")

modis.ag <- ggplot(data = modis.ut, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.ut[!is.na(modis.ut$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, limits = c(-1,1), colours = c("blue","red")) +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +#, limits = c(-10,10)) +#, oob = squish) +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  #geom_polygon(data = ut.map, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  coord_fixed(xlim = c(-104, -89),ylim = c(34,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#ggtitle(paste0("Trends in ", q.string,"th Quantile MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.ag


# modis.ut.sub <- ggplot(data = modis.ut.ss, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
#   #scale_fill_manual(values = setNames(colors, levs))
#   scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
#   theme_bw() + 
#   scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
#   geom_polygon(data = ut, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
#   geom_polygon(data = gsl, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.8) +
#   coord_fixed(xlim = c(-114, -109),ylim = c(42,37),ratio = 1.3) +
#   labs(fill = "%/year" )+
#   xlab("Longitude") +
#   ylab("Latitude") + 
#   theme(legend.text = element_text(size=12)) +
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
#   theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#   #  ggtitle(paste0(q.string,"th Quantile Trends - MODIS AOD (550 nm) 2000-2018 for Dust Events"))
#   
#   modis.ut.sub

#get average aod for 90th quantile and all observations for utah
modis.ave <- NULL
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
        tmp.quant <- quantile(tmp.dat.year, probs = 0, na.rm = TRUE)
        tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
        tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
        tmp.df <- data.frame("date" = tmp.date, "aod_quant" = tmp.quant.obs)
        point.quant.obs <- rbind(point.quant.obs, tmp.df)
        point.quant.obs <- point.quant.obs[complete.cases(point.quant.obs), ]
      }
      tmp.ave <- mean(point.quant.obs$aod_quant, na.rm = T)
      tmp.df <- data.frame("lat" = modis.lat[j],
                           "lon" = modis.lon[k],
                           "ave" = tmp.ave)
      modis.ave <- rbind(modis.ave,tmp.df)
      print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
    }
  }

spdf <- SpatialPointsDataFrame(coords = modis.ave[, c("lon", "lat")], data = modis.ave,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
modis.ut.ave <- spdf[!is.na(over(spdf, as(ut, "SpatialPolygons"))), ]
modis.ut.ave <- as.data.frame(modis.ut.ave)

ggplot(data = modis.ut.ave, aes(x = lon, y = lat, fill = ave)) + geom_tile() +
  #scale_fill_manual(values = setNames(colors, levs))
  #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  scale_fill_gradient(low = "gray81", high = "red") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = ut, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  geom_polygon(data = gsl, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.8) +
  coord_fixed(xlim = c(-114, -109),ylim = c(42,37),ratio = 1.3) +
  labs(fill = "AOD" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm"))
#  ggtitle(paste0(q.string,"th Quantile Trends - MODIS AOD (550 nm) 2000-2018 for Dust Events"))









#get background trend for all modis observations
#get observations in desired quantile for each gridpoint
modis.trend.df.allobs <- data.frame(modis.lonlat)
colnames(modis.trend.df.allobs) <- c("lon","lat")
modis.trend.df.allobs[,"slope"] <- NA
modis.trend.df.allobs[,"pval"] <- NA
modis.trend.df.allobs[,"normslope"] <- NA
#get quantile for each year at each gridpoint and trends in quantile
for (j in 1:length(modis.lat)) {
  for (k in 1:length(modis.lon)) {
    #j = 10
    #k = 15
      point.obs <- dust.green[j,k,]
      tmp.ken <- kendallTrendTest(point.obs ~ modis.date, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
      tmp.ken.68 <- kendallTrendTest(point.obs ~ modis.date, na.action = na.pass, ci.slope = TRUE, conf.level = 0.68)
      tmp.95.ci <- tmp.ken$interval$limits * 365
      tmp.68.ci <- tmp.ken.68$interval$limits *365
      tmp.norm.95.ci <- (tmp.95.ci/median(point.obs, na.rm = TRUE))*100
      tmp.norm.68.ci <- (tmp.68.ci/median(point.obs, na.rm = TRUE))*100
      tmp.slope <- tmp.ken$estimate[2] * 365
      tmp.p <- tmp.ken$p.value
      aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
      modis.trend.df.allobs[aod.ind,3] <- tmp.slope
      modis.trend.df.allobs[aod.ind,4] <- tmp.p
      modis.trend.df.allobs[aod.ind,5] <- (tmp.slope/median(point.obs, na.rm = TRUE))*100
      modis.trend.df.allobs[aod.ind,6] <- tmp.norm.68.ci[1]
      modis.trend.df.allobs[aod.ind,7] <- tmp.norm.68.ci[2]
      modis.trend.df.allobs[aod.ind,8] <- tmp.norm.95.ci[1]
      modis.trend.df.allobs[aod.ind,9] <- tmp.norm.95.ci[2]
    # } else {
    #   aod.ind <- which(modis.trend.df$lon == modis.lon[k] & modis.trend.df$lat == modis.lat[j])
    #   modis.trend.df[aod.ind,3] <- NA
    #   modis.trend.df[aod.ind,4] <- NA
    #   modis.trend.df[aod.ind,5] <- NA
    # }
    print(paste0('finished ', modis.lon[k]," ",modis.lat[j]))
  }
}

colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V6"] <- "l68"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V7"] <- "h68"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V8"] <- "l95"
colnames(modis.trend.df.allobs)[colnames(modis.trend.df.allobs)=="V9"] <- "h95"


test.sub <- modis.trend.df.allobs[697,]
limy <- max(abs(test.sub[8:9]))

ggplot(data = data.frame(x = c(0,19), y = c(limy*-19,limy*19))) +
  geom_blank() +
  geom_ribbon(aes(x=c(0,19), ymin = test.sub$l68*x, ymax = test.sub$h68*x), fill = "lightgrey", alpha = 0.8) +
  geom_ribbon(aes(x=c(0,19), ymin = test.sub$l95*x, ymax = test.sub$h95*x), fill = "lightgrey", alpha = 0.4) +
  geom_abline(slope = test.sub$l68, intercept = 0) +
  geom_abline(slope = test.sub$h68, intercept = 0) +
  geom_abline(slope = test.sub$l95, intercept = 0) +
  geom_abline(slope = test.sub$h95, intercept = 0) +
  geom_abline(slope = test.sub$normslope, intercept = 0, color = ifelse(test.sub$normslope >= 0, "red" , "blue")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(0,19), breaks = c(0,6,11,16), expand = c(0,0), labels = c(2000,2005,2010,2015)) +
  ylim(limy*-19,limy*19) +
  xlab("Date") +
  ylab("Change (%)") +
  ggtitle("40.5, -108.5") +
  theme(plot.title = element_text(size=6))
  

#get only tiles in US
# Add an NA row between each state
tmp=lapply(split(states[,c("lat","long","region")], states$region), function(x) {
  bind_rows(list(x, data.frame(region=NA, lat=NA, long=NA)))
})
tmp = bind_rows(tmp)

point.filter = in.out(as.matrix(tmp[, c("lat","long")]), 
                      as.matrix(data.frame("lat" = modis.trend.df$lat,"long" = modis.trend.df$lon)))

#filter to get only us tiles
modis.us.sub.allobs <- modis.trend.df.allobs[point.filter,]

#make ss vector for statistical significane
modis.us.sub.allobs$ss <- modis.us.sub.allobs$pval <= 0.05
modis.us.sub.allobs$ss[!modis.us.sub.allobs$ss] <- NA

#get utah by itself
us <- getData("GADM", country="USA", level=1)
ut = us[match(toupper("Utah"),toupper(us$NAME_1)),]
spdf.allobs <- SpatialPointsDataFrame(coords = modis.us.sub.allobs[, c("lon", "lat")], data = modis.us.sub.allobs,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
modis.ut.allobs <- spdf.allobs[!is.na(over(spdf.allobs, as(ut, "SpatialPolygons"))), ]
modis.ut.allobs <- as.data.frame(modis.ut.allobs)


modis.plot <- ggplot(data = modis.ut.allobs, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.ut.allobs[!is.na(modis.ut.allobs$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = ut.map, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-114,-109), ylim = c(37,42), ratio = 1.3) +
  #coord_fixed(xlim = c(-128, -89),ylim = c(27,50),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot



#subset and plot region
modis.mw.sub <- subset(modis.us.sub.allobs, lat >= 40 & lat <= 50 & lon >= -113 & lon <= -97)

modis.plot.sub <- ggplot(data = modis.mw.sub, aes(x = lon, y = lat, fill = normslope, colour = ss)) + geom_tile() +
  geom_tile(data = modis.mw.sub[!is.na(modis.mw.sub$ss), ], aes(color = ss), size = 0.25) + 
  #scale_fill_manual(values = setNames(colors, levs))
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "transparent") +
  theme_bw() + 
  scale_color_manual(values = c(`TRUE` = "black"), guide = FALSE) +
  geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black", size = 0.8) +
  geom_polygon(data = canada, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  geom_polygon(data = mexico, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-113, -97),ylim = c(40,49),ratio = 1.3) +
  labs(fill = "%/year" )+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.text = element_text(size=12)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
  ggtitle(paste0("Trends in MODIS Corrected Land AOD (550 nm) 2000-2018 for Dust Events"))

modis.plot.sub




#pick an improve site to analyze further
test.site <- subset(dat.aero, site_name == "Bozeman")
#plot site for initial investigation
ggplot(test.site, aes(x = time_utc, y = AOD_fine)) + geom_point() + theme_bw()
test.var.sel <- "AOD_fine"
colnames(test.site)[colnames(test.site)=="time_utc"] <- "Date"

#get quantile for each year at each test.site
test.site.quant <- NULL
for (i in 1:length(q)) {
  #i=1
  tmp.dat <- test.site %>%
    group_by(year(Date)) %>%
    summarise_at(vars(test.var.sel), ~as.numeric(quantile(get(test.var.sel), na.rm = T, probs = q[i])))
  if (i == 1) {
    test.site.quant <- tmp.dat
    colnames(test.site.quant)[i+1] <- q[i]
  } else {
    test.site.quant[,i+1] <- tmp.dat[[test.var.sel]]
    colnames(test.site.quant)[i+1] <- q[i]
  }
  print(paste0('finished ', q[i]))
}

test.years <- unique(year(test.site$Date))
test.site.quant.trend <- NULL
for (i in 1:length(q)) {
  #i = 1
  test.site.var.quant <- NULL
  for (j in 1:length(test.years)) {
    #j = 2
    tmp.dat <- subset(test.site, year(Date) == test.years[j])
    tmp.quant <- subset(test.site.quant, `year(Date)` == test.years[j])
    tmp.high <- subset(tmp.dat, get(test.var.sel) >= tmp.quant[[i+1]])
    test.site.var.quant <- rbind(test.site.var.quant,tmp.high)
  }
  #get vector of date as numeric (in seconds)
  test.site.var.quant$Date_sec <- as.numeric(test.site.var.quant$Date)
  #use kendall trend test
  tmp.ken <- kendallTrendTest(get(test.var.sel) ~ Date_sec, test.site.var.quant, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  tmp.slope <- tmp.ken$estimate[2] * year.day
  tmp.p <- tmp.ken$p.value
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.normslope <- as.numeric(tmp.slope)/median(test.site.var.quant[[test.var.sel]], na.rm = TRUE)
  if (i == 1) {
    tmp.df <- data.frame("site_name" = test.site$site_name[1], 
                         "lat" = test.site$Latitude[1], 
                         "lon" = test.site$Longitude[1],
                         "slope" = tmp.slope/year.day,
                         "normslope" = tmp.normslope * 100,
                         "pval" = tmp.p,
                         "intercept" = tmp.intercept)
    test.site.quant.trend <- tmp.df
  } else {
    tmp.df <- data.frame("slope" = tmp.slope/year.day,
                         "normslope" = tmp.normslope * 100,
                         "pval" = tmp.p,
                         "intercept" = tmp.intercept)
    names(tmp.df) <- c(paste0("slope",q[i]),
                       paste0("normslope",q[i]),
                       paste0("pval",q[i]),
                       paste0("intercept",q[i]))
    test.site.quant.trend <- cbind(test.site.quant.trend, tmp.df)
  }
}


#plot site for initial investigation
ggplot(test.site, aes(x = Date, y = AOD_fine)) +
  geom_point(size = 0.5,
             color = ifelse(month(test.site$Date) == 3 | month(test.site$Date) == 4 | month(test.site$Date) == 5, "green",
                            ifelse(month(test.site$Date) == 6 | month(test.site$Date) == 7 | month(test.site$Date) == 8,"red","gray"))) +
  #scale_y_continuous(trans='log10') +
  #coord_trans(y="log10") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept, slope = slope), color = "blue") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept0.5, slope = slope0.5), color = "blue") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept0.75, slope = slope0.75), color = "blue") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept0.9, slope = slope0.9), color = "blue") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept0.95, slope = slope0.95), color = "blue") +
  geom_abline(data = test.site.quant.trend, aes(intercept = intercept0.98, slope = slope0.98), color = "blue") +
  theme_bw()

ggplot(test.site, aes(x = Date, y = AOD_fine)) + 
  geom_point(size = 0.5,
             color = ifelse(month(test.site$Date) == 3 | month(test.site$Date) == 4 | month(test.site$Date) == 5, "green",
                            ifelse(month(test.site$Date) == 6 | month(test.site$Date) == 7 | month(test.site$Date) == 8,"red","gray"))) +
geom_quantile(quantiles = q, 
              aes(linetype = as.factor(..quantile..)),
              #aes(color = as.factor(..quantile..)),
              size = 1, inherit.aes = TRUE, show.legend = TRUE) +
  scale_y_continuous(trans='log2') +
  theme_bw()




ggplot(data = bozeman.sub, aes(time_utc,AOD_500nm)) +
  geom_point(size = 0.5, 
             color = ifelse(month(bozeman.sub$time_utc) == 3 | month(bozeman.sub$time_utc) == 4 | month(bozeman.sub$time_utc) == 5, "green",
                            ifelse(month(bozeman.sub$time_utc) == 6 | month(bozeman.sub$time_utc) == 7 | month(bozeman.sub$time_utc) == 8,"red","gray"))) +
  #color = ifelse(month(bozeman.sub$time_utc) == 3 | month(bozeman.sub$time_utc) == 4 | month(bozeman.sub$time_utc) == 5, "green", 
  # ifelse(month(bozeman.sub$time_utc) == 6 | ifelse(month(bozeman.sub$time_utc) == 7 | ifelse(month(bozeman.sub$time_utc) == 8, "red", "gray"))))) + 
  #color = "gray") +
  geom_point(data=bozeman.daily.sub, aes(time_utc,AOD_500nm),color = "black", size = 0) +
  geom_line(data=bozeman.daily.sub, aes(group = grp), color = "black", size = 1) +#, 
  #color = "#09557f",
  #alpha = 0.6,
  #size = 0.6) +
  labs(x = "Date", 
       y = "AOD",
       #title = paste0(toupper(substring(sites[site.pick], 1, 1)), substring(sites[site.pick], 2), sep = ""),' Montana') +
       title = "Bozeman, Montana AOD") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        plot.title = element_text(size=15, face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=13))+
  #theme(axis.text.y = element_text(size=13)) +
  scale_y_continuous(trans='log2',breaks = c(0.02,0.2,2)) +
  geom_quantile(quantiles = qp, 
                aes(linetype = as.factor(..quantile..)),
                #aes(color = as.factor(..quantile..)),
                size = 1, inherit.aes = TRUE, show.legend = TRUE) +
  scale_linetype_manual(values=c("twodash","dashed","dotdash","solid")) +
  guides(linetype = guide_legend(title = "Quantiles")) #+
#labs(color="Quantiles") +
#scale_colour_manual(name="Quantiles") +
#scale_colour_manual(values = c('red', 'blue','black','yellow','red','red','red','red')) +
#geom_smooth(method="lm", se=FALSE) +
# scale_x_datetime(
#  breaks = seq(as.POSIXct("2000-01-01 00:00:00 UTC"),
#              as.POSIXct("2018-12-31 23:59:59 UTC"), "2 years"))

