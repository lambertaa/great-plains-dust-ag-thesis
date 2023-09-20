#This script was run with R version 4.0.2
#This script analyzes aeronet and improve data for the Great Plains region (this can be adjusted based on states and coordinates selected as needed).
#Quantile regression using the quantreg package is applied to each site for each network to obtain trends in high impact events
#Some quantile regression and mapping portions of the script require loading the quant_reg_boot_map_functions.R script and applying its functions
#We also obtain Sen's slope through the EnvStats package. This is only for IMPROVE as AERONET dataset is too large for the function to handle

#clear environments
rm(list=ls())

#load packages
library(EnvStats)
library(lubridate)
library(plyr)
library(dplyr)
library(quantreg)
library(gridExtra)
library(sp)
library(maps)
library(ggplot2)


#set working directory as /IMPROVE
setwd("~/IMPROVE")

#read in data, keep header. I downloaded them at separate times so have to read in both files separately and then combine them
data1 <- fread("IMPROVE.txt", stringsAsFactors = F, header = T, skip = 0)
data2 <- fread("IMPROVE2.txt", stringsAsFactors = F, header = T, skip = 0)

#combine separate data files and remove them
improve.dat <- rbind(data1,data2)
rm("data1","data2")

#call all -999 values NA
improve.dat[improve.dat == -999] <- NA

#rename columns for pm10, pm25, reconstructed, soil, coarse, total carbon for easier analysis throughout this script
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
#pick one of the above parameters to analyze
improve.var.sel <- "coarse"

#get var name for a more convenient way to do plot titles
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

#upon closer inspection of sitename, there are two sites in Voyageurs National Park in the same location (where an instrument change is apparent)
#will combine these two and eliminate the one month or so from the first instrument
#overlap begins 11-17-1999 and ends 12-29-1999. Removing observations for these dates from Voyageurs NP #1
improve.dat <- improve.dat[!(improve.dat$SiteName == "Voyageurs NP #1" & improve.dat$Date >= as.Date("11-17-1999", "%m-%d-%Y")),]

#rename Voyageurs sites to Voyageurs NP
improve.dat$SiteName[improve.dat$SiteName == "Voyageurs NP #1" | improve.dat$SiteName == "Voyageurs NP #2"] <- "Voyageurs NP"

#check plots for sites requiring further inspection here
test.site <- subset(improve.dat, SiteName == "Voyageurs NP")
ggplot(test.site, aes(x = Date, y = coarse)) + geom_point() + theme_bw()

#count number of observations for desired variable by site
count <- improve.dat %>%
  group_by(SiteName) %>%
  summarize_at(vars(improve.var.sel), ~sum(!is.na(.)))

#we already know anything with less than 10 observations for desired variable is worthless. let's remove those
sites.remove <- count[count$coarse <= 10,]$SiteName
improve.dat <- improve.dat[!(improve.dat$SiteName %in% sites.remove)]


#get sites vector
improve.sites <- unique(improve.dat$SiteName)

# write function to count observations for network and check if it meets required annual observations for at least 7 years
# this function will be used for both improve and aeronet sites
# x is dataframe, req is required number of obs per year, var is the chosen variable, represented by improve.var.sel
check.yearly.obs <- function(x,req,var,sites) {
  #count all non NA observations for each site for each year
  network.obs <- x %>%
    group_by(year(Date), SiteName) %>%
    summarise_at(vars(var), ~sum(!is.na(.)))
  
  network.obs <- network.obs[(network.obs[[var]] != 0),]
  
  #get maximum year gap for each site
  dat.lag <- NULL
  for (i in 1:length(sites)) {
    tmp.dat <- subset(network.obs, SiteName == sites[i])
    Diff = tmp.dat$`year(Date)` - lag(tmp.dat$`year(Date)`)
    tmp.max <- max(Diff, na.rm = TRUE)
    tmp.df <- data.frame("SiteName" = sites[i],
      "Diff" = tmp.max)
    dat.lag <- rbind(dat.lag, tmp.df)
  }
  
  #order dat.lag alphabetically to match year.check further down
  dat.lag$SiteName <- as.character(dat.lag$SiteName)
  dat.lag <- dat.lag[order(dat.lag$SiteName),]
  
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

#run function to check if network meets annual observational requirements for at least 7 years
check.yearly.obs(improve.dat,improve.obs.req,improve.var.sel,improve.sites)

#the year.check dataframe shows every the number of years meeting annual observational frequency criteria (check), the maximum gap in years between
#observations (diff), and the maximum possible number of consecutive years meeting observational criteria (best cons)
#any site with bestcons < 7 will be removed, but sites with data gaps 3 years or more are analyzed subjectively to determine any need for data removal

#check plots for sites requiring further inspection here
# test.site <- subset(improve.dat, SiteName == "Meadview")
# ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()
# test.site <- subset(improve.dat, SiteName == "Voyageurs NP")
# ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()
# test.site <- subset(improve.dat, SiteName == "Sycamore Canyon")
# ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()
# test.site <- subset(improve.dat, SiteName == "Saguaro NM")
# ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()
# test.site <- subset(improve.dat, SiteName == "Joshua Tree NP")
# ggplot(test.site, aes(x = Date, y = pm25)) + geom_point() + theme_bw()

#Remove data before 2003 from Meadview site since it is separated by an 11 year gap from the rest of the data
improve.dat <- improve.dat[!(improve.dat$SiteName == "Meadview" & year(improve.dat$Date) < 2003), ]
#Remove 2000 data from Voyageurs site since it is separated by an 6 year gap from the rest of the data
improve.dat <- improve.dat[!(improve.dat$SiteName == "Voyageurs NP" & year(improve.dat$Date) < 2000), ]
#Only has data in early 90s before large gap till 2000. Remove everything before
improve.dat <- improve.dat[!(improve.dat$SiteName == "Sycamore Canyon" & year(improve.dat$Date) < 2000), ]
#8 year gap between early 90s data and 2000 data at Saguaro NM. Removing everything before 2000
improve.dat <- improve.dat[!(improve.dat$SiteName == "Saguaro NM" & year(improve.dat$Date) < 2000), ]
#Another 8 year gap before 2000 at Joshua Tree NP. Removing data before then
improve.dat <- improve.dat[!(improve.dat$SiteName == "Joshua Tree NP" & year(improve.dat$Date) < 2000), ]

#run function to check if network meets annual observational requirements for at least 7 years again (check on year.check df)
check.yearly.obs(improve.dat,improve.obs.req,improve.var.sel,improve.sites)

#create new sites vector with only sites that meet criteria (pick is TRUE)
improve.year.check.sub <- subset(year.check, pick == "TRUE")
improve.sites <- unique(improve.year.check.sub$SiteName)

#subset improve.summer to sites that only meet criteria
improve.dat <- improve.dat[improve.dat$SiteName %in% improve.sites, ]

#generate ascending years vector
improve.years <- sort(unique(year(improve.dat$Date)), decreasing = FALSE)

#remove unnecessary data
rm(year.check, test.site, count, improve.year.check.sub, sites.remove, improve.obs.req)

#subset improve.dat to only desired states
#in this case, we are analyzing the Great Plains states below (in IMPROVE dataset, states are labeled by state abbreviation)
improve.dat <- subset(improve.dat, State == "ND" |
                        State == "SD" |
                        State == "NE" |
                        State == "KS" |
                        State == "OK" |
                        State == "AR" |
                        State == "MO" |
                        State == "IA" |
                        State == "MN")


#improve.site.combo <- subset(improve.dat, SiteName == "Voyageurs NP #1" | SiteName == "Voyageurs NP #2")

#update improve.sites vector
improve.sites <- unique(improve.dat$SiteName)

#get date into numeric format in preparation for quantile regression analysis
#Good info to know: origin for as.Date is Jan 1, 1970. Numeric date now will be in number of days since Jan 1, 1970
improve.dat$datenum <- as.numeric(improve.dat$Date)

#using dlply, we group improve.dat by sitename and apply the function knedallTrendTest to the dataframe (called df in the function)
#kendallTrendTest obtains Sens slope, Kendall's tau, pvalue, and other stats including confidence intervals indicated by conf.level = 0.95 (95% confidence intervals)
#we apply the test to the selected variable (improve.var.sel) against numeric date (datenum). Resulting slope is in change per day
improve.ken.list <- dlply(improve.dat, "SiteName", function(df)
  kendallTrendTest(get(improve.var.sel) ~ datenum, data = df, na.action = na.pass, ci.slope = T, conf.level = 0.95))

#we need each site's coordinates in order to plot the results
improve.info <- NULL
for (i in 1:length(improve.sites)) {
  #i = 26
  tmp.info <- improve.dat[improve.dat$SiteName == improve.sites[i],][1,]
  tmp.info <- data.frame("SiteName" = as.character(tmp.info$SiteName),
                         "Latitude" = tmp.info$Latitude,
                         "Longitude" = tmp.info$Longitude)
  improve.info <- rbind(improve.info, tmp.info)
}
rm(tmp.info)

#order alphabetically to match kendall trend test output
improve.info$SiteName <- as.character(improve.info$SiteName)
improve.info <- improve.info[order(improve.info$SiteName),]

#we need median to get slope normalized by the median (and eventually turned into percent change)
improve.median <- improve.dat %>%
  group_by(SiteName) %>%
  summarise_at(vars(all_of(improve.var.sel)), ~median(., na.rm = T))

#put everything into a single dataframe
improve.ken.sen.trends <- data.frame("SiteName" = names(improve.ken.list),
                                     "Latitude" = improve.info$Latitude,
                                     "Longitude" = improve.info$Longitude,
                                     "pval" = sapply(improve.ken.list, function(x) x$p.value),
                                     #percent change for slope and ci's obtained by dividing by median and multiplying by 100
                                     #must also multiply by 365 to convert from change/day to change/year
                                     "normslope" = (((sapply(improve.ken.list, function(x) x$estimate[2]))*365)/improve.median$coarse)*100,
                                     "LCL" = ((unlist(sapply(improve.ken.list, function(x) x$interval$limits[[1]]), use.names = FALSE)*365)/improve.median$coarse)*100,
                                     "UCL" = ((unlist(sapply(improve.ken.list, function(x) x$interval$limits[[2]]), use.names = FALSE)*365)/improve.median$coarse)*100
)

rm(improve.info, improve.ken.list, improve.year.check.sub, improve.median, i)

#set to home directory
setwd("~/")
#export table to csv so we don't have to go through this long process again
write.csv(improve.ken.sen.trends,"improve_ken_sen_ag_coarse_trends.csv", row.names = TRUE)

#remove any rows with NA obs to prep for plotting
improve.ken.sen.trends <- na.omit(improve.ken.sen.trends)

#Create Increasing/Decreasing factor column for plotting
for (i in 1:nrow(improve.ken.sen.trends)) {
  if (improve.ken.sen.trends$normslope[i] >= 0) {
    improve.ken.sen.trends$sign[i] <- "Increasing"
  } else {
    improve.ken.sen.trends$sign[i] <- "Decreasing"
  }
}

#Create  < or > 0.05 factor column for plotting
for (i in 1:nrow(improve.ken.sen.trends)) {
  if (improve.ken.sen.trends$pval[i] <= 0.05) {
    improve.ken.sen.trends$sig[i] <- "< 0.05"
  } else {
    improve.ken.sen.trends$sig[i] <- "> 0.05"
  }
}

#get map for states to plot
states <- map_data("state")

#subset to selected states
states.sel <- subset(states, region == "north dakota" | 
                       region == "south dakota" | 
                       region == "nebraska" | 
                       region == "kansas" |
                       region == "oklahoma" |
                       region == "minnesota" |
                       region == "iowa" |
                       region == "missouri")

#plot results
#plot states.sel as the base
ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #set coordinates extent for plot
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3) +
  #plot results using sign for color and significance for shape
  geom_point(data=improve.ken.sen.trends, aes(x=Longitude, y = Latitude,
                                              size = abs(normslope),
                                              colour = sign,
                                              fill = sign,
                                              shape = sig),
             group = FALSE) +
  #blue for decreasing, red for increasing
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
  #no legend for fill
  guides(fill = FALSE) +
  theme_bw() + 
  #set text size for legend
  theme(legend.text = element_text(size=12)) +
  #labels for axes
  xlab("Longitude") +
  ylab("Latitude") +
  #set size of text and margins
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
  theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 





#now perform quantile regression on improve data

#call quant_reg_boot_map_functions.R file for mapping and quantile regression functions
setwd("~/")
source("quant_reg_boot_map_functions.R")

#use site.quant.reg function to perform quantile regression with bootstrapped uncertainty statistics
site.quant.reg(df = improve.dat,
               network = "IMPROVE",
               sitevec = improve.sites,
               time.conv = 365,
               var = improve.var.sel,
               R.num  = 1000)
improve.quant.reg <- network.quant.reg
rm(network.quant.reg)

#export table to csv so we don't have to go through this ridciulously long process again
write.csv(improve.quant.reg,"improve_90_bootstrap_coarse_trends.csv", row.names = TRUE)

#read in table by uncommenting line below if you have already run this and want to map the data differently
#improve.quant.reg <- fread("improve_90_bootstrap_coarse_trends.csv")

#check statistical significance
improve.quant.reg$ss <- as.factor(improve.quant.reg$pval <= 0.05)

#get extent for mapping
long.ext <- c(-104.5, -89)
lat.ext <- c(34,49)

#use map trends function to map quantile regression values for improve
#mapping requires lat, lon, normslope, pval, and ss parameters in dataframe with those exact names
maptrends(trends.df = improve.quant.reg, 
          long.ext = long.ext, 
          lat.ext = lat.ext, 
          states.sel = states.sel)




#AERONET section
#only quantile regression applied here because datasets are too large for sen's slope function

#set working directory
setwd("~/AERONET/aeronet_ag")
#get filenames
file_list <- list.files()

#read in all files and combine into dat.bind. 
#this can take a while with 15 minute averaging applied. If you want to speed up the process, remove the averaging portion of the for loop
dat.aero <- NULL
for (i in seq_along(file_list)){
  #i=10
  #pull out ith filename
  fname <- file_list[i]
  #read file, skip first 6 rows, read in first 53 columns, set 1st row as column names
  input <- fread(fname, skip = 6, stringsAsFactors = F, check.names = T, select = c(1:53))
  #because of weird error, first column doesn't have a name, pull out 2nd through 53 column names and use those as names (so ignore the 79/80 column warning)
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

#select from AOD_fine, AOD_coarse, and AOD_total
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

#2019 data is incomplete as of the time of the download (some time in 2019), subset to all years before 2019
dat.aero <- subset(dat.aero, year(dat.aero$Date) < 2019)

#require at least 1 observation per day per year
aero.obs.req <- 365

#create vector of all aeronet sites
aero.sites <- unique(dat.aero$SiteName)

#convert all nan to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dat.aero[is.nan(dat.aero)] <- NA

#use check.yearly.obs function created above to check if sites meet requirement for annual frequency of obs and number of years with obs
check.yearly.obs(dat.aero, aero.obs.req, aero.var.sel, aero.sites)

#check on sites with large gaps
test.site <- subset(dat.aero, SiteName == "cart_site")
ggplot(test.site, aes(x = Date, y = AOD_coarse)) + geom_point() + theme_bw()

#remove all data from before 1998 because of 4 year gap prior in cart_site
dat.aero <- dat.aero[!(dat.aero$SiteName == "cart_site" & year(dat.aero$Date) < 1998), ]


#check on site observations again to see subsetted data meet criteria
check.yearly.obs(dat.aero, aero.obs.req, aero.var.sel, aero.sites)

#create new sites vector with only sites that meet criteria
year.check.sub <- subset(year.check, pick == "TRUE")
aero.sites <- unique(year.check.sub$SiteName)

rm(year.check,year.check.sub,test.site, aero.obs.req)

#subset dat.sub to sites that only meet criteria
dat.aero <- dat.aero[dat.aero$SiteName %in% aero.sites, ]

#generate ascending years vector
years <- sort(unique(year(dat.aero$Date)), decreasing = FALSE)

#convert date to numeric for quantile regression. Numeric date will be in seconds
dat.aero$datenum <- as.numeric(dat.aero$Date)

#number of seconds in a year - need this to convert from change/second to change/year for quantile regression analysis
year.sec <- 60*60*24*365

#use function on aeronet data to get quantile regression for desired variable

aero.quant.reg <- site.quant.reg(df = dat.aero, 
                                 network = "AERONET", 
                                 sitevec = aero.sites, 
                                 time.conv = year.sec, 
                                 var = aero.var.sel,
                                 R.num = 1000)
