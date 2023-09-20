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
#library("mblm", lib.loc="/usr/local/lib/R/site-library")
library(sp)
library(maptools)
library(maps)
library(rgdal)
library(reshape2)
library(scales)
library(raster)


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
rm(new_files)

#rename columns to easier names
colnames(dat.aero)[colnames(dat.aero)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.aero)[colnames(dat.aero)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.aero)[colnames(dat.aero)=="Total_AOD_500nm.tau_a."] <- "AOD_total"
colnames(dat.aero)[colnames(dat.aero)=="Site_Longitude.Degrees."] <- "Longitude"
colnames(dat.aero)[colnames(dat.aero)=="Site_Latitude.Degrees."] <- "Latitude"
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
aero.sites <- sites


#generate df with set time period averaged data
aero.averaged.dat <- dat.aero %>%
  group_by(Date = cut(Date, breaks="14 days"),SiteName) %>%
  summarise_all(mean, na.rm = TRUE)
aero.averaged.dat$Date <- as.POSIXct(aero.averaged.dat$Date)


#generate ascending years vector
years <- sort(unique(year(dat.aero$Date)), decreasing = FALSE)

dat.aero$datenum <- as.numeric(dat.aero$Date)

test.site <- subset(dat.aero, SiteName == sites[1])
test.site <- test.site[!is.na(test.site$AOD_coarse),]
boxplot(test.site$AOD_coarse)
hist(test.site$AOD_coarse)

#subset for o and g sites
# dat.aero <- subset(dat.aero, SiteName == "Bozeman" |
#          SiteName == "Missoula" |
#          SiteName == "NEON_CVALLA" |
#          SiteName == "Table_Mountain" |
#          SiteName == "BSRN_BAO_Boulder" |
#          SiteName == "Red_Mountain_Pass")

sites <- unique(dat.aero$SiteName)

# QR <- rq(test.site$AOD_coarse ~ test.site$datenum, tau=0.9)
# summary(QR, se='boot', bsmethod = "xy")

#LM <- lm(test.site$y~x)

#number of seconds in a year - need this to convert from change/second to change/year later on
year.sec <- 60*60*24*365

#call quant_reg_boot_map_functions.R file for mapping and quantile regression functions
setwd("~/")
source("quant_reg_boot_map_functions.R")

#use function on aeronet data to get quantile regression for desired variable
aero.quant.reg <- site.quant.reg(df = dat.aero, 
               network = "AERONET", 
               sitevec = sites, 
               time.conv = year.sec, 
               var = var.sel,
               R.num = 1000)

#store in appropriate df with network name
aero.quant.reg <- network.quant.reg
#remove df generated from qr funciton
rm(network.quant.reg)

#read in data
aero.quant.reg <- fread("aero_90_bootstrap_coarse_trends.csv")

#check statistical significance
aero.quant.reg$ss <- as.factor(aero.quant.reg$pval <= 0.05)

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

#use map trends function to map quantile regression values for aeronet
#mapping requires lat, lon, normslope, pval, and ss parameters in dataframe
maptrends(trends.df = aero.quant.reg)

#export table to csv so we don't have to go through this ridciulously long process again
write.csv(aero.quant.reg,"aero_90_bootstrap_coarse_trends.csv", row.names = TRUE)




#improve analysis
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

#create new soil plus coarse parameter
improve.dat$soilpluscoarse <- improve.dat$soil_pm25 + improve.dat$coarse

#select desired variable out of soil_pm25,pm25,pm25_rec,coarse,pm10,pm10_rec, or totalcarbon
improve.var.choices <- c("soil_pm25","pm25","pm25_rec","coarse","pm10","pm10_rec","totalcarbon","soilpluscoarse")
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
} else if (improve.var.sel == improve.var.choices[8]) {
  improve.var.string <- "Fine Soil Plus Coarse Mode"
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
improve.dat$datenum <- as.numeric(improve.dat$Date)

#use site.quant.reg function to perform quantile regression with bootstrapped uncertainty statistics
site.quant.reg(df = improve.dat,
               network = "IMPROVE",
               sitevec = improve.sites,
               time.conv = 365,
               var = improve.var.sel,
               R.num  = 1000)
improve.quant.reg <- network.quant.reg
rm(network.quant.reg)


setwd("~/")
improve.quant.reg <- fread("improve_90_bootstrap_coarse_trends.csv")
#check statistical significance
improve.quant.reg$ss <- as.factor(improve.quant.reg$pval <= 0.05)

#remove sites with weirdly large trends
improve.quant.reg <- subset(improve.quant.reg, sitename != "Salmon NF")
imnprove.quant.reg <- improve.quant.reg[!is.na(improve.quant.reg$normslope),] #remove rows with NA in variable

#use map trends function to map quantile regression values for improve
#mapping requires lat, lon, normslope, pval, and ss parameters in dataframe
maptrends(trends.df = improve.quant.reg)

#export table to csv so we don't have to go through this ridciulously long process again
write.csv(improve.quant.reg,"improve_90_bootstrap_coarse_trends.csv", row.names = TRUE)



#monthly analysis
#aeronet

#create months numbered vector
months <- c(1:12)
#month names for plotting
monthnames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")


monthly.quant.reg.boot <- function(months, sites, data,var, R.num, monthnames, time.conv) {
  #network.monthly.trends <- data.frame(id = character(0), nrow = length(sites)*length(months), ncol = 10)
  network.monthly.trends <- data.frame(matrix(NA, nrow = length(sites)*length(months), ncol = 10))
  network.monthly.trends[c(1,10)] <- sapply(network.monthly.trends[c(1,10)],as.character)
  #network.monthly.trends <- NULL
  for (i in 1:length(months)) {
    #i = 1
    tmp.month <- subset(data, month(Date) == months[i])
    
    #get quantile for each year at each site for the month
    tmp.month.df <- NULL
    for (j in 1:length(sites)) {
      #j = 1
      tmp.site <- subset(tmp.month, SiteName == sites[j])
      tmp.site <- tmp.site[!is.na(tmp.site[[var]]),]
      if (nrow(tmp.site) == 0) {
        tmp.df <- data.frame("sitename" = sites[j],
                             "lat" = tmp.site$Latitude[1],
                             "lon" = tmp.site$Longitude[1],
                             "slope" = NA,
                             "normslope" = NA,
                             "lowerci" = NA,
                             "upperci" = NA,
                             "pval" = NA,
                             "month" = months[i],
                             "monthname" = monthnames[i]) 
      } else {
        tmp.qr <- rq(get(var) ~ datenum, tau=0.9, data = tmp.site)
        qr.summ <- summary(tmp.qr, se='boot', bsmethod = "xy", R = R.num)
        rqboot <- boot.rq(cbind(1,tmp.site$datenum),tmp.site[[var]],tau=0.9, R = R.num, na.rm = T, bsmethod = "xy") 
        tmp.ci <- t(apply(rqboot$B, 2, quantile, c(0.025,0.975)))
        #pull out parameters from quantile regression summary
        tmp.slope <- qr.summ$coefficients[2,1] * time.conv
        tmp.normslope <- (tmp.slope/quantile(tmp.site[[var]], c(0.9)))*100
        tmp.p <- qr.summ$coefficients[2,4]
        tmp.normci <- c((tmp.ci[2,1]/quantile(tmp.site[[var]], c(0.9))),
                        (tmp.ci[2,2]/quantile(tmp.site[[var]], c(0.9))))
        #store in tmp df
        tmp.df <- data.frame("sitename" = sites[j],
                             "lat" = tmp.site$Latitude[1],
                             "lon" = tmp.site$Longitude[1],
                             "slope" = tmp.slope,
                             "normslope" = tmp.normslope,
                             "lowerci" = tmp.normci[1],
                             "upperci" = tmp.normci[2],
                             "pval" = tmp.p,
                             "month" = months[i],
                             "monthname" = monthnames[i]) 
      }
      #tmp.month.df <- rbind(tmp.month.df,tmp.df)
      #row.names(tmp.month.df) <- 1:nrow(tmp.month.df)
      tmp.df[c(1,10)] <- sapply(tmp.df[c(1,10)],as.character)
      print(paste0("finished ", sites[j], " ", monthnames[i]))
      network.monthly.trends[((i-1)*length(sites))+j,] <- tmp.df
    }
    #network.monthly.trends[c((j*i):((j*i)+11)),] <- tmp.month.df
    #network.monthly.trends <<- rbind(network.monthly.trends,tmp.month.df)
  }
  colnames(network.monthly.trends) <- colnames(tmp.df)
  network.monthly.trends$nonzero <- !(network.monthly.trends$lowerci <= 0 & network.monthly.trends$upperci >= 0)
  #check statistical significance
  network.monthly.trends$ss <- as.factor(network.monthly.trends$pval <= 0.05)
  
  #remove rows with NA for slope
  network.monthly.trends <- na.omit(network.monthly.trends)
  
  for (i in 1:nrow(network.monthly.trends)) {
    if (network.monthly.trends$pval[i] <= 0.05) {
      network.monthly.trends$sig[i] <- "< 0.05"
    } else {
      network.monthly.trends$sig[i] <- "> 0.05"
    }
  }
  
  for (i in 1:nrow(network.monthly.trends)) {
    if (network.monthly.trends$normslope[i] >= 0) {
      network.monthly.trends$sign[i] <- "Increasing"
    } else {
      network.monthly.trends$sign[i] <- "Decreasing"
    }
  }
  return(network.monthly.trends)
}

#aeronet monthly analysis
aero.monthly.trends <- monthly.quant.reg.boot(months = months,
                       sites = aero.sites,
                       data = dat.aero,
                       var = var.sel,
                       R.num = 1000,
                       monthnames = monthnames,
                       time.conv = 365*24*60)

#export table to csv so we don't have to go through this ridciulously long process again
write.csv(aero.monthly.trends,"aeronet_90_bootstrap_monthly_coarse_trends.csv", row.names = TRUE)


#improve monthly analysis
improve.monthly.trends <- monthly.quant.reg.boot(months = months,
                       monthnames = monthnames,
                       sites = improve.sites, 
                       data = improve.dat,
                       var = improve.var.sel,
                       R.num = 1000,
                       time.conv = 365)

#export table to csv so we don't have to go through this ridciulously long process again
write.csv(improve.monthly.trends,"improve_90_bootstrap_monthly_coarse_trends.csv", row.names = TRUE)

aero.monthly.trends <- fread("aeronet_90_bootstrap_monthly_coarse_trends.csv")
improve.monthly.trends <- fread("improve_90_bootstrap_monthly_coarse_trends.csv")

#function to pull out legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

monthly.plots <- function(monthly.trends) {
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
                  geom_point(data=subset(monthly.trends, monthname == monthnames[i]), aes(x=lon, 
                                                                                                  y = lat,
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
  maxslope <- monthly.trends %>%
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
               mylegend, widths = c(10,1.5))
               #top = textGrob(paste0(q.string, "th Quantile Trends - ", improve.var.string),gp=gpar(fontsize = 12, font = 3)))
}

monthly.plots(monthly.trends = improve.monthly.trends)







# do the same but for subset region
#make empty list to store plots
monthly.trends.sub <- function(monthly.trends, var.string) {
  p.sub <- list()
  #generate base plot to add data to
  monthly.sub <- subset(monthly.trends, lat >= 32.5 & lat <= 50 & lon >= -105 & lon <= -90)
  #monthly.sub$sign <- as.factor(monthly.sub$sign)
  basemap <- ggplot() + geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
    coord_fixed(xlim = c(-105, -90),ylim = c(32.5,49),ratio = 1.3)
  
  scale_fill_monthly <- function(...){
    ggplot2:::manual_scale(
      'fill', 
      values = setNames(c('blue', 'red'), c("Decreasing","Increasing")), 
      drop = FALSE,
      ...
    )
  }
  
  scale_color_monthly <- function(...){
    ggplot2:::manual_scale(
      'color', 
      values = setNames(c('blue', 'red'), c("Decreasing","Increasing")), 
      drop = FALSE,
      ...
    )
  }
  
  scale_shape_monthly <- function(...){
    ggplot2:::manual_scale(
      'shape', 
      values = setNames(c(1, 19), c("FALSE","TRUE")), 
      drop = FALSE,
      guide = "legend",
      ...
    )
  }
  
  #only use specific months
  monthly.sub.sub <- subset(monthly.sub, monthname == "Mar" | monthname == "June" | monthname == "Oct")
  #run for loop to make a new plot for each month
  for (i in 1:length(months)) {
    #store plots in list
    #temporary plot for one month
    #i = 10
    tmp.monthly <- subset(monthly.sub, monthname == monthnames[i])
    tmp.plot = (basemap +
                  geom_point(data=tmp.monthly, aes(x=lon, y = lat,
                                                   shape = as.factor(ss),
                                                   size = abs(normslope),#),
                                                   colour = sign,
                                                   fill = sign),
                             group = FALSE) +
                  scale_color_monthly() +
                  scale_fill_monthly() +
                  scale_shape_monthly() +
                  #scale_color_manual(values = c("Decreasing" = "Blue","Increasing" = "Red"), guide = "legend", drop = FALSE) +
                  #scale_fill_manual(values = c("Decreasing" = "Blue","Increasing" = "Red"), guide = FALSE, drop = FALSE) +
                  #scale_size_identity(trans = "sqrt", guide = "legend") +
                  scale_size_continuous(range(1:6), limits = c(0,round(max(monthly.sub.sub$normslope))), breaks = c(5,10,15,20), guide = "legend") + 
                  #scale_size_continuous(range(1:6), guide = "legend") + 
                  #scale_shape_manual(values = c("FALSE" = 1,"TRUE" = 16), labels = c("> 0.05","< 0.05"), guide = "legend", drop = FALSE) +
                  labs(size = "%/year", color = "Trend", shape = "P-val") +
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
  maxslope <- monthly.sub.sub %>%
    #maxslope <- aero.all.months.sites.slope %>%
    group_by(month) %>%
    summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))
  
  #pull out legend
  mylegend<-g_legend(p.sub[[maxslope$month[which.max(maxslope$normslope)]]])
  
  grid.arrange(arrangeGrob(p.sub[[3]] + theme(legend.position = "none"),
                           p.sub[[6]] + theme(legend.position = "none"),
                           p.sub[[10]] + theme(legend.position = "none"),
                           ncol = 3),
               mylegend, widths = c(10,1.5))
               #top = textGrob(paste0(q.string, "th Quantile Trends - ", var.string),gp=gpar(fontsize = 12, font = 3)))
}

monthly.trends.sub(monthly.trends = improve.monthly.trends,
                   var.string = improve.var.string)

#put aeronet and improve together for monthly plots

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                     region == "colorado" | 
                     region == "montana" | 
                     region == "north dakota")# |
                       #region == "south dakota" |
                       #region == "nebraska" |
                       #region == "kansas" |
                       #region == "oklahoma" |
                       #region == "arkansas" |
                       #region == "missouri" |
                       #region == "iowa" |
                       #region == "minnesota")


improve.monthly.trends$network <- "IMPROVE"
aero.monthly.trends$network <- "AERONET"
network.monthly.trends <- rbind(improve.monthly.trends,aero.monthly.trends)
network.monthly.trends$netsig <- paste0(network.monthly.trends$network," ",network.monthly.trends$sig)


#get ag states by themselves
#us <- getData("GADM", country="USA", level=1)
us <- as_Spatial(read_sf("~/cb_2018_us_state_500k.shp"))
agstates = us[match(toupper(c(unique(states.ag$region))),toupper(us$NAME)),]
#agstates = us[match(toupper(c("North Dakota","South Dakota","Nebraska","Kansas","Oklahoma","Missouri","Iowa","Minnesota")),toupper(us$NAME_1)),]
CRS.new <- proj4string(agstates)
spdf <- SpatialPointsDataFrame(coords = network.monthly.trends[, c("lon", "lat")], data = network.monthly.trends)
                               #proj4string = CRS.new)
proj4string(spdf) <- CRS.new

network.monthly.trends.ag <- spdf[!is.na(over(spdf, as(agstates, "SpatialPolygons"))), ]
network.monthly.trends.ag <- as.data.frame(network.monthly.trends.ag)
network.monthly.trends.ag$netsig <- paste0(network.monthly.trends.ag$network," ",network.monthly.trends.ag$sig)





p.sub <- list()
#subset network slopes
# network.monthly.sub <- subset(network.all.months.sites.slope, lat >= 36 & lat <= 43 & lon >= -104 & lon <= -92.5)
# network.monthly.sub$sign <- as.factor(network.monthly.sub$sign)
# network.monthly.sub$netsig <- as.factor(network.monthly.sub$netsig)
#generate base plot to add data to
basemap <- ggplot() + geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3)

basemap.og <- ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-116, -96.5),ylim = c(37,49),ratio = 1.3)
#run for loop to make a new plot for each month
for (i in 1:length(months)) {
  #store plots in list
  #temporary plot for one month
  tmp.plot = (basemap +
                geom_point(data=subset(network.monthly.trends.ag, monthname == monthnames[i]), aes(x=lon, y = lat,
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
                                   limits = levels(network.monthly.trends.ag$sign)) +
                scale_shape_manual(values = c(24,2,15,0), 
                                   labels = c("AERO < 0.05", "AERO > 0.05", "IMPROVE < 0.05","IMPROVE > 0.05"), 
                                   guide = "legend", 
                                   drop = FALSE, 
                                   limits = levels(as.factor(network.monthly.trends$netsig))) +
                scale_fill_manual(values = c("Blue","Red"), guide = FALSE, drop = FALSE) +
                scale_size_continuous(range = c(3,9), guide = "legend", breaks = c(5,10,15,20), limits = c(0,40)) +
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
maxslope <- network.monthly.trends.ag %>%
  group_by(month) %>%
  summarise_at(vars(normslope), ~max(abs(normslope),na.rm = TRUE))

#pull out legend
mylegend<-g_legend(p.sub[[which.max(maxslope$normslope)]])

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
             mylegend, widths = c(10,1.5))
             #top = textGrob(paste0(q.string, "th Quantile Trends - ", improve.var.string, " PM + AOD"),gp=gpar(fontsize = 12, font = 3)))

grid.arrange(arrangeGrob(p.sub[[3]] + theme(legend.position = "none"),
                         p.sub[[6]] + theme(legend.position = "none"),
                         p.sub[[10]] + theme(legend.position = "none"),
                         ncol = 3),
             mylegend, 
             widths = c(10,1.5),
             top = textGrob(paste0(q.string, "th Quantile Trends - ", var.string),gp=gpar(fontsize = 12, font = 3)))



#make regional monthly plots
aero.monthly.points <- SpatialPoints(data.frame(aero.monthly.trends$lon,aero.monthly.trends$lat))
proj4string(aero.monthly.points) <- proj4string(us)
aero.monthly.trends$state <- as.character(over(aero.monthly.points,us)$NAME_1)

aero.monthly.trends.var <- aero.monthly.trends %>%
  group_by(state) %>%
  summarize_at(vars(normslope), ~var(., na.rm = T))

aero.monthly.trends.var <- subset(aero.monthly.trends.var, state == "North Dakota" |
                                   state == "South Dakota" |
                                   state == "Nebraska" |
                                   state == "Kansas" |
                                   state == "Oklahoma" |
                                   state == "Missouri" |
                                   state == "Iowa" |
                                   state == "Minnesota")

improve.monthly.points <- SpatialPoints(data.frame(improve.monthly.trends$lon,improve.monthly.trends$lat))
proj4string(improve.monthly.points) <- proj4string(us)
improve.monthly.trends$state <- as.character(over(improve.monthly.points,us)$NAME_1)

improve.monthly.trends.var <- improve.monthly.trends %>%
  group_by(state) %>%
  summarize_at(vars(normslope), ~var(., na.rm = T))

improve.monthly.trends.var <- subset(improve.monthly.trends.var, state == "North Dakota" |
                                    state == "South Dakota" |
                                    state == "Nebraska" |
                                    state == "Kansas" |
                                    state == "Oklahoma" |
                                    state == "Missouri" |
                                    state == "Iowa" |
                                    state == "Minnesota")


aero.monthly.nosat <- aero.monthly.trends[!is.na(aero.monthly.trends$state), ]

#assign states to regions
GP <- c("North Dakota", "South Dakota" , "Minnesota" , "Kansas" , "Nebraska" , "Iowa" , "Missouri")
SW <- c("Arizona" , "New Mexico" , "Utah" , "Colorado")
NW <- c("Washington" , "Oregon" , "Idaho" , "Montana" , "Wyoming")
W <- c("California", "Nevada")
S <- c("Texas","Oklahoma","Louisianna","Arkansas")

#create empty region column
aero.monthly.nosat$region <- rep(NA,nrow(aero.monthly.nosat))


for (i in 1:nrow(aero.monthly.nosat)) {
  if (sum(as.numeric(aero.monthly.nosat$state[i] == SW))==1) {
    aero.monthly.nosat$region[i] = "SW"
  } else if (sum(as.numeric(aero.monthly.nosat$state[i] == NW))==1) {
    aero.monthly.nosat$region[i] = "NW"
  } else if (sum(as.numeric(aero.monthly.nosat$state[i] == GP))==1) {
    aero.monthly.nosat$region[i] = "GP"
  } else if (sum(as.numeric(aero.monthly.nosat$state[i] == W))==1) {
    aero.monthly.nosat$region[i] = "W"
  } else if (sum(as.numeric(aero.monthly.nosat$state[i] == S))==1) {
    aero.monthly.nosat$region[i] = "S"
  }
}

region.trend <- aero.monthly.nosat %>%
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
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=.2,
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





























etestdata <- subset(improve.dat, SiteName == "Agua Tibia" | SiteName == "Yellowstone NP 1")


qr.summ.monthly <- dlply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df)
  #summary(rq(get(var) ~ datenum, tau=0.9, data = df))))
  summary(rq(get(improve.var.sel) ~ datenum, tau=0.9, data = df, na.rm = T), se='boot', bsmethod = "xy", R = 1000)))

qr.monthly.stats <- ldply(qr.summ.monthly, coef)

qr.boot.monthly <- dlply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df, xname = "datenum", yname = "coarse") {
  xmat = cbind(1,df[[xname]])
  y = df[[yname]]
  rqboot = boot.rq(xmat,y, tau = 0.9, R = 100, na.rm = T, bsmethod = "xy")
  t(apply(rqboot$B, 2, quantile, c(0.025,0.975)))
}))

testdata[, list(list(boot.rq(cbind(1,datenum), coarse, tau = 0.9, R = 100, na.rm = T, bsmethod = "xy"))), by = sitemonth]



test <- by(improve.dat, improve.dat$sitemonth, testfun(df = improve.dat, datenumcol = improve.dat$datenum, varcol = improve.dat$coarse))

qr.boot.monthly <- dlply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df) {
  x <-df$datenum
  y <- df$coarse
  store <- boot.rq(cbind(1,x),y,tau=0.9, R = 100, na.rm = T, bsmethod = "xy")
  t(apply(store$B, 2, quantile, c(0.025,0.975)))
}))

qr.boot.monthly <- dlply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df) {
  boot.rq(cbind(1,datenum),coarse,tau=0.9, R = 100, na.rm = T, bsmethod = "xy", data = df)
}))

qr.boot.monthly <- function(df) 
  boot.rq(cbind(1,datenum),get(improve.var.sel), tau = 0.9, R = 100, na.rm = T, bsmethod = "xy", data = df)

test <- improve.dat %>%
  group_by(SiteName,month(Date)) %>%
  lapply(function(xx) boot.rq(cbind(1,xx$datenum),xx$coarse, tau = 0.9, R = 100, na.rm = T, bsmethod = "xy"))

test <- improve.dat %>%
  group_by(SiteName,month(Date)) %>%
  do(boot.rq(cbind(1,"datenum"),"coarse", tau = 0.9, R = 100, na.rm = T, bsmethod = "xy"))

testfun <- function(df,datecol,sitecol,datenumcol,varcol) {
  df %>%
    group_by(sitecol, month(datecol)) %>%
    boot.rq(cbind(1,"datenum"),"coarse", tau = 0.9, R = 100, na.rm = T, bsmethod = "xy")
}

function(df) 


ddply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df) {
  x = df[[datenumcol]]
  y = df[[improve.var.sel]]
  boot.rq(cbind(1,x),y,tau=0.9, R = 100, na.rm = T, bsmethod = "xy")
}), browser())

dlply(improve.dat, as.quoted(.(SiteName,month(Date))), function(df) browser())

ddply(improve.dat, .(SiteName,month(Date)), function(df) browser() )

#qr.summ.monthly <- dlply(improve.dat, as.quoted(.(SiteName,month(Date))), failwith(NULL, function(df)
 # summary(rq(get(var) ~ datenum, tau=0.9, data = df))))
  #summary(rq(get(var) ~ datenum, tau=0.9, data = df), se='boot', bsmethod = "xy", R = 1000)))










test <- as.data.frame(qr.monthly.stats)
sapply(qr.summ.monthly, coefficients)










appQR.b <- boot.rq(cbind(1,test.site$datenum),test.site$AOD_coarse,tau=0.9, R=1000, na.rm = T)
hist(QR.b$B[,2])

t(apply(QR.b$B, 2, quantile, c(0.025,0.975)))
confint(LM)


plot(test.site$Date,test.site$AOD_coarse)
abline(coefficients(LM),col="green")
abline(coefficients(QR),col="blue")

for(i in seq_len(nrow(QR.b$B))) {
  abline(QR.b$B[i,1], QR.b$B[i,2], col='#0000ff01')
}
