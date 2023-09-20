
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

dat.aero <- fread("20020101_20121231_Fresno.ONEILL_lev20",skip = 6, stringsAsFactors = F, check.names = T, select = (c(1:53)))
#because of weird error, first column doesn't have a name, pull out 2nd through 53 column names and use those as names
header <- names(dat.aero)[2:53]
dat.aero <- dat.aero[,1:52]
colnames(dat.aero) <- header
#change all -999 values to NA
dat.aero[dat.aero == -999] <- NA
#combine date and time
dat.aero$Date <- as.POSIXct(paste(dat.aero$Date_.dd.mm.yyyy., dat.aero$Time_.hh.mm.ss.), format="%d:%m:%Y %H:%M:%S")
#create sitename var
dat.aero$SiteName <- rep(sapply(dat.aero$AERONET_Site_Name[1], tolower), length(dat.aero$Date))

#rename columns to easier names
colnames(dat.aero)[colnames(dat.aero)=="Coarse_Mode_AOD_500nm.tau_c."] <- "AOD_coarse"
colnames(dat.aero)[colnames(dat.aero)=="Fine_Mode_AOD_500nm.tau_f."] <- "AOD_fine"
colnames(dat.aero)[colnames(dat.aero)=="Total_AOD_500nm.tau_a."] <- "AOD_total"

#quantiles, select quantile for analysis with q.sel, get q.ind to be used later, get q.string to use in plot titles
q <- c(0,0.25,0.5,0.75,0.90,0.95,0.98)
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


#begin IMPROVE portion

#set working directory as /IMPROVE
setwd("~/IMPROVE")

#read in data, keep header. I downloaded them at separate times so have to read in both files separately and then combine them
data1 <- fread("IMPROVE.txt", stringsAsFactors = F, header = T, skip = 0)
data2 <- fread("IMPROVE2.txt", stringsAsFactors = F, header = T, skip = 0)

improve.dat <- rbind(data1,data2)
rm("data1","data2")

#get only selected site (fresno)
dat.improve <- subset(improve.dat, SiteName == "Fresno")

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


#get only selected site (fresno)
dat.improve <- subset(improve.dat, SiteName == "Fresno")

#plots both together
ggplot() +
  geom_line(mapping = aes(x = dat.improve$Date, y = dat.improve$coarse), color = "blue") +
  geom_line(mapping = aes(x = dat.aero$Date, y = dat.aero$AOD_coarse*150), color = "black") +
  scale_y_continuous(
    "AOD",
    sec.axis = sec_axis(~ . /150, name = "AOD")
  ) +
  theme_bw()

#now only plot overlap
#get subs for overlapping times
date.min <- min(dat.improve$Date, na.rm = T)
date.max <- max(dat.aero$Date, na.rm = T)
dat.aero.sub <- subset(dat.aero, Date >= date.min)
dat.improve.sub <- subset(dat.improve, Date <= date.max)

# # generate the groups automatically and plot
# idx <- c(1, diff(dat.aero.sub$Date))
# i2 <- c(1,which(idx != 1), nrow(dat.aero.sub)+1)
# dat.aero.sub$grp <- rep(1:length(diff(i2)), diff(i2))
# 
# 
# #plot subsets together
# ggplot() +
#   geom_line(mapping = aes(x = dat.improve.sub$Date, y = dat.improve.sub$coarse), color = "blue") +
#   geom_line(mapping = aes(x = dat.aero.sub$Date, y = dat.aero.sub$AOD_coarse*150, group = dat.aero.sub$grp), color = "black") +
#   geom_point(mapping = aes(x = dat.aero.sub$Date, y = dat.aero.sub$AOD_coarse*150), color = "black", size = 0.00001) +
#   scale_y_continuous(
#     "AOD",
#     sec.axis = sec_axis(~ . /150, name = "AOD")
#   ) +
#   theme_bw()
# 
# #zoom in
# ggplot() +
#   geom_line(mapping = aes(x = dat.improve.sub$Date, y = dat.improve.sub$coarse), color = "blue") +
#   geom_line(mapping = aes(x = dat.aero.sub$Date, y = dat.aero.sub$AOD_coarse*300), color = "black") +
#   #geom_point(mapping = aes(x = dat.aero.sub$Date, y = dat.aero.sub$AOD_coarse*300), color = "black", size = 0.00001) +
#   scale_y_continuous(
#     limits = c(0,60),
#     "ug/m^3",
#     sec.axis = sec_axis(~ . /300, name = "AOD")
#   ) +
#   theme_bw() +
#   scale_x_date(limits = c(as.Date("2011-05-01"), as.Date("2011-11-01"))) +
#   xlab("Date")
# 
# 
# #try getting monthly averaged obs and plotting
# aero.sub.monthly <- dat.aero.sub %>%
#   group_by(Date = cut(Date, breaks="1 month")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE)
# aero.sub.monthly$Date <- as.Date(aero.sub.monthly$Date)
# 
# improve.sub.monthly <- dat.improve.sub %>%
#   group_by(Date = cut(Date, breaks="1 month")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE)
# improve.sub.monthly$Date <- as.Date(improve.sub.monthly$Date)
# 
# # generate the groups automatically and plot
# idx <- c(1, diff(aero.sub.monthly$Date))
# i2 <- c(1,which(idx != 10), nrow(aero.sub.monthly)+1)
# aero.sub.monthly$grp <- rep(1:length(diff(i2)), diff(i2))
# 
# #plot subsets together
# ggplot() +
#   geom_line(mapping = aes(x = improve.sub.monthly$Date, y = improve.sub.monthly$coarse), color = "blue") +
#   geom_line(mapping = aes(x = aero.sub.monthly$Date, y = aero.sub.monthly$AOD_coarse*300), color = "black") +
#   #geom_point(mapping = aes(x = aero.sub.monthly$Date, y = aero.sub.monthly$AOD_coarse*150), color = "black", size = 0.00001) +
#   scale_y_continuous(
#     "AOD",
#     sec.axis = sec_axis(~ . /300, name = "AOD")
#   ) +
#   theme_bw()
# 
# merge(improve.sub.monthly$Date, aero.sub.monthly, by.y = "Date", by.x = "AOD_coarse", all.x = TRUE)
# 
# # create sample data frame with sales data
# test <- data.frame(date = as.Date(c("2017/08/12", "2017/08/15", "2017/09/02")), quantity = c(3,2,1))
# # create the date range
# dates <- data.frame(date = seq(min(test$date), max(test$date), by = "day"))
# # perform the left join
# # (keeping all rows from "dates", and joining the sales dataset to them)
# result <- merge(dates, test, by.y = "date", by.x = "date", all.x = TRUE)
# 
# setDT(aero.sub.monthly)
# setDT(improve.sub.monthly)
# 
# improve.sub.monthly[aero.sub.monthly, on = c("Date")]
# 
# 
# 
# 
# 
# dat.aero.sub.ave <- dat.aero.sub %>%
#   group_by(Date = cut(Date, breaks="1 hour")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE)
# 
# dat.aero.sub.ave$SiteName <- rep(sapply(dat.aero$AERONET_Site_Name[1], tolower), length(dat.aero.sub.ave$Date))


# #because aeronet data is hourly averaged, date_sec actually goes by hours so year.sec must equal hours in a year
# year.sec <- 24*365
# #get regression coefficients and confidence intervals for each quantile
# aero.slopes <- NULL
# for (i in 1:length(q)) {
#   #i = 1
#   tmp.quant <- quantile(dat.aero.sub.ave[[var.sel]], na.rm = T, probs = q[i])
#   tmp.dat <- subset(dat.aero.sub.ave, get(var.sel) >= tmp.quant)
#   tmp.dat$Date_sec <- as.numeric(tmp.dat$Date)
#   tmp.ken <- kendallTrendTest(get(var.sel) ~ Date_sec, tmp.dat, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
#   #multiply slope by year.sec to go from change/sec to %change/year
#   tmp.slope <- tmp.ken$estimate[2] * year.sec * 100
#   tmp.p <- tmp.ken$p.value
#   tmp.tau <- tmp.ken$estimate[1]
#   tmp.intercept <- tmp.ken$estimate[3]
#   tmp.ci <- tmp.ken$interval$limits * year.sec * 100
#   #normalize slope and confidence intervals by dividing by the median value for that site within the quantile
#   tmp.normslope <- as.numeric(tmp.slope)/median(tmp.dat[[var.sel]], na.rm = TRUE)
#   tmp.normci <- tmp.ci/median(tmp.dat[[var.sel]], na.rm, na.rm = TRUE)
#   #store all of these values in a temporary dataframe
#   tmp.df <- data.frame("normslope" = tmp.normslope,
#                        "pval" = tmp.p,
#                        "normLCI" = tmp.normci[1],
#                        "normUCI" = tmp.normci[2])
#   if (i == 1) {
#     aero.slopes <- tmp.df
#   } else {
#     aero.slopes <- cbind(aero.slopes, tmp.df)
#   }
#   print(paste0("finished ",q[i]))
# }
# 
# colnames(aero.slopes) <- c("slope0",
#                            "p0",
#                            "lci0",
#                            "uci0",
#                            "slope25",
#                            "p25",
#                            "lci25",
#                            "uci25",
#                            "slope50",
#                            "p50",
#                            "lci50",
#                            "uci50",
#                            "slope75",
#                            "p75",
#                            "lci75",
#                            "uci75",
#                            "slope90",
#                            "p90",
#                            "lci90",
#                            "uci90",
#                            "slope95",
#                            "p95",
#                            "lci95",
#                            "uci95",
#                            "slope98",
#                            "p98",
#                            "lci98",
#                            "uci098")

#because aeronet data is hourly averaged, date_sec actually goes by hours so year.sec must equal hours in a year
year.sec <- 24*365
#get regression coefficients and confidence intervals for each quantile
aero.slopes.r <- NULL
for (i in 1:length(q)) {
  #i = 1
  tmp.quant <- quantile(dat.aero.sub.ave[[var.sel]], na.rm = T, probs = q[i])
  tmp.dat <- subset(dat.aero.sub.ave, get(var.sel) >= tmp.quant)
  tmp.dat$Date_sec <- as.numeric(tmp.dat$Date)
  tmp.ken <- kendallTrendTest(get(var.sel) ~ Date_sec, tmp.dat, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  #multiply slope by year.sec to go from change/sec to %change/year
  tmp.slope <- tmp.ken$estimate[2] * year.sec * 100
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits * year.sec * 100
  #normalize slope and confidence intervals by dividing by the median value for that site within the quantile
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.dat[[var.sel]], na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.dat[[var.sel]], na.rm, na.rm = TRUE)
  #store all of these values in a temporary dataframe
  tmp.df <- data.frame("normslope" = tmp.normslope,
                       "pval" = tmp.p,
                       "normLCI" = tmp.normci[1],
                       "normUCI" = tmp.normci[2],
                       "q" = as.factor(q[i]))
  aero.slopes.r <- rbind(aero.slopes.r, tmp.df)
  print(paste0("finished ",q[i]))
}

ggplot(data = aero.slopes.r, aes(x = q, y = normslope)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=normLCI, ymax=normUCI), width=.2,
                position=position_dodge(0.05))


#because aeronet data is hourly averaged, date_sec actually goes by hours so year.sec must equal hours in a year
days <- 365
#get regression coefficients and confidence intervals for each quantile
improve.slopes <- NULL
for (i in 1:length(q)) {
  #i = 1
  tmp.quant <- quantile(dat.improve.sub$coarse, na.rm = T, probs = q[i])
  tmp.dat <- subset(dat.improve.sub, coarse >= tmp.quant)
  tmp.dat$Date_sec <- as.numeric(tmp.dat$Date)
  tmp.ken <- kendallTrendTest(coarse ~ Date_sec, tmp.dat, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
  #multiply slope by year.sec to go from change/sec to %change/year
  tmp.slope <- tmp.ken$estimate[2] * days * 100
  tmp.p <- tmp.ken$p.value
  tmp.tau <- tmp.ken$estimate[1]
  tmp.intercept <- tmp.ken$estimate[3]
  tmp.ci <- tmp.ken$interval$limits * days * 100
  #normalize slope and confidence intervals by dividing by the median value for that site within the quantile
  tmp.normslope <- as.numeric(tmp.slope)/median(tmp.dat$coarse, na.rm = TRUE)
  tmp.normci <- tmp.ci/median(tmp.dat$coarse, na.rm = TRUE)
  #store all of these values in a temporary dataframe
  tmp.df <- data.frame("normslope" = tmp.normslope,
                       "pval" = tmp.p,
                       "normLCI" = tmp.normci[1],
                       "normUCI" = tmp.normci[2],
                       "q" = as.factor(q[i]))
  improve.slopes <- rbind(improve.slopes, tmp.df)
  print(paste0("finished ",q[i]))
}

ggplot(data = improve.slopes, aes(x = q, y = normslope)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=normLCI, ymax=normUCI), width=.2,
                position=position_dodge(0.05))

aero.slopes.r$network <- "AERONET"
improve.slopes$network <- "IMPROVE"

both.slopes <- rbind(aero.slopes.r,improve.slopes)

dodge <- position_dodge(0.5)
ggplot(data = both.slopes, aes(x = q, y = normslope, color = network)) +
  geom_point(position = dodge) +
  theme_bw() +
  geom_errorbar(aes(ymin=normLCI, ymax=normUCI), width=.2,
                position=dodge) +
  scale_color_manual(values = c("black","dark grey")) +
  xlab("Quantile") +
  ylab("% change/year") +
  labs(color = "Network")
