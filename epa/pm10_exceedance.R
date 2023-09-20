
#clear environments
rm(list=ls())

#load packages
library("lubridate", lib.loc="/usr/local/lib/R/site-library")
library("dplyr", lib.loc="/usr/local/lib/R/site-library")
library(readr)
library(plyr)
library(tidyverse)

#set working directory to where pm10 exceedance data is stored
setwd("~/epa_pm10_annual")

#read in all files and store in data
#data <-
#  list.files() %>%
#  map_df(~fread(.))

file_names <- list.files()
data <- do.call(rbind,lapply(file_names,read.csv))

#remove all additional monitors at same site by only including POC 1 monitors
data.poc1 <- subset(data, POC == 1)

#investigate specific counties/cities
dat.state <- subset(data.poc1, State == "NE")
state.max <- dat.state[which.max(dat.state$`Actual Exceedances`),]
site.sum <- dat.state %>%
  group_by(`Site ID`) %>%
  summarize_at(vars(`Actual Exceedances`,`Estimated Exceedances`), ~sum(., na.rm = T))
dat.site <- subset(data.poc1, `Site ID` == 490570002)
dat.c <- subset(data.poc1, City == "Aberdeen")

test <- dat.state %>%
  group_by(year) %>%
  summarize_at(vars(`Actual Exceedances`,`Estimated Exceedances`), ~sum(., na.rm = T))
ggplot(data = test, aes(x = year, y = `Actual Exceedances`)) +
  geom_line()



#group by state and year and average estimated exceedances across all sites within each state
dat.mean.exceed <- data.poc1 %>%
  group_by(State,year) %>%
  summarise_at(vars(`Estimated Exceedances`), ~mean(`Estimated Exceedances`,na.rm=T))

#set up regions
MW <- c("ND", "SD" , "MN" , "KS" , "NE" , "IA" , "MO")
SW <- c("AZ" , "NM" , "UT" , "CO")
NW <- c("WA" , "OR" , "ID" , "MT" , "WY")
W <- c("CA", "NV")
S <- c("TX","OK","LA","AR")

#Use region to make a pattern vector
pattern <- paste(SW, collapse="|")
#Subset data by selected region
dat.mean.mw <- dat.mean.exceed[grepl(pattern, dat.mean.exceed$State),]

#plot average number of exceedance days per year for region, each different colored line reps a state
ggplot(dat.mean.mw, aes(x=year,y = `Estimated Exceedances`, group = State)) +
  geom_line(aes(color = State)) +
  geom_point(aes(color = State)) +
  theme_bw() +
  ylab("Days #") +
  xlab("Year") +
  guides(color = guide_legend(title="State")) +
  ggtitle("South PM10 Average Exeedance Days by State")

#get average values in first decade
dat.mean.first <- subset(dat.mean.exceed, year < 2010)
dat.mean.first.mean <- dat.mean.first %>%
  group_by(State) %>%
  summarize_at(vars(`Estimated Exceedances`), ~mean(`Estimated Exceedances`, na.rm = T))

#get average values in second decade
dat.mean.sec <- subset(dat.mean.exceed, year >= 2010)
dat.mean.sec.mean <- dat.mean.sec %>%
  group_by(State) %>%
  summarize_at(vars(`Estimated Exceedances`), ~mean(`Estimated Exceedances`, na.rm = T))

#subset second decade df by common states with first decade
common.sec <- dat.mean.sec.mean[(dat.mean.sec.mean$State %in% dat.mean.first.mean$State),]
#subset first decade df by common states with second decade
common.first <- dat.mean.first.mean[(dat.mean.first.mean$State %in% dat.mean.sec.mean$State),]

#calculate the difference in average number of exceedance days by state between first and second decades
common.dif <- data.frame("State" = common.sec$State, "Dif" = c(common.sec$`Estimated Exceedances` - common.first$`Estimated Exceedances`))



#check exceedance day correlation with well construction
dat.state <- subset(data.poc1, State == "WY")


#set directory to oil_gas_files
# setwd("/home/andylambert/oil_gas_files")
# #read in csv file
# data <- fread("2000_2018_oil_gas_states_sel.CSV")
# 
# #read into dataframe, keep well, supddate, state, status, type, lat, and lon variables
# data.df <- data.frame("well" == as.character(data$`Well Name`), 
#                       "spuddate" = data$`Spud Date`, 
#                       "state" = data$State,
#                       "status" = data$`Well Status`,
#                       "type" = as.character(data$`Production Type`), 
#                       "lat" = data$`Surface Hole Latitude (WGS84)`, 
#                       "lon" = data$`Surface Hole Longitude (WGS84)`)
# 
# #remove wells that have not yet disturbed land surface
# data.sub <- data.df[!(data.df$status == "CANCELLED" | 
#                         data.df$status == "EXPIRED PERMIT" | 
#                         data.df$status == "PERMITTED" | 
#                         data.df$status == "UNKNOWN"),]
# 
# #count number of wells constructed per year (by spud date) in each state
# wells.year <- data.sub %>%
#   group_by(year(spuddate), state) %>%
#   summarise_at(vars(lat), ~sum(!is.nan(.)))
# 
# colnames(wells.year)[3] <- "yearcount"
# wells.year <- wells.year[!(wells.year$state == "SD" | wells.year$state == "NE"),]
# 
# #dat.site <- subset(dat.state, `Site ID` == 560010800)
# wells.state <- data.frame(subset(wells.year, state == "WY"))
# colnames(wells.state)[1] <- "year"
# 
# 
# site.id <- unique(dat.state$`Site ID`)
# tmp.state.cor.df <- NULL
# for (i in 1:length(site.id)) {
# #  i = 1
#   dat.site <- subset(dat.state, `Site ID` == site.id[i])
#   dat.merge <- merge(wells.state, dat.site, by = "year", all.x = TRUE, all.y = TRUE)
#   tmp.cor <- cor.test(dat.merge$`Actual Exceedances`, dat.merge$yearcount)
#   tmp.p <- tmp.cor$p.value
#   tmp.c <- tmp.cor$estimate
#   tmp.df <- data.frame("state" = dat.merge$state[1],
#                        "county" = dat.merge$County[1],
#                        "city" = dat.merge$City[1],
#                        "siteid" = dat.merge$`Site ID`[1],
#                        "cc" = tmp.c,
#                        "pval" = tmp.p)
#   tmp.state.cor.df <- rbind(tmp.state.cor.df, tmp.df)
# }
