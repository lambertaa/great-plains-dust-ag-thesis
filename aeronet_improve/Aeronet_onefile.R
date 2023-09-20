
rm(list=ls())

library(lubridate)
library(dplyr)
library(data.table)
#library(tidyverse)

#data <- read.table("~/AERONET/19980101_20191231_Table_Mountain.lev20", skip = 6, header = T, sep = ",")

data <- fread("~/AERONET/19980101_20191231_Table_Mountain.lev20", skip = 6, stringsAsFactors = F, check.names = T)
data[data == -999] <- NA
data$time_utc <- as.POSIXct(paste(as.Date(data$Date.dd.mm.yyyy., format="%d:%m:%Y"), data$Time.hh.mm.ss.))

df.sub <- data %>%
  select(time_utc, 
         AOD_1020nm:AOD_340nm, 
         X440.870_Angstrom_Exponent:X440.675_Angstrom_Exponent.Polar.,
         AERONET_Site_Name:Site_Elevation.m.) %>%
  group_by(time_utc = cut(time_utc, breaks="30 min")) %>%
  summarise_at(vars(-AERONET_Site_Name, -time_utc), funs(mean(., na.rm=T))) 

df.sub$time_utc <- as.POSIXct(df.sub$time_utc)
plot(df.sub$time_utc, df.sub$X440.870_Angstrom_Exponent, type='o')
boxp <- boxplot(df.sub$X440.870_Angstrom_Exponent)
sd(df.sub$X440.870_Angstrom_Exponent, na.rm=T) * 4
hist(df.sub$X440.870_Angstrom_Exponent)

na <- df.sub %>% 
  mutate(year = year(time_utc)) %>%
  group_by(year) %>%
  #summarise_at(vars(X440.870_Angstrom_Exponent), ~sum(!is.na(.)))
  summarise_all(~sum(!is.na(.)))

year_dif <- tail(year(df.sub$time_utc), -1) - head(year(df.sub$time_utc), -1)
max_dif <- which.max(year_dif)


if (max(year_dif) > 2) {
  test <- df.sub[-c(1:max_dif),]
}

plot(test$time_utc,test$X440.870_Angstrom_Exponent, type='o')
