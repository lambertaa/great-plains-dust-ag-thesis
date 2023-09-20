
rm(list=ls())

library(ncdf4)
library(maps)
library(fields)
library(lubridate)
library(dplyr)
library(stringr)
library(RCurl)
library(chron)
library(lattice)
library(RColorBrewer)

path <- "https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL3MAEN.004/"
#prodname <- "MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031"

#Create date vector to use in directory name
date.string <- seq(as.Date("2000/01/01"), as.Date("2018/12/01"), "months")
date.string <- as.character(date.string)
date.string <- str_replace_all(date.string,"[-]",".")


years <- as.character(2000:2018)
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
prodyear <- paste0(years,"_F08_0031")
#prodmonth <- paste0("MISR_AM1_CGAS_0_5_DEG_",months)
#prodname <- data.frame(matrix(NA,nrow = length(years)*length(months),ncol = 1))
prodname <- NULL
for (k in 1:length(prodyear)) {
  #k = 2
  prodname <- append(prodname,paste0("MISR_AM1_CGAS_0_5_DEG_",months,"_",prodyear[k]), after = length(prodname))
}


url <- paste0(path,date.string,"/",prodname,".nc.nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")

start <- 2

url <- url[start:length(url)]

fileConn<-file("MISR_url.txt")
writeLines(url, fileConn)
close(fileConn)
