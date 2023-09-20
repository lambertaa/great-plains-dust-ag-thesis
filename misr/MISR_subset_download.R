
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

path <- "https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL3DAEN.004/"
#prodname <- "MISR_AM1_CGAS_0_5_DEG_FEB_2000_F08_0031"

#Create date vector to use in directory name
date.string <- seq(as.Date("2000/02/25"), as.Date("2018/12/31"), "days")
date.string.alt <- format(date.string,format = "%b-%d-%Y")
date.string.alt <- str_replace_all(date.string.alt,"[-]","_")
date.string.alt <- toupper(date.string.alt)
date.string <- as.character(date.string)
date.string <- str_replace_all(date.string,"[-]",".")



filename <- paste0(date.string,"/MISR_AM1_CGAS_0_5_DEG_",date.string.alt,"_F08_0031.nc")
filepath <- paste0(path,filename)
filepathsub <- paste0(filepath,".nc?Optical_depth_average[0:1:8][0:1:3][0:1:5][0:1:0][230:1:280][100:1:190]")

fileConn<-file("MISR_daily_url.txt")
writeLines(filepathsub, fileConn)
close(fileConn)
#prodyear <- paste0(years,"_F08_0031")
#prodmonth <- paste0("MISR_AM1_CGAS_0_5_DEG_",months)
#prodname <- data.frame(matrix(NA,nrow = length(years)*length(months),ncol = 1))
prodname <- NULL
# for (i in 1:length(years)) {
#   #k = 2
#   prodname <- append(prodname,paste0("MISR_AM1_CGAS_0_5_DEG_",months,"_",prodyear[k]), after = length(prodname))
# }


url <- paste0(path,date.string,"/",prodname,".nc")

start <- 2

url <- url[start:length(url)]
date.string <- date.string[start:length(date.string)]
prodname <- prodname[start:length(prodname)]


for (j in 1:length(url)) {
  #j = 1
  download.string <- url[j]
  
  dest <- paste0("MISR_data_subset/",date.string[j],"_",prodname[j],".nc")
  
  download.file(download.string, destfile = dest, method = "auto",
                quiet = FALSE, mode="wb", cacheOK = TRUE)
  
  
  ncin <- nc_open("MISR_data_subset/MISR_test.nc")
  dname <- "Optical_depth_average"
  ncvar_get(ncin,"ParticleType_labels")
  
  ptype <- ncvar_get(ncin,"ParticleType_labels")
  aodlabels <- ncvar_get(ncin,"OpticalDepth_labels")
  bandlabels <- ncvar_get(ncin,"Band_labels")
  
  lon <- ncvar_get(ncin,"longitude_bounds")
  nlon <- dim(lon)
  head(lon)
  
  lat <- ncvar_get(ncin,"latitude_bounds")
  nlat <- dim(lat)
  head(lat)
  
  print(c(nlon,nlat))
  
  time <- ncvar_get(ncin,"time_bounds")
  tunits <- ncatt_get(ncin,"time_bounds","units")
  nt <- dim(time)
  
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  
  # get temperature
  aod_array <- ncvar_get(ncin, dname)
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(aod_array)
  
  # convert time -- split the time units string into fields
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  timenew <- chron(mean(time),origin=c(tmonth, tday, tyear))
  
  
  # replace netCDF fill values with NA's
  aod_array[aod_array==fillvalue$value] <- NA
  
  # get a single slice or layer (January)
  m <- 6
  n <- 1
  
  
  # # quick map
  # image(colMeans(lon),colMeans(lat),tmp_slice, col=rev(brewer.pal(10,"RdBu")))
  # 
  # # levelplot of the slice
  # grid <- expand.grid(lon=colMeans(lon), lat=colMeans(lat))
  # cutpts <- c(0,0.01,0.02,0.03,0.04,0.05,0.1,0.5,1,2)
  # levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  #           col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- as.matrix(expand.grid(colMeans(lon),colMeans(lat)))
  dim(lonlat)
  
  for (i in 1:length(bandlabels)) {
    #i = 1
    band <- strsplit(bandlabels[i]," ")[[1]][1]
    tmp_slice <- aod_array[,,m,i,n]
    
    # vector of `tmp` values
    tmp_vec <- as.vector(tmp_slice)
    length(tmp_vec)
    
    # create dataframe and add names
    if (i == 1) {
      tmp_df <- data.frame(cbind(lonlat,tmp_vec))
      names(tmp_df) <- c("lon","lat",paste0(dname,band, sep="_"))
    } else {
      tmp_df <- data.frame(cbind(tmp_df,tmp_vec))
      names(tmp_df)[i+2] <- paste(dname,band, sep="_")
    }
    head(na.omit(tmp_df), 10)
  }
  
  
  # set path and filename
  #csvpath <- "MISR_data_subset"
  #csvname <- "cru_tmp_1.csv"
  csvfile <- paste0("MISR_data_subset/",date.string[j],"_",prodname[j],".csv")
  write.table(na.omit(tmp_df),csvfile, row.names=TRUE, sep=",")
  
  file.remove(dest)
}
