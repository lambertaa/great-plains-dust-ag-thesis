rm(list=ls())

library(ncdf4)
library(maps)
library(fields)
library(lubridate)
library(dplyr)
library(stringr)
library(RCurl)
#library(downloader)

#set working directory to download and export data to MISR_swath_data
setwd("/home/andylambert/MISR_swath_data")

#Create date vector to use in directory name
date.string <- seq(as.Date("2000/03/01"), as.Date("2018/12/31"), "days")
date.string <- as.character(date.string)
date.string <- str_replace_all(date.string,"[-]",".")


for (i in 1220:length(date.string)) {
  #i = 16
  #create directory string vector with date string and base url
  dir.string <- paste0("https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/",date.string[i],"/contents.html")
  
  #pull out filenames and keep only misr.nc filenames
  filenames <- getURL(dir.string, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose = TRUE)
  filenames = paste(dir.string, strsplit(filenames, "\r*\n")[[1]], sep = "")
  filenames <- sub('.*">',"",filenames)
  filenames <- str_extract(filenames,"[^<]+")
  filenames <- str_extract(filenames,"[^ ]+")
  filenames <- filenames[grepl('^MISR',filenames)]
  filenames <- filenames[grepl('\\.nc$',filenames)]
  
  #keep filenames only within desired paths (24 through 56)
  filename.path <- sub("_O.*",'',filenames)
  filename.path <- as.integer(sub(".*\\_P",'',filename.path))
  filenames <- filenames[filename.path <= 50 & filename.path >= 25]
  
  if (identical(filenames, character(0)) == TRUE) {
    next
    } else {
    for (j in 1:length(filenames)) {
      #j = 1
      #create string to be used as url for download
      download.string <- paste0("https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/",date.string[i],"/",filenames[j])
      date.dest <- str_replace_all(date.string[i],"[.]","_")
      dest <- paste0(date.dest,"_",filenames[j])
      
      #download file and save with same name as download file
      download.file(download.string, destfile = dest, method = "auto",
                    quiet = FALSE, mode="wb", cacheOK = TRUE)
      
      #setwd("/home/andylambert/MISR_swath_data")
      ncname <- dest
      #ncfname <- paste(ncname, ".nc", sep = "")
      
      
      # open a NetCDF file
      ncin <- nc_open(ncname)
      
      #get variables into dataframe
      data <- data.frame(
        'lat' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Latitude")),
        'long' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Longitude")),
        'elevation' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Elevation")),
        'year' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Year")),
        'month' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Month")),
        'day' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Day")),
        'hour' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Hour")),
        'minute' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Minute")),
        'ns_aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Nonspherical_Aerosol_Optical_Depth")),
        'aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Aerosol_Optical_Depth")),
        'aod_uncertainty' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Aerosol_Optical_Depth_Uncertainty")),
        'angstrom_550_860' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Angstrom_Exponent_550_860nm")),
        'abs_aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Absorption_Aerosol_Optical_Depth")),
        'small_aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Small_Mode_Aerosol_Optical_Depth")),
        'medium_aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Medium_Mode_Aerosol_Optical_Depth")),
        'large_aod' = c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Large_Mode_Aerosol_Optical_Depth"))
      )
      
      #remove NA obs
      data <- data[complete.cases(c(ncvar_get(ncin,"4.4_KM_PRODUCTS/Latitude"))),]
      
      data <- subset(data, lat >= 25 & lat <= 50 & long >= -130 & long <= -90 )
      
      data$time <- with(data, ymd_hm(paste(year, month, day, hour, minute, sep= ' ')))
      
      #output subsettted file as csv
      output.string <- paste0(str_extract(dest,"[^.]+"),".csv")
      write.csv(data,file = output.string)
      
      #delete old file
      file.remove(dest)
      print(paste0("Finished ",output.string)) 
    }
  }
}

