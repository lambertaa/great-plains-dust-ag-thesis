
rm(list = ls())

library(gdalUtils)
library(rgdal)
library(raster)
library(RColorBrewer)
library(ggplot2)
library(maps)

setwd("/home/andylambert")
path <- "MODISdata/MOD08_D3/"
year <- list.files(path)
year <- year[21]
filedirs <- list.dirs(paste0(path,year))
filedirs <- filedirs[2:length(filedirs)]
#filedirs <- filedirs[-c(33)]

# gdal_info <- GDALinfo("MODISdata/MOD08_D3/2000/055/MOD08_D3.A2000055.061.2017276160246.hdf",returnScaleOffset=FALSE)
# subdataset_metadata <- attr(gdal_info, "subdsmdata")
# length(subdataset_metadata) 
# 
# 
# evi_subdataset <- subdataset_metadata[grep("250m 16 days EVI", subdataset_metadata)[1]]
# evi_subdataset_name <- sapply(strsplit(evi_subdataset, "="), "[[", 2)
# 


for (i in 1:length(filedirs)) {
  i = 1
  filename <- list.files(filedirs[i])
  sds <- get_subdatasets(paste0(filedirs[i],"/",filename))

  gdal_translate(sds[32],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_ocean_land_aod_mean_550.tif"))
  gdal_translate(sds[38],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_land_aod_mean_3bands.tif"))
  gdal_translate(sds[53],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_deepblue_land_aod_mean_3bands.tif"))
  gdal_translate(sds[58],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_deepblue_land_aod_mean_550.tif"))
  gdal_translate(sds[63],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_deepblue_land_ang_mean.tif"))
  gdal_translate(sds[78],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_darktarget_deepblue_combo_aod_mean_550.tif"))
  gdal_translate(sds[83],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_ocean_aod_mean_7bandave.tif"))
  gdal_translate(sds[91],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_ocean_fine_aod_mean_7bands.tif"))
  gdal_translate(sds[107],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_ocean_model_aod_mean_ptype.tif"))
}

# for (j in 1:length(year)) {
#   j = 1
#   tmp.year <- year[j]
#   tmp.filedirs <- list.dirs(paste0(path,tmp.year))
#   tmp.filedirs <- tmp.filedirs[2:length(tmp.filedirs)]
#   for (i in 1:length(filedirs)) {
#     i = 1
#     filename <- list.files(tmp.filedirs[i])
#     sds <- get_subdatasets(paste0(tmp.filedirs[i],"/",filename))
#     
#     gdal_translate(sds[32],dst_dataset = paste0("MODIS_tif/",year,"/",strsplit(filename, ".hdf")[[1]],"_ocean_land_aod_mean_550.tif"))
#   }
# }



# rast <- raster("MODIS_tif/MOD08_D3.A2000055.061.2017276160246_ocean_land_aod_mean.tif")
# rgb <- brick("MODIS_tif/MOD08_D3.A2000055.061.2017276160246_land_aod_mean.tif")
# plotRGB((rgb*0.00100000004749745), 3,2,1)
# plot(rast)
# 
# #scale factor = 0.00100000004749745
# 
# r.pts <- rasterToPoints(rast, spatial = TRUE)
# latlon <- data.frame(coordinates(r.pts))
# latlontest <- coordinates(r.pts)
# 
# #df <- data.frame("lon" = latlon$x, "lat" = latlon$y, "aod" = values(rast))
# 
# #image(latlon$x,latlon$y,values(rast), col=rev(brewer.pal(10,"RdBu")))
# 
# test <- as.data.frame(rast, xy = T)
# colnames(test) <- c("lon","lat","aod")
# 
# states <- map_data("state")
# 
# ggplot() + geom_tile(data = test, aes(x = lon, y = lat, fill = aod)) +
#   #scale_fill_manual(values = setNames(colors, levs))
#   #scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", na.value = "gray") +
#   theme_bw() + 
#   geom_polygon(data = states, aes(x=long,y=lat, group = group), fill=NA, color="black") +
#   coord_fixed(xlim = c(-130, -85),ylim = c(25,50),ratio = 1.3) +
#   labs(fill = "%/year" )+
#   xlab("Longitude") +
#   ylab("Latitude") + 
#   theme(legend.text = element_text(size=12)) +
#   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size=14), title = element_text(size=14)) +
#   ggtitle(paste0("Trends in 75th Quantile Non-Spherical AOD (865 nm) 2000-2016"))
