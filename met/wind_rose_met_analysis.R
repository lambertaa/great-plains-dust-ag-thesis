#clear environments
rm(list=ls())

#load packages
library("lubridate", lib.loc="/usr/local/lib/R/site-library")
library("dplyr", lib.loc="/usr/local/lib/R/site-library")
library("tidyr", lib.loc="/usr/local/lib/R/site-library")
library(sp)
library(maptools)
library(maps)
library(rgdal)
library(openair)
library(RColorBrewer)

#set working directory to where met data is stored
setwd("/home/andylambert/ag_met_data")

files <- list.files()

#set beaufort wind thresholds
beaufort <- c(0,0.3,1.5,3.3,5.5,8,10.8,13.9,13.9,17.2,20.7,24.5,28.4,32.6)


#make breaks for standard wind rose
rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)

#labels for each break - north is 0 and 360
rose_labs <- c(
  "North", "North-Northeast", "Northeast", "East-Northeast",
  "East", "East-Southeast", "Southeast", "South-Southeast",
  "South", "South-Southwest", "Southwest", "West-Southwest",
  "West", "West-Northwest", "Northwest", "North-Northwest",
  "North"
)

rose_labs_num <- seq(0,360,22.5)

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


radperdeg  <- 0.0174532925

all.stats <- NULL
for (i in 1:length(files)) {
  #i = 2
  df <- fread(files[i], skip = 10, na.strings = "") %>% slice(-1) #read in file, skipping metadata, and removing line following header
  
  #read in metadata
  con <- file(files[i], "r") #read in file
  lines <- readLines(con, n = 10)[c(7,8,10)] #store metadata for state and coords in lines
  lines <- gsub(".*: ","",lines) #preserve only important parts of string
  close(con) #close file
  
  # convert strings to numeric
  df$wind_speed_set_1 <- as.numeric(df$wind_speed_set_1)
  df$wind_direction_set_1 <- as.numeric(df$wind_direction_set_1)
  df$wind_gust_set_1 <- as.numeric(df$wind_gust_set_1)
  #add lat, long, and state
  df$latitude <- as.numeric(lines[1])
  df$longitude <- as.numeric(lines[2])
  df$state <- lines[3]
  #split datetime string and convert to posixct time - store as date
  datetimestrings <- strsplit(df$Date_Time, split = "T")
  date <- unlist(datetimestrings)[seq(from = 1, to = length(unlist(datetimestrings)), by = 2)]
  time <- unlist(datetimestrings)[seq(from = 2, to = length(unlist(datetimestrings))-1, by = 2)]
  time <- substr(time,1,nchar(time)-1)
  df$date <- as.POSIXct(paste0(date," ",time), tz = "UTC")
  #isolate year and month for monthly and annual analysis
  df$Year <- as.numeric(format(df$date, "%Y"))
  df$month <- factor(format(df$date, "%B"), levels = month.name)
  
  # #subset months
  df <- subset(df, month == "January" |
                  month == "February" |
                  #month == "March" |
                  #month == "April" |
                  #month == "May")# |
                  #month == "June" |
                  #month == "July" |
                  #month == "August") #|
                  #month == "September" |
                  #month == "October" |
                  #month == "November") #|
                  month == "December")
  
  #create dataframe only for higher wind events above set wind speed
  df.high <- df[which(df$wind_speed_set_1 >= 5.5),]
  #df.high <- df
  
  #bin observations for cardinal wind direction
  df.high$dir <- (tibble(df.high$wind_direction_set_1) %>%
                    mutate(
                      rose = cut(
                        df.high$wind_direction_set_1,
                        #wd,
                        breaks = rose_breaks,
                        labels = rose_labs,
                        right = FALSE,
                        include.lowest = TRUE
                      )
                    ))[,2]
  
  #bin observations for cardinal wind direction
  df.high$dirnum <- (tibble(df.high$wind_direction_set_1) %>%
                       mutate(
                         rose = cut(
                           df.high$wind_direction_set_1,
                           #wd,
                           breaks = rose_breaks,
                           labels = rose_labs_num,
                           right = FALSE,
                           include.lowest = TRUE
                         )
                       ))[,2]
  
  df.high$dirnum <- as.numeric(levels(df.high$dirnum))[df.high$dirnum]
  df.high$dirnum[which(df.high$dirnum == 0)] <- 360
  
  #get stats grouped by cardinal wind direction
  df.stats <- df.high %>%
    group_by(dir) %>%
    summarize_at(vars(wind_speed_set_1, wind_gust_set_1, dirnum), funs(sum(!is.na(.)),mean(., na.rm = T)))
  
  df.stats$latitude <- df$latitude[1]
  df.stats$longitude <- df$longitude[1]
  df.stats$state <- df$state[1]
  
  totalwind <- sum(df.stats$dirnum_sum)
  
  df.stats <- df.stats[order(df.stats$wind_speed_set_1_sum, decreasing = TRUE),]
  df.stats <- df.stats[1:3,]
  df.stats$dirfrac <- df.stats$dirnum_sum/totalwind
  
  df.stats$ycom <- cos(df.stats$dirnum_mean*radperdeg)*-1
  df.stats$xcom <- sin(df.stats$dirnum_mean*radperdeg)*-1
  df.stats$rank <- rank(df.stats$wind_speed_set_1_sum) * (1/3)
  
  all.stats <- rbind(all.stats, df.stats)
  
  #plot
  # if (i == 1) {
  #   p <- ggplot() + geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  #     coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3) +
  #     geom_segment(aes(x = df.stats$longitude, y = df.stats$latitude, 
  #                      xend = df.stats$longitude + (df.stats$xcom*df.stats$rank), 
  #                      yend = df.stats$latitude + (df.stats$ycom*df.stats$rank)),
  #                  arrow = arrow(length = unit(0.2, "cm"))) +
  #     theme_bw() +
  #     ylab("Latitude") +
  #     xlab("Longitude") 
  # } else {
  #   p <- p +
  #     geom_segment(aes(x = df.stats$longitude, y = df.stats$latitude, 
  #                      xend = df.stats$longitude + (df.stats$xcom*df.stats$rank), 
  #                      yend = df.stats$latitude + (df.stats$ycom*df.stats$rank)),
  #                  arrow = arrow(length = unit(0.2, "cm")))
  # }
  print(paste0("finished ",i))
  #print(p)
  #dev.off()
}

ggplot() + geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill=NA, color="black") +
      coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3) +
      # geom_segment(aes(x = all.stats$longitude, y = all.stats$latitude,
      #                  xend = all.stats$longitude + (all.stats$xcom*all.stats$dirfrac*2.5),
      #                  yend = all.stats$latitude + (all.stats$ycom*all.stats$dirfrac*2.5)),
  geom_segment(aes(x = longitude, y = latitude,
                   xend = longitude + (xcom*dirfrac*6.5),
                   yend = latitude + (ycom*dirfrac*6.5)),
                   data = all.stats,
                   #linetype = "dirfrac",
                       #xend = all.stats$longitude + (all.stats$xcom*all.stats$rank),
                       #yend = all.stats$latitude + (all.stats$ycom*all.stats$rank)),
                   arrow = arrow(length = unit(0.15, "cm")),
               color = "grey20") +
      theme_bw() +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
      theme(plot.margin=unit(c(0,0,0,0.5),"cm")) +
      ylab("Latitude") +
      xlab("Longitude")
















wwindRose(mydata = df, ws = "wind_speed_set_1", wd = "wind_direction_set_1", paddle = FALSE, type = "month")
#windRose(mydata = df, ws = "wind_speed_set_1", wd = "wind_direction_set_1", paddle = FALSE, type = "month", breaks = beaufort)

#get wind rose function
source("/home/andylambert/wind_rose_function.R")

#plot wind rose with function
p <- plot.windrose(data = df,
              spd = "wind_speed_set_1",
              dir = "wind_direction_set_1")
              #spdseq = c(0,0.3,1.5,3.3,5.5,8,10.8,13.9,13.9,17.2,20.7,24.5,28.4,32.6))
#monthly
p.2 <- p + facet_wrap(~month,ncol = 3)
p.2 <- p.2 + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank())

#test wind_dir
wind_dir <- tibble(
  #wd = (0:16 / 16) * 360
  wd = 15
)

#wind_dir %>%
#bin observations for cardinal wind direction
df.high$dir <- (tibble(df.high$wind_direction_set_1) %>%
  mutate(
    rose = cut(
      df.high$wind_direction_set_1,
      #wd,
      breaks = rose_breaks,
      labels = rose_labs,
      right = FALSE,
      include.lowest = TRUE
    )
  ))[,2]

#bin observations for cardinal wind direction
df.high$dirnum <- (tibble(df.high$wind_direction_set_1) %>%
                  mutate(
                    rose = cut(
                      df.high$wind_direction_set_1,
                      #wd,
                      breaks = rose_breaks,
                      labels = rose_labs_num,
                      right = FALSE,
                      include.lowest = TRUE
                    )
                  ))[,2]

df.high$dirnum <- as.numeric(levels(df.high$dirnum))[df.high$dirnum]
df.high$dirnum[which(df.high$dirnum == 0)] <- 360

#get stats grouped by cardinal wind direction
df.stats <- df.high %>%
  group_by(dir) %>%
  summarize_at(vars(wind_speed_set_1, wind_gust_set_1, dirnum), funs(sum(!is.na(.)),mean(., na.rm = T)))

df.stats$latitude <- df$latitude[1]
df.stats$longitude <- df$longitude[1]
df.stats$state <- df$state[1]

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

df.stats <- df.stats[order(df.stats$wind_speed_set_1_sum, decreasing = TRUE),]
df.stats <- df.stats[1:3,]

radperdeg  <- 0.0174532925

df.stats$ycom <- cos(df.stats$dirnum_mean*radperdeg)*-1
df.stats$xcom <- sin(df.stats$dirnum_mean*radperdeg)*-1
df.stats$rank <- rank(df.stats$wind_speed_set_1_sum) * (1/3)

#plot
p <- ggplot() + geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill=NA, color="black") +
  coord_fixed(xlim = c(-104.5, -89),ylim = c(34,49),ratio = 1.3) +
  geom_segment(aes(x = df.stats$longitude, y = df.stats$latitude, 
                   xend = df.stats$longitude + (df.stats$xcom*df.stats$rank), 
                   yend = df.stats$latitude + (df.stats$ycom*df.stats$rank)),
               arrow = arrow(length = unit(0.2, "cm"))) +
  theme_bw() +
  ylab("Latitude") +
  xlab("Longitude")
