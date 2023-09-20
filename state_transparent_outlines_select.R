
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

#get map for states to plot
states <- map_data("state")

#get selected states
states.sel <- subset(states, region == "wyoming" | 
                       region == "colorado" | 
                       region == "montana" | 
                       region == "north dakota" |
                       region == "south dakota" |
                       region == "nebraska" |
                       region == "kansas" |
                       region == "oklahoma" |
                       #region == "arkansas" |
                       region == "missouri" |
                       region == "iowa" |
                       region == "minnesota")

p <- ggplot(data = states.sel) + geom_polygon(aes(x=long,y=lat, group = group), fill=NA, color="yellow", size = 2) +
  coord_fixed(xlim = c(-115.5, -90),ylim = c(33.25,49),ratio = 1.3) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(),
    axis.text.y = element_blank(), 
    axis.title = element_blank()
  )
p
setwd("/home/andylambert/grad_deliverables")

ggsave(p, filename = "states_sel_all_transparent.png", bg = "transparent")
