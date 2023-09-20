site.quant.reg <- function(df,network,sitevec,time.conv,var, R.num) {
  #create empty df
  network.quant.reg <- NULL
  for (i in 1:length(sitevec)) {
    #i = 3
    tmp.site <- subset(df, SiteName == sitevec[i]) #subset site
    tmp.site <- tmp.site[!is.na(tmp.site[[var]]),] #remove rows with NA in variable
    
    if (nrow(tmp.site) == 0) {
      #store in tmp df
      tmp.df <- data.frame("sitename" = sitevec[i],
                           "lat" = tmp.site$Latitude[1],
                           "lon" = tmp.site$Longitude[1],
                           "slope" = NA,
                           "normslope" = NA,
                           "lowerci" = NA,
                           "upperci" = NA,
                           "pval" = NA)
    } else {
      tmp.qr <- rq(get(var) ~ datenum, tau=0.9, data = tmp.site)
      if (network == "AERONET") {
        qr.summ <- summary(tmp.qr, se = 'boot', bsmethod = 'xy', R = R.num, mofn = round(0.75*nrow(tmp.site)))
        rqboot <- boot.rq(cbind(1,tmp.site$datenum),tmp.site[[var]],tau=0.9, R = R.num, na.rm = T, bsmethod = "xy", mofn = round(0.75*nrow(tmp.site)))
      } else {
        qr.summ <- summary(tmp.qr, se='boot', bsmethod = "xy", R = R.num)
        rqboot <- boot.rq(cbind(1,tmp.site$datenum),tmp.site[[var]],tau=0.9, R = R.num, na.rm = T, bsmethod = "xy") 
      }
      tmp.ci <- t(apply(rqboot$B, 2, quantile, c(0.025,0.975)))
      #pull out parameters from quantile regression summary
      tmp.slope <- qr.summ$coefficients[2,1] * time.conv
      tmp.normslope <- (tmp.slope/quantile(tmp.site[[var]], c(0.9)))*100
      tmp.p <- qr.summ$coefficients[2,4]
      tmp.normci <- c((tmp.ci[2,1]/quantile(tmp.site[[var]], c(0.9))),
                      (tmp.ci[2,2]/quantile(tmp.site[[var]], c(0.9))))
      #store in tmp df
      tmp.df <- data.frame("sitename" = sitevec[i],
                           "lat" = tmp.site$Latitude[1],
                           "lon" = tmp.site$Longitude[1],
                           "slope" = tmp.slope,
                           "normslope" = tmp.normslope,
                           "lowerci" = tmp.normci[1],
                           "upperci" = tmp.normci[2],
                           "pval" = tmp.p)
    }
    #add to df
    network.quant.reg <- rbind(network.quant.reg,tmp.df)
    print(paste0("finished ",sitevec[i]))
  }
  #reset rownames
  rownames(network.quant.reg) <- 1:length(sitevec)
  #store in workspace
  network.quant.reg <<- network.quant.reg
  return(network.quant.reg)
}


maptrends <- function(trends.df, long.ext, lat.ext, states.sel) {
  ggplot() + geom_polygon(data = states.sel, aes(x=long,y=lat, group = group), fill=NA, color="black") +
    coord_fixed(xlim = long.ext, ylim = lat.ext,ratio = 1.3) +
    #the lines below were used below were used for mapping different focus regions
    #geom_polygon(data = states.ag, aes(x=long,y=lat, group = group), fill="green", size = 3, alpha = 0.5) +
    #geom_polygon(data = states.oil, aes(x=long,y=lat, group = group), fill="orange", size = 3, alpha = 0.5)
  #size depends on the absolute value of the normalized slope, color on positive or negative, fill is the same, shape depends on statistical significance
  #tmp.plot <<- tmp.plot + geom_point(data=trends.df, aes(x=lon, 
    geom_point(data=trends.df, aes(x=lon, 
                                   y = lat,
                                   size = abs(normslope),
                                   color = ifelse(normslope > 0,
                                           "Increasing","Decreasing"),
                                   shape = ss,
                                   fill = ifelse(normslope > 0,
                                          "Increasing","Decreasing")),
                                   group = FALSE) +
    #blue is decreasing, red is increasing
    scale_color_manual(values = c("Blue","Red")) +
    #1 is for open circle (not significant), 21 is filled circle (significant)
    scale_shape_manual(values = c(1,21), labels = c("> 0.05","< 0.05")) +
    scale_fill_manual(values = c("Blue","Red"), 
                      guide = FALSE) +
    #setting the range of the possible sizes of circles
    scale_size_continuous(range = c(1,10)) +
    #titles for each legend
    labs(size = "%/year", 
         color = "Trend",
         shape = "P-Val") +
    #have to override aesthetics for legend to manually fill shape 21 for statistical significance
    guides(fill = guide_legend(override.aes = list(shape = 21)),
           shape = guide_legend(override.aes = list(fill = "black"))) +
    #now have to get rid of fill legend
    guides(fill = FALSE) +
    #set theme to simple black and white
    theme_bw() + 
    theme(legend.text = element_text(size=12)) +
    xlab("Longitude") +
    ylab("Latitude") +
    #ggtitle(paste0("Trends in ", q.string, "th Quantile ", var.string)) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.title = element_text(size=14), title = element_text(size=14)) +
    theme(plot.margin=unit(c(0,0,0,0.5),"cm")) 
  #tmp.plot
}





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
  return(network.monthly.trends)
}