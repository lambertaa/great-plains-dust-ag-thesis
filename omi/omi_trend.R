#get quantile for each year at each gridpoint and trends in quantile

#' @param long longtiude of my variable
#' @param lat latitude of my variable
#' @param years 
#'
#'
#'

blah <- function(long,lat,years) {
  for (j in 1:length(long)) {
    for (k in 1:length(lat)) {
      #j = 5
      #k = 15
      point.quant.obs <- NULL
      for (i in 1:length(years)) {
        #i = 1
        tmp.year <- years[i]
        ind.year <- which(year(date) == tmp.year)
        tmp.dat.year <- ai.array[j,k,ind.year]
        tmp.date <- date[ind.year]
        tmp.quant <- quantile(tmp.dat.year, probs = q.sel, na.rm = TRUE)
        tmp.quant.obs <- tmp.dat.year[tmp.dat.year >= tmp.quant]
        tmp.date <- tmp.date[tmp.dat.year >= tmp.quant]
        tmp.df <- data.frame("date" = tmp.date, "ai_quant" = tmp.quant.obs)
        point.quant.obs <- rbind(point.quant.obs, tmp.df)
        point.quant.obs <- point.quant.obs[complete.cases(point.quant.obs), ]
        #converts to number of days since 1970-01-01
        point.quant.obs$date <- as.numeric(point.quant.obs$date)
        #colnames(site.quant.obs)[colnames(site.quant.obs)=="tmp.quant.obs"] <- "aod_quant"
        # tmp.quant <- apply(tmp.dat.year, c(1,2), quantile, probs = 0.75, na.rm = TRUE)
        # tmp.quant.obs <- NULL
        # print(paste0("finished ",tmp.year))
      }
      if (nrow(point.quant.obs) >= 3) {
        tmp.ken <- kendallTrendTest(ai_quant ~ date, point.quant.obs, na.action = na.pass, ci.slope = TRUE, conf.level = 0.95)
        tmp.slope <- tmp.ken$estimate[2] * 365
        tmp.p <- tmp.ken$p.value
        ai.ind <- which(omi.trend.df$lon == long[j] & omi.trend.df$lat == lat[k])
        omi.trend.df[ai.ind,3] <- tmp.slope
        omi.trend.df[ai.ind,4] <- tmp.p
        omi.trend.df[ai.ind,5] <- (tmp.slope/median(point.quant.obs$ai_quant, na.rm = TRUE))*100
      } else {
        ai.ind <- which(omi.trend.df$lon == long[j] & omi.trend.df$lat == lat[k])
        omi.trend.df[ai.ind,3] <- NA
        omi.trend.df[ai.ind,4] <- NA
        omi.trend.df[ai.ind,5] <- NA
      }
      print(paste0('finished ', long[j]," ",lat[k]))
    }
  }
}
