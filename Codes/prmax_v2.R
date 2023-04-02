library(circular)
library(stats)
library(raster)
library(reshape2)
#library(tidyverse)
library(Metrics)
library(wrapr)
library(data.table)
library(dplyr)
# library(scales)
library(rlist)
library(ggplot2)

setwd("~/Projekt_22/data_Precip")

#Load basics.Data to initialize
load("~/Projekt_22/data_Precip/basics.RData")

#Calculate average prmax and std. over all hist_models (48), ensemble members and years

sequence <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
              38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)

if (exists("mean_1") == FALSE){
  for (i in sequence){  # Problem with 20 
    print(i)
    load(gsub(" ", "", (paste("./prec/hist_",model_his[i],".prmax.RData"))))
    assign(paste0("mean_",             # Save Mittelwerte of every hist_modell
                  i),
           apply(prmax, c(1,2), mean))
    
    assign(paste0("std_",             # Save Std. of every hist_modell
                  i),
           apply(prmax, c(1,2), sd))
}
}

obvs_data <- c("CHIRPS", "ERA5", "GPCC", "GPCP", "PERSIANN", "REGEN")

#Create a for-loop over pbservational data so it saves all images automatically
for (names in obvs_data){
#Calculate average prmax and std. over all 5 obsv_dataset and save it in variable
  print(names)
  load(gsub(" ", "", (paste("./Rdata/",names,".prmax.RData"))))
  
  lis_diff <- list()
  lis_sd <- list()
  
  if (names == "ERA5"){
    prmax <- prmax*1000
  }
  if (names == "REGEN"){
    prmax <- prmax*86400
  }
  
  mean_obvs <- apply(prmax, c(1,2), mean)
  std_obvs <- apply(prmax, c(1,2), sd)
  lat <- lat
  lon <- lon
  #Change 0 values to NA values if necessary
  mean_obvs[mean_obvs == 0] = NA
  std_obvs[std_obvs == 0] = NA
  
  #Regrid observational data to (-180, 180) and (-90, 90)
  lat.new <- seq(-90, 90, 1)
  lon.new <- seq(-180, 180, 1)
  
  # Load lat.old and lon.old 
  lat.old <- lat
  lon.old <- lon
  lon.old <- lapply(lon.old, function(x) ifelse(x > 180, -360 + x, x))
  prmax.old <- mean_obvs
  sd.old <- std_obvs
  
  ######Convert to matrix to compare - Security check##### 
  #dimnames(prmax.old) = list(lon = lon.old, lat = lat.old)
  #old.df <- melt(prmax.old)
  #head(old.df)
  
  #Try with approach from Patrick
  prmax.new.obvs = matrix(NA, nrow=length(lon.new),ncol=length(lat.new))
  sd.new.obvs =  matrix(NA, nrow=length(lon.new),ncol=length(lat.new))
  
  for (ii in 1:length(lon.new)) {
    for (jj in 1:length(lat.new)) {
      
      #Assign new values to nn for the observational datasets
      idx.lon = which(abs(as.numeric(lon.old)-lon.new[ii]) <=
                            min(abs(as.numeric(lon.old)-lon.new[ii])))[1] #Welcher Gitterpunkte ist am nächsten dran am neuen Gitter (Minimale Distanz)
      idx.lat = which(abs(as.numeric(lat.old)-lat.new[jj]) <=
                            min(abs(as.numeric(lat.old)-lat.new[jj])))[1]
      
      
      prmax.new.obvs[ii, jj] <- prmax.old[idx.lon, idx.lat] #Zuordnung neuer Wert zu nearest neighour
      sd.new.obvs[ii, jj] <- sd.old[idx.lon, idx.lat]
    }
  }
  
  #Convert into dataframes
  dimnames(prmax.new.obvs) = list(lon = lon.new, lat = lat.new)
  new.df <- melt(prmax.new.obvs)
  head(new.df)
  
  dimnames(sd.new.obvs) = list(lon.new, lat=lat.new)
  new.df.sd <- melt(sd.new.obvs)
  head(new.df.sd)
  
  
  #Regrid everything to (-180, 180) and (-90, 90) Grid so if statement does not have to be used
  for (i in sequence){
    print(i)
    
    prmax.new.his = matrix(NA, nrow=length(lon.new),ncol=length(lat.new))
    load(gsub(" ", "", (paste("./prec/hist_",model_his[i],".prmax.RData"))))
    prmax.his <- assign(paste("mean_", i, sep = ""), apply(prmax, c(1,2), mean))
    lon.his <- lon_his[[i]]  # With double brackets you access lists within lists
    lon.his <- apply(lon_his[[i]], c(1), function(x) ifelse(x > 180, -360 + x, x))
    lat.his <- lat_his[[i]]
    for (ii in 1:length(lon.new)) {
      for (jj in 1:length(lat.new)) {
        #Assign new values to nn for the hist models
        idx.lon.his = which(abs(lon.his-lon.new[ii]) <=
                          min(abs(lon.his-lon.new[ii])))[1] #Welcher Gitterpunkte ist am nähsten dran am neuen Gitter (Minimale Distanz)
        idx.lat.his = which(abs(lat.his-lat.new[jj]) <=
                          min(abs(lat.his-lat.new[jj])))[1]
        
        prmax.new.his[ii, jj] <- prmax.his[idx.lon.his, idx.lat.his] #Zuordnung neuer Wert zu nearest neighour
      }
    }
    
    dimnames(prmax.new.his) = list(lon = lon.new, lat = lat.new)
    new.df.his <- melt(prmax.new.his)
    head(new.df.his)
    
    #Apply formula for standartised Mittelwertabweichung
    diff <- (new.df.his$value - new.df$value)
    
    lis_diff <- list.append(lis_diff, mean(diff, na.rm=TRUE))
    print("Next follows the list of diffs")
    
    diff <- diff / new.df.sd$value
    print("The Sds are saved now")
    lis_sd <- list.append(lis_sd, mean(new.df.sd$value, na.rm=TRUE))
    
    diff.matrix <- diff
    #dimnames(pe.matrix) = list(lon = lon.new, lat = lat.new)
    diff.df <- melt(diff.matrix)
    diff.df['lat'] <- new.df['lat']
    diff.df['lon'] <- new.df['lon']
    diff.df <- diff.df[, c(2,3,1)]
    #diff.df['lon'] <- lapply(diff.df['lon'], function(x) ifelse(x > 180, -360 + x, x)) #Change into 0-180 longitude values
    head(diff.df)
    assign(paste0("diff_",             # Save Endmatrix for every of caluclatin on observational data
                  i),
           diff.df)
    
    #Try to make a for loop ??
     diff.df %>% ggplot(aes(x = lon, y = lat, fill = value)) + 
       ggtitle(paste0(names, sep= " ", "obvs. vs. hist_model", i, sep="")) +
       geom_raster(interpolate = FALSE) +  # adding heat maps 
       labs(x="Longitude", y="Latitude", fill = "Norm. Sd") +
       #scale_fill_viridis_c(na.value = NA, limits = c(-5, 5)) + # change color
       #scale_fill_gradient2(high = "green", mid = "white", low ="red", midpoint = 0, limits = c(-5,5), na.value = NA) +
       #scale_fill_gradientn(
       #colours=c("darkgreen", "chartreuse4", "chartreuse", "white", "coral", "coral2", "coral3"),
       #  values = rescale(c(3, 2, 1, 0, -1, -2, -3)), limits = c(-3,3)) +
       scale_fill_fermenter(palette = "RdBu", breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3, 3), na.value = "grey60") + #oob = squish) + 
       guides(fill = guide_coloursteps(even.steps = F, show.limits = T)) +
       #scale_fill_manual(values = c("0" = "white")) + 
       borders() + # adding country borders
       coord_equal(expand = FALSE)
    # 
     ggsave(path = paste0("~/Projekt_22/google/prmax/", names, sep="","/") , filename = paste0(names, "_vs_hist", i, ".png", sep = ""))
     print("Figure saved")
     dev.set(dev.prev())
     dev.off()
  }
  
  lis_diff_new <- list()
  print("The mean of lis_diff is calculated now")
  for (i in 1:length(lis_diff)) {
    x <- lis_diff[[i]]
    lis_diff_new <- append(lis_diff_new, x)
  }
  
  lis_sd_new <- list()
  print("The sd of lis_diff is calculated now")
  for (i in 1:length(lis_sd)) {
    y <- lis_sd[[i]]
    lis_sd_new <- append(lis_sd_new, y)
  }
  
  print("The mean of the list of mean differences is calculated now")
  mean_lis_diff <- mean(unlist(lis_diff_new), na.rm=TRUE)
  assign(paste0("prmax_mean_diff_",  names), mean_lis_diff)
  print(mean_lis_diff)
  print("The maximum of lis_diff_new is")
  print(max(unlist(lis_diff_new)))
  assign(paste0("prmax_max_diff_",  names), max(unlist(lis_diff_new)))
  print("The minimum of lis_diff_new is")
  print(min(unlist(lis_diff_new)))
  assign(paste0("prmax_min_diff_",  names), min(unlist(lis_diff_new)))
  
  mean_lis_sd <- mean(unlist(lis_sd_new), na.rm = TRUE)
  print("The mean of the list of sds is calculated now")
  print(mean_lis_sd)
  assign(paste0("prmax_mean_sd_",  names), mean_lis_sd)
  print("The maximum of lis_diff_new is")
  print(max(unlist(lis_sd_new)))
  assign(paste0("prmax_max_diff_",  names), max(unlist(lis_sd_new)))
  print("The minimum of lis_diff_new is")
  print(min(unlist(lis_sd_new)))
  assign(paste0("prmax_min_diff_",  names), min(unlist(lis_sd_new)))
  
  # Combine all the 48 dataframes of the standardized difference and calculate mean and std. over it
  diff_list <- list()
  for (i in sequence){
    diff_list[[i]] <- get(paste("diff_", i, sep=""))
  }
  
  mean_end <- rbindlist(diff_list)[,lapply(.SD,mean), list(lat, lon)]
  sd_end <- rbindlist(diff_list)[,lapply(.SD,sd), list(lat, lon)] 
  
  ##Plot End result for each observational dataset#######
  
  mean_end %>% ggplot(aes(x = lon, y = lat, fill = value)) + 
    ggtitle(paste0(names, sep= " ", "obvs. vs. hist_model mean FINAL")) +
    geom_raster(interpolate = FALSE) +  # adding heat maps 
    labs(x="Longitude", y="Latitude", fill = "Mean norm. Sd") +
    #scale_fill_viridis_c(na.value = NA, limits = c(-5, 5)) + # change color
    #scale_fill_gradient2(high = "green", mid = "white", low ="red", midpoint = 0, limits = c(-5,5), na.value = NA) + # 
    #scale_fill_gradientn(
    #  colours=c("darkgreen", "chartreuse4", "chartreuse", "white", "coral", "coral2", "coral3"),
    #  values = rescale(c(3, 2, 1, 0, -1, -2, -3)), limits = c(-3,3)) + 
    scale_fill_fermenter(palette = "RdBu", breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3, 3), na.value = "grey60") + #, oob = squish) + 
    guides(fill = guide_coloursteps(even.steps = F, show.limits = T)) +
    #scale_fill_manual(values = c("0" = "white"))
    borders() + # adding country borders
    coord_equal(expand = FALSE)
  
  #ggsave(path = "~/Documents/Projekt_22/google/prmax/CHIRPS/", filename = "CHIRPS_vs_hist_mean_FINAL.png")
  ggsave(path = paste0("~/Projekt_22/google/prmax/", names, sep="","/") , filename = paste(names, sep="", "_vs_hist_mean_FINAL.png"))
  print("Figure saved mean")
  dev.set(dev.prev())
  dev.off()
  
  ##Same for the std.
  
  sd_end %>% ggplot(aes(x = lon, y = lat, fill = value)) + 
    ggtitle(paste0("CHIRPS obvs. vs. hist_model std. FINAL")) +
    labs(x="Longitude", y="Latitude", fill = "Sd norm. Sd") +
    geom_raster(interpolate = FALSE) +  # adding heat maps 
    #scale_fill_viridis_c(na.value = NA, limits = c(-5, 5)) + # change color
    #scale_fill_gradient2(high = "green", mid = "white", low ="red", limits = c(0,3), na.value = NA) +
    scale_fill_fermenter(palette = "Greens", breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3), limits = c(0, 3), na.value = "grey60", direction=-1) + #oob = squish) + 
    guides(fill = guide_coloursteps(even.steps = F, show.limits = T)) +
    borders() + # adding country borders
    coord_equal(expand = FALSE)
  
  #ggsave(path = "~/Documents/Projekt_22/google/prmax/CHIRPS/", filename = paste("CHIRPS_vs_hist_std_FINAL.png"))
  ggsave(path = paste0("~/Projekt_22/google/prmax/", names, sep="","/") , filename = paste(names, sep="", "_vs_hist_std_FINAL.png"))
  print("Figure saved mean")
  dev.set(dev.prev())
  dev.off()
}
######Convert to matrix to compare - Security check##### 
#dimnames(prmax.his) = list(lon = lon.his, lat = lat.his)
#old.df.his = melt(prmax.his)









  
  
