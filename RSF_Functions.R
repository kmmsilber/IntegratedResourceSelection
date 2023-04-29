

##################################################################
### FUNCTIONS USED IN GRASSLAND BIRD RESOURCE SELECTION MODELS ###
##################################################################

library(dplyr)
library(mgcv)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(plotKML)
library(data.table)
library(FNN)
library(spbabel)
library(tidyverse)
library(lubridate)


##### load datasets #####
# Konza map
KNZ<- readOGR("./GIS001/GIS001.shp") #shapefile for konza site boundary

# watershed shapefile 
ws.konza <- readOGR("./GIS032/GIS032.shp") #shapefile for konza watersheds

# exclude unsampled watersheds
samp.ws<-c("2D","C3A","C3B","C3C","C3SA","C3SB","C3SC",
           "K1B","K2A","N1A","N1B","N2A","N2B")
knz.samp.sites<-ws.konza[which(ws.konza$NAME_1 %in% samp.ws),]

# watershed covariate data
watershed_info<-read.csv("WatershedCovariates2021.csv")
# remove umlat in column name
colnames(watershed_info)[1] <- gsub('^...','',colnames(watershed_info)[1])

# rasters for distance to woody vegetation
rl.dist.to.shrubs<-raster("DistanceToShrubsRaster.tif")
rl.dist.to.trees<-raster("DistanceToTreesRaster.tif")

# raster for proportion of woody vegetation
woody.raster <- raster("randfor_FUNC_GRP_woodycover.tif")

#create spatial points for shrubs
pts.shrubs <- rasterToPoints(woody.raster, fun=function(x){x==3}, spatial = TRUE)
pts.shrubs <- spTransform(pts.shrubs, crs(ws.konza))

#create spatial points for trees
pts.trees <- rasterToPoints(woody.raster, fun=function(x){x==2}, spatial = TRUE)
pts.trees <- spTransform(pts.trees, crs(ws.konza))

# elevation raster
sf.elev <- raster("GIS200/GIS200.tif")

# calculate derivative of elevation (i.e. slope)
W.s1 <- matrix(c(0,0,0,1,0,-1,0,0,0),3,3)/(2*2) 
W.s2 <- matrix(c(0,1,0,0,0,0,0,-1,0),3,3)/(2*2) 
der.elev <- sqrt(focal(sf.elev, W.s1, fun=sum)^2 + focal(sf.elev, W.s2, fun=sum)^2)

# soils raster
sf.soils <- readOGR("GIS220/Konza_SSURGO_Soils_(GIS220).shp")

# create time variable
date.range<-seq(as.Date("1-Apr-14", format = "%d-%b-%y"), as.Date("1-Sep-21", format = "%d-%b-%y"), by="days")
time.range<-c(0:as.numeric(difftime(max(date.range),min(date.range),"1977-12-16 00:00:00 UTC",units="days")))
df.time<-data.frame(date = date.range,time = time.range)
df.time$month <- month(df.time$date)
df.time$year <- year(df.time$date)

# constrain random point time periods to summer months and 2014-2020
df.summer<-df.time[which(month(df.time$date) %in% c(5, 6, 7)),]


##### calculate daily centroid of multiple waypoints #####
func_CalculateDailyCentroid<-function(df){
  avg.loc <- data.frame(bands = aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$Group.1,
                        time = aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$Group.2,
                        date = aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$Group.3,
                        year = aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$Group.4,
                        month = aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$Group.5,
                        sx = round(aggregate(df$sx,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$x),
                        sy = round(aggregate(df$sy,by=list(df$bands, df$time, df$date, df$year, df$month),FUN=mean)$x))
  avg.loc<-avg.loc[!duplicated(avg.loc[,c("bands","sx","sy")]),] #change month/year here to adjust territory summaries
  pts <- avg.loc
  coordinates(pts) = ~ sx + sy
  
  # add CRS for bird locations
  proj4string(pts) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
  pts <- spTransform(pts, crs(KNZ))
  
  # remove points outside of Konza
  pts <- pts[-which(is.na(over(pts,KNZ)$AREA)==TRUE),]
}


##### aggregate nearby points based on expected territory size within a season into one centroid  #####
func_AggNearbyPoints<-function(df.input, buffer){
  #create empty data frames to store territory info
  df.all<-data.frame(NULL)
  #create a loop for each individual (i.e. band combo)
  for(i in unique(df.input$bands)){
    df.bands<-df.input[df.input$bands == i,]
    #create a loop for each year so we only aggregate observations within a year
    for(j in unique(df.bands$year)){
      df.temp<-as.data.frame(df.bands[df.bands$year == j,])
      #create a distance matrix for observations within a year
      dmat <- gDistance(df.bands[df.bands$bands == i & df.bands$year == j,], byid=TRUE)
      #create a loop to cycle through each day's centroid and decide whether or not to aggregate
      for(r in 1:nrow(df.temp)) {
        #store the rows that should be aggregated
        a<-which(dmat[r,] < buffer)
        agg.df<-df.temp[a,]
        #take the minimum time (i.e. when territory was selected)
        # and calculate the centroid
        new.df<-data.frame(bands = i,
                           time = min(agg.df$time),
                           date = min(agg.df$date),
                           year = min(agg.df$year),
                           month = min(agg.df$month),
                           sx = mean(agg.df$sx),
                           sy = mean(agg.df$sy))
        # save all the values of the newly calculated centroid
        df.all <- rbind(df.all, new.df)
        df.all<-df.all[!duplicated(df.all), ]
        df.all}}
  }
  #turn centroids back into coordinates
  coordinates(df.all) <- ~sx+sy
  # add CRS for bird locations
  proj4string(df.all) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
  df.all <- spTransform(df.all, crs(KNZ))
  #print the data frame
  df.all
}


##### calculate slope for observed territories and pseudo-absences #####
func_SlopeRSF<-function(spp.pts, buffer, spp_data, n_abs){
  #calculate slope for used territories
  terr.elev <- raster::extract(der.elev, spp.pts,buffer = buffer,fun=mean, df=TRUE)
  terr.elev<- data.frame(slope = terr.elev$layer,
                         bands = spp.pts$bands,
                         time = spp.pts$time,
                         sx = spp.pts@coords[,1],
                         sy = spp.pts@coords[,2],
                         pres = 1)
  # calculate slope for unused territories
  abs.pts<-spsample(knz.samp.sites,n=n_abs,type="random")
  unused.terr.elev <- raster::extract(der.elev, abs.pts, buffer = buffer, fun=mean, df=TRUE)
  unused.terr.elev<- data.frame(slope = unused.terr.elev$layer,
                                bands = NA,
                                time = sample(df.summer$time, size = length(abs.pts@coords[,1]), replace = TRUE),
                                sx = abs.pts@coords[,1],
                                sy = abs.pts@coords[,2],
                                pres = 0)
  all.elev<-rbind(terr.elev,unused.terr.elev)
  # save in species' data frame
  spp_data<-right_join(spp_data, all.elev, by = c("bands", "time","sx","sy"))
}


##### add watershed name to species data frame to bind management and precip values #####
func_Watershed<-function(spp_data){
  temp_data<-spp_data
  coordinates(temp_data)<- ~ sx+sy
  crs(temp_data)<-crs(ws.konza)
  watershed<-data.frame(raster::extract(ws.konza,temp_data)$NAME_1)
  colnames(watershed)<-"PlotName"
  cbind(spp_data, watershed)
}


##### add elevation and soil data to data frame #####
func_Elev_Soil<-function(spp_data){
  temp_data<-spp_data
  coordinates(temp_data)<- ~ sx+sy
  crs(temp_data)<-crs(ws.konza)
  elev <- raster::extract(sf.elev, temp_data,fun=mean, df=TRUE)[,2]
  soil <- raster::extract(sf.soils, temp_data)$MUSYM
  cbind(spp_data, elev, soil)
}


##### predict herbaceous veg from species-specific resource selection model #####
func_VegRSF<-function(spp_data){
  spp_data$obs<-0
  veg.data <- data.frame(bands = spp_data$bands,
                         time = spp_data$time, 
                         sx = spp_data$sx, 
                         sy = spp_data$sy)
  veg.data2 <- data.frame(veg.data,HT = predict(HT.mod,spp_data,type="response", exclude = "s(obs)"),
                          LG = predict(LG.mod,spp_data,type="response", exclude = "s(obs)"),
                          LF = predict(LF.mod,spp_data,type="response", exclude = "s(obs)"),
                          BG = predict(BG.mod,spp_data,type="response", exclude = "s(obs)"),
                          DG = predict(DG.mod,spp_data,type="response", exclude = "s(obs)"))
  spp_data<-left_join(spp_data, veg.data2, by = c("bands", "time","sx","sy"))
}


##### extract distance to shrubs/trees and proportion of shrubs/trees for each territory #####
func_WoodyRSF<-function(spp_data, size.buffer, dist.shrubs, dist.trees, prop.shrubs, prop.trees){
  #calculate distance to shrubs for used and unused territories
  spp.pts<-spp_data
  coordinates(spp.pts) <- ~ sx + sy
  dist.shrubs <- raster::extract(dist.shrubs,spp.pts,buffer = size.buffer, fun=mean, df=TRUE)
  dist.trees <- raster::extract(dist.trees,spp.pts,buffer = size.buffer,fun=mean, df=TRUE)
  prop.shrubs <- raster::extract(prop.shrubs,spp.pts,buffer = size.buffer, fun=mean, df=TRUE)
  prop.trees <- raster::extract(prop.trees,spp.pts,buffer = size.buffer, fun=mean, df=TRUE)
  dist<- data.frame(DistToShrubs = dist.shrubs[,2],
                    DistToTrees = dist.trees[,2],
                    PropShrubs = prop.shrubs[,2],
                    PropTrees = prop.trees[,2],
                    bands = spp.pts$bands,
                    time = spp.pts$time,
                    sx = spp.pts@coords[,1],
                    sy = spp.pts@coords[,2])
  # save in species' data frame
  left_join(spp_data, dist, by = c("bands","time","sx","sy"), all.x = TRUE)
}



##### create predictive raster based on resource selection functions #####
func_CreatePredictiveRaster <- function(map.resolution, sf.ws, model, time.input, buffer, dist.shrubs, dist.trees, prop.shrubs, prop.trees){
  
  # create data frame with all locations & choose resolution
  df_watershed_pred <- expand.grid(sx = seq(706379,712869, by = map.resolution),
                                   sy = seq(4327733,4332653, by = map.resolution))
  
  # convert data frame to coordinates
  coordinates(df_watershed_pred) <- ~ sx + sy
  
  # set CRS for both objects
  proj4string(df_watershed_pred) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
  proj4string(knz.samp.sites) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
  
  # remove points not within sampled watersheds
  df_watershed_pred <- df_watershed_pred[-which(is.na(over(df_watershed_pred,sf.ws)$AREA)==TRUE),]
  
  
  #save coordinates into data frame
  watershed <- data.frame(sx = df_watershed_pred@coords[,1],
                          sy = df_watershed_pred@coords[,2])
  
  # provide watershed name based on location
  watershed<-data.frame(raster::extract(knz.samp.sites,df_watershed_pred)$NAME_1)
  colnames(watershed)<-"watershed"
  
  ### predict slope using models
  pred.pts<-df_watershed_pred
  terr.elev <- raster::extract(der.elev, pred.pts, fun=mean, df=TRUE)
  pred.pts$slope <- terr.elev$layer
  df.preds<-as.data.frame(pred.pts)
  df.preds <- cbind(df.preds, watershed)
  
  ### predict elevation
  elev <- raster::extract(sf.elev, pred.pts,fun=mean, df=TRUE)[,2]
  df.preds <- cbind(df.preds, elev)
  
  ### predict veg using models
  # save watershed info
  df.preds<-left_join(df.preds, watershed_info, by = "watershed")
  
  # set time 
  df.preds$time<-time.input
  
  # add month and observer column
  df.preds<-left_join(df.preds, df.time, by = "time")
  df.preds$obs<-0
  
  # add management column
  df.preds$mgmt <- paste(df.preds$gr, df.preds$fire, sep = "")
  
  # predict herbaceous veg
  veg.preds <- data.frame(HT = predict(HT.mod,df.preds,type="response", exclude = "s(obs)"),
                          LG = predict(LG.mod,df.preds,type="response", exclude = "s(obs)"),
                          LF = predict(LF.mod,df.preds,type="response", exclude = "s(obs)"),
                          BG = predict(BG.mod,df.preds,type="response", exclude = "s(obs)"),
                          DG = predict(DG.mod,df.preds,type="response", exclude = "s(obs)"))
  df.preds<-cbind(df.preds, veg.preds)
  
  # predict woody veg
  df.preds$bands<-NA
  df.preds<-func_WoodyRSF(df.preds,buffer, dist.shrubs, dist.trees, prop.shrubs, prop.trees)
  
  # predict probability of territory
  spp.pred<-as.vector(predict(model,df.preds,type="response", exclude = "s(obs)"))
  
  # prep predicted data to plot on Konza
  sp.spp<-df.preds[,c("sx","sy")]
  sp.spp$pres<-spp.pred
  coordinates(sp.spp) <- ~sx+sy
  crs(sp.spp)<-crs(ws.konza)
  sp.spp<-rasterFromXYZ(sp.spp)
  sp.spp}



##### create a distance to woody vegetation raster after removing one upland shrub/tree #####
func_DistanceToWoodyVegRaster <-function(shape.extent, layer, xmin, xmax, ymin, ymax){
  rl.woody <- raster("randfor_FUNC_GRP_woodycover.tif")
  rl.woody<-crop(rl.woody,shape.extent)
  woodyveg <- rl.woody == layer
  names(woodyveg) <- "woodyveg"
  rl.temp <- woodyveg
  rl.temp[rl.temp == 0] <- NA
  pts <- rl.temp
  rl.temp<-data.frame(rasterToPoints(rl.temp))
  rl.temp <- rl.temp[!(rl.temp$x > xmin & rl.temp$x < xmax &
                         rl.temp$y > ymin & rl.temp$y < ymax),]
  woodyveg<-rasterToPoints(woodyveg)
  dist <- get.knnx(rl.temp[,1:2], woodyveg[,1:2], k = 1, algo = "kd_tree")$nn.dist/1000
  df.temp<-data.frame(sx = woodyveg[,1],sy = woodyveg[,2],dist = dist[,1])
  rl.dist.to.woodyveg<-rasterFromXYZ(df.temp)
  crs(pts) <- "+proj=utm +zone=14 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  crs(rl.dist.to.woodyveg) <- "+proj=utm +zone=14 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  rl.dist.to.woodyveg
}


##### create a distance to woody vegetation raster after removing all of shrubs/trees in areas of slope >10% #####
func_DistanceToWoodyVegRaster_Slope <-function(shape.extent, layer, woody_ws){
  rl.woody <- raster("randfor_FUNC_GRP_woodycover.tif")
  rl.woody<-crop(rl.woody, shape.extent)
  rl.slope <- crop(der.elev,extent(shape.extent))
  rl.slope[] <- ifelse(rl.slope[] <.1, 0, NA)
  df.temp <- merge(woody_ws, rasterToPoints(rl.slope), by = c("x","y"))
  rl.uplands <- crop(sf.elev,extent(shape.extent))
  rl.uplands[] <- ifelse(rl.uplands[] >400, 0, NA)
  df.temp <- merge(df.temp, rasterToPoints(rl.uplands), by = c("x","y"))[,1:2]
  rl.woody <- rasterize(df.temp, rl.woody, field = 0, update = TRUE)
  woodyveg <- rl.woody == layer
  names(woodyveg) <- "woodyveg"
  rl.temp <- woodyveg
  rl.temp[rl.temp == 0] <- NA
  pts <- rl.temp
  rl.temp<-rasterToPoints(rl.temp)
  woodyveg<-rasterToPoints(woodyveg)
  dist <- get.knnx(rl.temp[,1:2], woodyveg[,1:2], k = 1, algo = "kd_tree")$nn.dist/1000
  df.temp<-data.frame(sx = woodyveg[,1], sy = woodyveg[,2],dist = dist[,1])
  rl.dist.to.woodyveg<-rasterFromXYZ(df.temp)
  crs(pts) <- "+proj=utm +zone=14 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  crs(rl.dist.to.woodyveg) <- "+proj=utm +zone=14 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"
  rl.dist.to.woodyveg
}



##### create raster for habitat viability and woody removal experiment #####

func_WoodyRemovalExperiment<-function(name.ws, file.name, map.resolution, model, time.input, buffer,
                                      shrub_island_xmin, shrub_island_xmax, shrub_island_ymin, shrub_island_ymax,
                                      upland_tree_xmin, upland_tree_xmax, upland_tree_ymin, upland_tree_ymax){
  
  ###create baseline raster with original shrubs and trees
  
  #select watershed
  sf.ws<-ws.konza[which(ws.konza$NAME_1 %in% name.ws),]
  
  #save original shrub and tree rasters
  rl.dist.to.shrubs<-raster("DistanceToShrubsRaster.tif")
  rl.shrubs <- raster("randfor_FUNC_GRP_woodycover.tif")
  rl.shrubs[rl.shrubs != 3] <- 0
  rl.shrubs[rl.shrubs == 3] <- 1
  
  rl.dist.to.trees<-raster("DistanceToTreesRaster.tif")
  rl.trees <- raster("randfor_FUNC_GRP_woodycover.tif")
  rl.trees[rl.trees != 2] <- 0
  rl.trees[rl.trees == 2] <- 1
  
  sp.ws<-func_CreatePredictiveRaster(map.resolution, sf.ws, model, time.input, buffer, rl.dist.to.shrubs, rl.dist.to.trees, rl.shrubs, rl.trees)
  writeRaster(sp.ws, paste(file.name,"Predictions", format(Sys.Date(), "%b%d"), sep = "_"))
  
  # make maps for woody removal experiment
  df_sp<-data.frame(rasterToPoints(sp.ws))
  
  # subset konza map
  df_knz <- fortify(sf.ws, id = "NAME_1")
  # create color palette
  col.pal<-colorRampPalette(c("black","navy","darkslateblue","darkslategray4","mediumseagreen","khaki"))
  
  # subset trees and shrubs to just focal watershed
  shrubs_ws <- pts.shrubs[-which(is.na(over(pts.shrubs,sf.ws)$AREA)==TRUE),]
  shrubs_ws <- data.frame(shrubs_ws)
  trees_ws <- pts.trees[-which(is.na(over(pts.trees,sf.ws)$AREA)==TRUE),]
  trees_ws <- data.frame(trees_ws)
  
  
  # plot predictions
  plot_ws <- ggplot() +
    geom_raster(data = df_sp, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = (col.pal(100)), name = "Prob.Selection") +
    coord_fixed()
  
  plot_wsshrubs<-ggplot() +
    geom_raster(data = df_sp, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    geom_point(data = shrubs_ws, aes(x = x, y = y), col = "sienna4", size = 0.9) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = (col.pal(100)), name = "Prob.Selection") +
    coord_fixed()
  
  plot_wstrees<-ggplot() +
    geom_raster(data = df_sp, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    geom_point(data = trees_ws, aes(x = x, y = y), col = "sienna4", size = 2, shape = 17) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_gradientn(colors = (col.pal(100)), name = "Prob.Selection") +
    coord_fixed()
  
  plot_all <- ggarrange(plot_ws, plot_wsshrubs, plot_wstrees, nrow = 1, ncol = 3,
                        labels = c("A","B","C"))
  
  # save plots
  png(paste(file.name,"_Plot_", format(Sys.Date(), "%b%d"), ".png", sep = ""))
  print(plot_ws)
  dev.off()
  
  png(paste(file.name,"_WoodyPanels_", format(Sys.Date(), "%b%d"), ".png", sep = ""))
  print(plot_all)
  dev.off()
  
  
  
  ##### woody removal experiment #####
  
  ### remove one upland shrub island
  rl.dist.rem.shrubs <- func_DistanceToWoodyVegRaster(sf.ws, 3, shrub_island_xmin, shrub_island_xmax, shrub_island_ymin, shrub_island_ymax)
  
  # shrub presence/absence:
  rl.rem.shrubs <- rl.dist.rem.shrubs
  rl.rem.shrubs[] <- ifelse(rl.rem.shrubs[] > 0, 0, 1)
  
  #create new raster
  sp.rem.shrubs<-func_CreatePredictiveRaster(map.resolution, sf.ws, model, time.input, buffer, rl.dist.rem.shrubs, rl.dist.to.trees, rl.rem.shrubs, rl.trees)
  writeRaster(sp.rem.shrubs, paste(file.name,"Predictions_ShrubRemoval", format(Sys.Date(), "%b%d"), sep = "_"))
  
  
  ### remove one upland tree
  rl.dist.rem.trees <- func_DistanceToWoodyVegRaster(sf.ws, 2, upland_tree_xmin, upland_tree_xmax, upland_tree_ymin, upland_tree_ymax)
  
  # trees presence/absence:
  rl.rem.trees <- rl.dist.rem.trees
  rl.rem.trees[] <- ifelse(rl.rem.trees[] > 0, 0, 1)

  #create new raster
  sp.rem.trees<-func_CreatePredictiveRaster(map.resolution, sf.ws, model, time.input, buffer, rl.dist.to.shrubs, rl.dist.rem.trees, rl.shrubs, rl.rem.trees)
  writeRaster(sp.rem.trees, paste(file.name,"Predictions_TreeRemoval", format(Sys.Date(), "%b%d"), sep = "_"))
  
  
  ### remove all shrubs >10% slope
  rl.dist.rem.shrubs_slope <- func_DistanceToWoodyVegRaster_Slope(sf.ws, 3, shrubs_ws)
  
  # shrub presence/absence:
  rl.rem.shrubs_slope <- rl.dist.rem.shrubs_slope
  rl.rem.shrubs_slope[] <- ifelse(rl.rem.shrubs_slope[] > 0, 0, 1)
  
  ### calculate predictions
  sp.rem.shrubs_slope<-func_CreatePredictiveRaster(map.resolution, sf.ws, model, time.input, buffer, rl.dist.rem.shrubs_slope, rl.dist.to.trees, rl.rem.shrubs_slope, rl.trees)
  writeRaster(sp.rem.shrubs_slope, paste(file.name,"Predictions_ShrubRemovalSlope", format(Sys.Date(), "%b%d"), sep = "_"))
  
  
  ### remove all trees >10% slope
  rl.dist.rem.trees_slope <- func_DistanceToWoodyVegRaster_Slope(sf.ws, 2, trees_ws)

  # tree presence/absence:
  rl.rem.trees_slope <- rl.dist.rem.trees_slope
  rl.rem.trees_slope[] <- ifelse(rl.rem.trees_slope[] > 0, 0, 1)
  
  
  ### calculate predictions
  sp.rem.trees_slope<-func_CreatePredictiveRaster(map.resolution, sf.ws, model, time.input, buffer, rl.dist.to.shrubs, rl.dist.rem.trees_slope, rl.shrubs, rl.rem.trees_slope)
  writeRaster(sp.rem.trees_slope, paste(file.name,"Predictions_TreeRemovalSlope", format(Sys.Date(), "%b%d"), sep = "_"))
  
  
  ### plot results from woody removal
  #removal of one shrub
  sp.rem <- sp.ws
  sp.rem[] <- sp.rem.shrubs[] - sp.ws[] 
  sp.rem[sp.rem<0]<-0
  sp.rem[] <- sp.rem[] * 100
  sp.rem[sp.rem[] >39] <- 40
  df_sp.rem.shrubs<-data.frame(rasterToPoints(sp.rem))
  
  #plot predictions
  plot_remshrub <- ggplot() +
    geom_raster(data = df_sp.rem.shrubs, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    theme_void() +
    #theme(legend.position = "none") +
    scale_fill_gradientn(colors = colorRampPalette(c("white","gold2","orange1","darkorange1"))(100), 
                         breaks=seq(0,40,10),
                         limits=c(0, 40),
                         labels = c(" 0", " 10", " 20", " 30", ">40")) +
    labs(fill='') +
    coord_fixed()
  
  
  #removal of one tree
  sp.rem <- sp.ws
  sp.rem[] <- sp.rem.trees[] - sp.ws[] 
  sp.rem[sp.rem<0]<-0
  sp.rem[] <- sp.rem[] * 100
  sp.rem[sp.rem[] >39] <- 40
  df_sp.rem.trees<-data.frame(rasterToPoints(sp.rem))
  
  #plot predictions
  plot_remtree <- ggplot() +
    geom_raster(data = df_sp.rem.trees, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    theme_void() +
    #theme(legend.position = "none") +
    scale_fill_gradientn(colors = colorRampPalette(c("white","gold2","orange1","darkorange1"))(100), 
                         breaks=seq(0,40,10),
                         limits=c(0, 40),
                         labels = c(" 0", " 10", " 20", " 30", ">40")) +
    labs(fill='') +
    coord_fixed()
  
  #removal of uplands shrubs
  sp.rem <- sp.ws
  sp.rem[] <- sp.rem.shrubs_slope[] - sp.ws[] 
  sp.rem[sp.rem<0]<-0
  sp.rem[] <- sp.rem[] * 100
  sp.rem[sp.rem[] >39] <- 40
  df_sp.rem.shrubs_slope<-data.frame(rasterToPoints(sp.rem))
  
  #plot predictions
  plot_remshrub_slope <- ggplot() +
    geom_raster(data = df_sp.rem.shrubs_slope, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    theme_void() +
    #theme(legend.position = "none") +
    scale_fill_gradientn(colors = colorRampPalette(c("white","gold2","orange1","darkorange1"))(100), 
                         breaks=seq(0,40,10),
                         limits=c(0, 40),
                         labels = c(" 0", " 10", " 20", " 30", ">40")) +
    labs(fill='') +
    coord_fixed()
  
  #removal of uplands trees
  sp.rem <- sp.ws
  sp.rem[] <- sp.rem.trees_slope[] - sp.ws[] 
  sp.rem[sp.rem<0]<-0
  sp.rem[] <- sp.rem[] * 100
  sp.rem[sp.rem[] >39] <- 40
  df_sp.rem.trees_slope<-data.frame(rasterToPoints(sp.rem))
  
  #plot predictions
  plot_remtree_slope <- ggplot() +
    geom_raster(data = df_sp.rem.trees_slope, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
    theme_void() +
    #theme(legend.position = "none") +
    scale_fill_gradientn(colors = colorRampPalette(c("white","gold2","orange1","darkorange1"))(100), 
                         breaks=seq(0,40,10),
                         limits=c(0, 40),
                         labels = c(" 0", " 10", " 20", " 30", ">40")) +
    labs(fill='') +
    coord_fixed()
  
  plot_all_rem <- ggarrange(plot_remshrub, plot_remtree, plot_remshrub_slope, plot_remtree_slope, nrow = 2, ncol = 2,
                            labels = c("A","B","C","D"), common.legend = TRUE)
  
  png(paste(file.name,"_WoodyRemovalPanels_", format(Sys.Date(), "%b%d"), ".png", sep = ""))
  print(plot_all_rem)
  dev.off()
  
}

