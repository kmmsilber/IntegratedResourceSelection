

# VALIDATE VEGETATION COMPOSITION -----------------------------------------

# load vegetation composition data
df_vegcomp2022 <- read.csv("VegetationComposition2022_CleanedData.csv")
df_vegcomp2022$obs <- 1 #create dummy observer
colnames(df_vegcomp2022)[c(5,6)] <- c("sx", "sy")

# remove study units off Konza
df_vegcomp2022 <- df_vegcomp2022[df_vegcomp2022$PlotName != "COA",]
df_vegcomp2022 <- df_vegcomp2022[df_vegcomp2022$PlotName != "COB",]


#### Live Grass #### 

#predict live grass
df_vegcomp2022$LG.pred <- predict(LG.mod, df_vegcomp2022, type="response", exclude = "s(obs)")

# calculate difference between observed and predicted values
veg_validation <- df_vegcomp2022
veg_validation$LG.abs.diff <- abs(df_vegcomp2022$X.LiveGrass-df_vegcomp2022$LG.pred)


#### Live Forbs ####

# predict live forbs
df_vegcomp2022$LF.pred <- predict(LF.mod, df_vegcomp2022, type="response", exclude = "s(obs)")

# calculate difference between observed and predicted values
veg_validation$LF.abs.diff <- abs(df_vegcomp2022$X.LiveForb-df_vegcomp2022$LF.pred)


#### Bare Ground ####

# predict bare ground
df_vegcomp2022$BG.pred <- predict(BG.mod, df_vegcomp2022, type="response", exclude = "s(obs)")

# calculate difference between observed and predicted values
veg_validation$BG.abs.diff <- abs(df_vegcomp2022$X.Bare-df_vegcomp2022$BG.pred)


#### Dead Grass and Litter ####

# predict dead grass and litter
df_vegcomp2022$DG.pred <- predict(DG.mod, df_vegcomp2022, type="response", exclude = "s(obs)")

# calculate difference between observed and predicted values
veg_validation$DG.abs.diff <- abs(df_vegcomp2022$X.DeadAndLitter-df_vegcomp2022$DG.pred)


#### Vegetation height ####

# load vegetation height data
df_veght2022 <- read.csv("VegetationHeight2022_CleanedData.csv")
df_veght2022$obs <- 1

# remove observations off Konza
df_veght2022 <- df_veght2022[df_veght2022$plot != "COA",]
df_veght2022 <- df_veght2022[df_veght2022$plot != "COB",]

# predict vegetation height
df_veght2022$HT.pred <- predict(HT.mod, df_veght2022, type="response", exclude = "s(obs)")

# calculate difference between observed and predicted values
veg_htvalidation <- df_veght2022
veg_htvalidation$abs.diff <- abs(df_veght2022$ht-df_veght2022$HT.pred)



# Calculate MAE (mean absolute error) ------------------------------------------------------------


val_desc <- veg_validation %>%
  group_by(mgmt) %>%
  reframe(LG.diff = abs(mean(LG.abs.diff, na.rm = TRUE)),
            LF.diff = abs(mean(LF.abs.diff, na.rm = TRUE)),
            BG.diff = abs(mean(BG.abs.diff, na.rm = TRUE)),
            DG.diff = abs(mean(DG.abs.diff, na.rm = TRUE)))

veg_desc_ht <- veg_htvalidation %>%
  group_by(mgmt) %>%
  reframe(HT.diff = mean(abs.diff, na.rm = TRUE) * 0.1)

# Live grass
mean(val_desc$LG.diff)

# Live forbs
mean(val_desc$LF.diff)

# Bare ground
mean(val_desc$BG.diff)

# Dead grass/litter
mean(val_desc$DG.diff)

# Veg height
mean(veg_desc_ht$HT.diff)



# VALIDATE RESOURCE SELECTION -----------------------------------------


### time = 2236 is May 15, 2021
### time = 2267 is June 15, 2021
### time = 2297 is July 15, 2021

### create high res KNZ map for GRSP territory selection:
# June 2021
sp.GRSP<-func_CreatePredictiveRaster(2, samp.ws,RSF_GRSP, 2267, 20, rl.dist.to.shrubs, rl.dist.to.trees, rl.shrubs, rl.trees)
writeRaster(sp.GRSP, "GRSPPredictions_AllKNZ_6Nov2022")

#save data in data frames for plotting
df_sp.GRSP<-data.frame(rasterToPoints(sp.GRSP, spatial = TRUE))
df_knz <- fortify(ws.konza, id="NAME_1")

#plot
ggplot() +
  geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = "snow3", col = "gray10", alpha = 0.8) +
  geom_raster(data = df_sp.GRSP, aes(x = x, y = y, fill = layer), alpha = 0.7) +
  geom_polygon(data = df_knz, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
  theme_void() +
  scale_fill_viridis_c(name = "Prob.Selection")


### load 2021 GRSP data
GRSP.pts.2021<-read.csv("GRSPLocations2021.csv")

### add time data
GRSP.pts.2021$date<-as.Date(GRSP.pts.2021$date, format = "%m/%d/%Y")
GRSP.pts.2021<-left_join(GRSP.pts.2021, df.time, by = "date")
GRSP.pts.2021$time<-GRSP.pts.2021$time.x


### calculate territories 
GRSP.pts.2021 <- func_CalculateDailyCentroid(GRSP.pts.2021)

# run this code three times to aggregate nearby points
GRSP.pts.2021<-func_AggNearbyPoints(GRSP.pts.2021, 40)

### use function to calculate and save slope for each observation
# buffer is territory radius (1/2 territory size)
GRSP_data_2021<-func_SlopeRSF_2021(GRSP.pts_2021, 20, GRSP_data_2021,length(GRSP.pts_2021$bands))

### supply watershed info
GRSP_data_2021<-func_Watershed(GRSP_data_2021)

### add watershed covariates to species data frames
GRSP_data_2021 <- left_join(GRSP_data_2021, ws_cov, by = c("year","PlotName"))

### extract soil and slope data
GRSP_data_2021<-func_Elev_Soil(GRSP_data_2021)

### make management column from fire & grazing columns
GRSP_data_2021$mgmt <- paste(GRSP_data_2021$gr, GRSP_data_2021$fire, sep = "")

### add predicted vegetation to species' data frames
GRSP_data_2021<-func_VegRSF(GRSP_data_2021)

### use function to calculate and save distance to woody veg for each observation
# buffer is territory radius (i.e. 1/2 of territory size)
# GRSP
GRSP_data_2021<-func_WoodyRSF(GRSP_data_2021,20)

#remove duplicated territories 
GRSP_data_2021<-GRSP_data_2021[!duplicated(GRSP_data_2021),]


## calculate brier score
GRSP_data_2021$brier <- predict(RSF_GRSP, newdata = GRSP_data_2021, type = "response")
#brier score... closer to 0 is perfect accuracy, 1 is perfect inaccuracy
mean((GRSP_data_2021$brier-GRSP_data_2021$pres)^2, na.rm = TRUE)


