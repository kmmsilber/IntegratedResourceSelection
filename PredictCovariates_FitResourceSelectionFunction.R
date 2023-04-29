

# load packages
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

### * all functions are saved in "RSF_Functions.R" * ###

#### PREP DATA ####

### load Konza map
KNZ<- readOGR("./GIS001/GIS001.shp") #shapefile for konza site boundary

### load watersheds and exclude unsampled watersheds
ws.konza <- readOGR("./GIS032/GIS032.shp") #shapefile for konza watersheds

samp.ws<-c("2D","C3A","C3B","C3C","C3SA","C3SB","C3SC",
           "K1B","K2A","N1A","N1B","N2A","N2B")

knz.samp.sites<-ws.konza[which(ws.konza$NAME_1 %in% samp.ws),]

### load bird data (choose one of the species below)
bird_data<-read.csv("GrasshopperSparrowData.csv")
bird_data<-read.csv("EasternMeadowlarkData.csv")
bird_data<-read.csv("DickcisselData.csv")

### create numeric time variable
date.range<-seq(as.Date("1-Apr-14", format = "%d-%b-%y"), as.Date("1-Sep-21", format = "%d-%b-%y"), by="days")
time.range<-c(0:as.numeric(difftime(max(date.range),min(date.range),"1977-12-16 00:00:00 UTC",units="days")))
df.time<-data.frame(date = date.range,time = time.range)
df.time$month <- month(df.time$date)
df.time$year <- year(df.time$date)

### add time to bird data
bird_data$date<-as.Date(bird_data$date, format = "%m/%d/%Y")
bird_data<-left_join(bird_data, df.time, by = "date")


### create territories for each species, buffer based on territory size
# (20 for Grasshopper Sparrows, 30 for Dickcissels, 50 for Eastern Meadowlarks)

# select buffer to create territories
buffer <- 20
bird.pts<-func_CalculateDailyCentroid(bird_data,buffer)


### if point within same year and within expected territory size, find centroid

bird.pts<-func_AggNearbyPoints(bird.pts, buffer)


#### PREP DATA: ADD SLOPE ####

bird_data<-func_SlopeRSF(bird.pts, buffer, bird_data, nrow(bird_data))


### PREP DATA: ADD LAND MANAGEMENT AND PRECIP COVARIATES ###

bird_data<-func_Watershed(bird_data)


#### PREP DATA: ADD COVARIATES TO SPECIES DATA ####

### load precip data
veg.precip<-read.csv("KNZPrecip_Dec9.csv")

### add precip and time to veg.ht
veg.ht<-left_join(veg.ht, veg.precip, by="year")
veg.ht$date<-as.Date(veg.ht$date, format = "%m/%d/%Y")
veg.ht<-left_join(veg.ht, df.time, by = "date")
veg.ht$month<-veg.ht$month.x

### add precip and time to veg.comp
veg.comp<-left_join(veg.comp, veg.precip, by="year")
veg.comp$date<-as.Date(veg.comp$date, format = "%m/%d/%Y")
veg.comp<-left_join(veg.comp, df.time, by = "date")
veg.comp$month<-veg.comp$month.x

### create a data frame with watershed data
ws_cov<-veg.comp[,c("year","PlotName","gr","fire","burnyr","Spring",
                    "BrLagOne","BrLagTwo")]
ws_cov<-ws_cov[!duplicated(ws_cov), ]

### add a month and year column to species data frames
bird_data$date<-left_join(bird_data, df.time, by = "time")$date.y
bird_data$year<-year(bird_data$date)

### add covariates to data frames
bird_data <- left_join(bird_data, ws_cov, by = c("year","PlotName"))

### add month as a covariate
bird_data$month<-month(bird_data$date)


#### PREP DATA: VEGETATION COVER MODELS ####

### fit vegetation height model 
HT.mod<-gam(ht ~ Spring + BrLagOne + BrLagTwo + month + gr + as.factor(fire) + 
              burnyr + s(obs, bs = "re") + s(time,bs="gp") + 
              s(sx,sy, bs = "gp"),family = tw(link="log"), 
            data=veg.ht, na.action = na.exclude)

summary(HT.mod)

### fit live grass model
LG.mod<-gam(X.LiveGrass ~ Spring + BrLagOne + BrLagTwo + month + gr + as.factor(fire) + 
              burnyr + s(obs, bs = "re") + s(time,bs="gp") + 
              s(sx,sy, bs = "gp"),family = betar(link="logit"), 
            data=veg.comp, na.action = na.exclude)

summary(LG.mod)

### fit live forbs model
LF.mod<-gam(X.LiveForb ~ Spring + BrLagOne + BrLagTwo + month + gr + as.factor(fire) + 
              burnyr + s(obs, bs = "re") + s(time,bs="gp") + 
              s(sx,sy, bs = "gp"),family = betar(link="logit"), 
            data=veg.comp, na.action = na.exclude)

summary(LF.mod)

### fit bare ground model
BG.mod<-gam(X.Bare ~ Spring + BrLagOne + BrLagTwo + month + gr + as.factor(fire) + 
              burnyr + s(obs, bs = "re") + s(time,bs="gp") + 
              s(sx,sy, bs = "gp"),family = betar(link="logit"), 
            data=veg.comp, na.action = na.exclude)

summary(BG.mod)

### fit dead veg model
DG.mod<-gam(X.DeadAndLitter ~ Spring + BrLagOne + BrLagTwo + month + gr + as.factor(fire) + 
              burnyr + s(obs, bs = "re") + s(time,bs="gp") + 
              s(sx,sy, bs = "gp"),family = betar(link="logit"), 
            data=veg.comp, na.action = na.exclude)

summary(BG.mod)

### add predicted vegetation to species' data frames
bird_data<-func_VegRSF(bird_data)


#### PREP DATA: DISTANCE TO WOODY VEGETATION ####

### use function to calculate and save woody vegetation predictions for each observation
bird_data<-func_WoodyRSF(bird_data, buffer, rl.dist.to.shrubs, rl.dist.to.trees, rl.shrubs, rl.trees)


#### FIT RESOURCE SELECTION MODEL ####

RSF_mod <- gam(pres ~ slope + HT + LG + LF + BG + DG + DistToShrubs + DistToTrees + s(time, bs = "gp"),
                family=binomial(link = "logit"),data=bird_data)
summary(RSF_mod)

# for later code, each species is saved in own model:
### Grasshopper Sparrows = RSF_GRSP
### Eastern Meadowlarks = RSF_EAME
### Dickcissels = RSF_DICK


