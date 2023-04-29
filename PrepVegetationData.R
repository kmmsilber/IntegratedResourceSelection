library(lubridate)
library(rgdal)
library(data.table)
library(raster)
library(dplyr)


##############################
### VEGETATION HEIGHT DATA ###
##############################

#read in data
df.veg<-read.csv("VegetationHeight_RawData.csv")

#only include transects (i.e., remove nests, telemetry, etc.)
df.veg<-df.veg[df.veg[,2]=="Transect",]

#fix formatting of first column name
colnames(df.veg)[1]<-"Point" 

#format dates
df.veg$Date<-dmy(df.veg$Date)


### multiple measurements are saved at one veg point, so we have to 
### correct the coordinates for each measurement

#separate measurements at each point into own data frame
R.CN<-df.veg[,c(1,3:7,17)] #robel measurements at center, viewed from the North
R.CE<-df.veg[,c(1,3:6,8,17)] #robel measurements at center, viewed from the East
R.CS<-df.veg[,c(1,3:6,9,17)] #robel measurements at center, viewed from the South
R.CW<-df.veg[,c(1,3:6,10,17)] #robel measurements at center, viewed from the West
R.N<-df.veg[,c(1,3:6,11,17)] #robel measurements at edge, North
R.E<-df.veg[,c(1,3:6,12,17)] #robel measurements at edge, East
R.S<-df.veg[,c(1,3:6,13,17)] #robel measurements at edge, South
R.W<-df.veg[,c(1,3:6,14,17)] #robel measurements at edge, West

#correct UTMS given measurements are taken up to 5m from central location
R.CN$PositionY<-R.CN$PositionY+2 #shift UTMS 2m North
R.CE$PositionX<-R.CN$PositionX+2 #shift UTMS 2m East
R.CS$PositionY<-R.CS$PositionY-2 #shift UTMS 2m South
R.CW$PositionX<-R.CW$PositionX-2 #shift UTMS 2m West
R.N$PositionY<-R.N$PositionY+5 #shift UTMS 5m North
R.E$PositionX<-R.N$PositionX+5 #shift UTMS 5m East
R.S$PositionY<-R.S$PositionY-5 #shift UTMS 5m South
R.W$PositionX<-R.W$PositionX-5 #shift UTMS 5m West


### create dataframe for veg heights

# combine measurements into one dataframe
veg.ht<-rbindlist(list(R.CN,R.CE,R.CS,R.CW,R.N,R.E,R.S,R.W),use.names = FALSE)

#add column names
colnames(veg.ht)<-c("point","plot","date","sx","sy","ht","obs")
veg.ht<-veg.ht[!is.na(veg.ht$ht),] #remove observations with missing measurements
veg.ht<-veg.ht[!is.na(veg.ht$sx),] #remove observations with missing locations


### add predictors

#assign grazing regime for watersheds within veg height dataframe
veg.ht$gr<-ifelse(grepl("N", veg.ht$plot), "bison",
                  ifelse(grepl("C", veg.ht$plot),"cattle", "ungr"))

burn.one<-c("K1B","N1A","N1B") #create vector of annually burned watersheds
burn.two<-c("2D","K2A","N2A","N2B") #create vector of 2-year burned watersheds
burn.thr<-c("C3A","C3B","C3C","C3SA","C3SB","C3SC") #create vector of 3-year burn watersheds

#assign burn frequencies to sample points in veg height dataframe
veg.ht$fire<-ifelse(veg.ht$plot %in% burn.one,1,
                     ifelse(veg.ht$plot %in% burn.two,2,3))

#create variables for month and year
veg.ht$month<-month(veg.ht$date)
veg.ht$year<-year(veg.ht$date)

#create binary variable for burn year
burn.hist<-read.csv("BurnHistories.csv")
burn.hist$Year <- year(as.Date(as.character(burn.hist$Year), format = "%Y"))
burn.hist<-burn.hist[,c("Watershed","Year")]
burn.hist$burnyr<-1

veg.ht<-left_join(veg.ht,burn.hist,
                    by = c("plot" = "Watershed","year" = "Year"))

veg.ht$burnyr <- ifelse(is.na(veg.ht$burnyr), 0, 1)

#extract elevation at each location
sf.elev <- raster("GIS200/GIS200.tif")
veg.pts<-veg.ht
coordinates(veg.pts) <- ~sx+sy
veg.ht$elev <- raster::extract(sf.elev, veg.pts,fun=mean, df=TRUE)[,2]

#extract slope at each location
W.s1 <- matrix(c(0,0,0,1,0,-1,0,0,0),3,3)/(2*2) 
W.s2 <- matrix(c(0,1,0,0,0,0,0,-1,0),3,3)/(2*2) 
der.elev <- sqrt(focal(sf.elev, W.s1, fun=sum)^2 + focal(sf.elev, W.s2, fun=sum)^2)
veg.ht$slope <- raster::extract(der.elev, veg.pts,fun=mean, df=TRUE)[,2]

#extract soil type at each location
sf.soils <- readOGR("GIS220/Konza_SSURGO_Soils_(GIS220).shp")
proj4string(veg.pts) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
veg.pts <- spTransform(veg.pts, crs(sf.soils))
veg.ht$soil <- raster::extract(sf.soils, veg.pts)$MUSYM

#change missing values from -9 to NA
veg.ht[veg.ht$ht < 0,]$ht <- NA

###################################
### VEGETATION COMPOSITION DATA ###
###################################

#read in data
daub<-read.csv("VegetationComposition_RawData.csv")

#only include transects (i.e., remove nests, telemetry, etc.)
daub<-daub[daub[,2]=="Transect",]

#fix formatting of first column name
colnames(daub)[1]<-("Point")

#format dates
daub$Date<-dmy(daub$Date)


### fix measurement coordinates; some frames are sampled 5m from the central point

#separate measurements at each point into own data frame
D.N<-daub[daub[,7]=="N",] #robel measurements at North
D.E<-daub[daub[,7]=="E",] #robel measurements at East
D.S<-daub[daub[,7]=="S",] #robel measurements at South
D.W<-daub[daub[,7]=="W",] #robel measurements at West

#shift points 5m from center point
D.N$PositionY<-D.N$PositionY+5 #shift UTMS 5m North
D.E$PositionX<-D.E$PositionX+5 #shift UTMS 5m East
D.S$PositionY<-D.S$PositionY-5 #shift UTMS 5m South
D.W$PositionX<-D.W$PositionX-5 #shift UTMS 5m West


###construct dataframes

#combine all points into one data frame
veg.comp<-rbind(daub[daub[,7]=="Center",],D.N,D.E,D.S,D.W)

#remove observations with missing locations
veg.comp<-veg.comp[!is.na(veg.comp$PositionX),] 

###combine litter and dead grass since they biologically serve similar functions
veg.comp$X.DeadAndLitter<-veg.comp$X.DeadGrass+veg.comp$X.Litter

###add predictors

#assign grazing regime for watersheds within veg height dataframe
veg.comp$gr<-ifelse(grepl("N", veg.comp$PlotName), "bison",
                  ifelse(grepl("C", veg.comp$PlotName),"cattle", "ungr"))

#assign burn frequencies to sample points in veg composition dataframe
veg.comp$fire<-ifelse(veg.comp$PlotName %in% burn.one,1,
                     ifelse(veg.comp$PlotName %in% burn.two,2,3))

#assign month (i.e., time of season)to sample points in veg composition dataframe
veg.comp$month<-month(veg.comp$Date)
veg.comp$year<-year(veg.comp$Date)

#add burn year as binary variable
veg.comp<-left_join(veg.comp,burn.hist,
                    by = c("PlotName" = "Watershed","year" = "Year"))

veg.comp$burnyr <- ifelse(is.na(veg.comp$burnyr), 0, 1)

#extract elevation at each location
veg.pts<-veg.comp
coordinates(veg.pts) <- ~PositionX+PositionY
veg.comp$elev <- raster::extract(sf.elev, veg.pts,fun=mean, df=TRUE)[,2]

#extract soil type at each location
proj4string(veg.pts) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83")
veg.pts <- spTransform(veg.pts, crs(sf.soils))
veg.comp$soil <- raster::extract(sf.soils, veg.pts)$MUSYM

#extract slope at each location
veg.comp$slope <- raster::extract(der.elev, veg.pts,fun=mean, df=TRUE)[,2]

###create percentages from column values
veg.comp$X.LiveGrass<-veg.comp$X.LiveGrass/100
veg.comp$X.LiveForb<-veg.comp$X.LiveForb/100
veg.comp$X.DeadAndLitter<-veg.comp$X.DeadAndLitter/100
veg.comp$X.Bare<-veg.comp$X.Bare/100


###write function to calculate evenness
calc.evenness<-function(live,forb,dead,bare){
  a<-ifelse(live!=0,live*log(live),0)
  b<-ifelse(forb!=0,forb*log(forb),0)
  c<-ifelse(dead!=0,dead*log(dead),0)
  d<-ifelse(bare!=0,bare*log(bare),0)
  ((a+b+c+d)*(-1))/log(4)
}


###calculate evenness across live grass, dead grass/litter, and bare ground
veg.comp$evenness<-calc.evenness(veg.comp$X.LiveGrass,veg.comp$X.LiveForb,
                                 veg.comp$X.DeadAndLitter,veg.comp$X.Bare)

###assign presence/absence of cover type using binomial distribution
veg.comp$pres.grass<-ifelse(veg.comp$X.LiveGrass == 0,0,1) #assign present or absent for live grass
veg.comp$pres.forb<-ifelse(veg.comp$X.LiveForb == 0,0,1) #assign present or absent for live forbs
veg.comp$pres.dead<-ifelse(veg.comp$X.DeadAndLitter == 0,0,1) #assign present or absent for dead grass
veg.comp$pres.bare<-ifelse(veg.comp$X.Bare == 0,0,1) #assign present or absent for bare ground

###increase 0 values to .1% to fit beta distribution
veg.comp$X.LiveGrass[is.na(veg.comp$X.LiveGrass)] <- 0
veg.comp$X.LiveGrass[veg.comp$X.LiveGrass == 0] <- veg.comp$X.LiveGrass[veg.comp$X.LiveGrass == 0] + 0.001
veg.comp$X.LiveGrass[veg.comp$X.LiveGrass == 1] <- veg.comp$X.LiveGrass[veg.comp$X.LiveGrass == 1] - 0.001
veg.comp$X.LiveForb[is.na(veg.comp$X.LiveForb)] <- 0
veg.comp$X.LiveForb[veg.comp$X.LiveForb == 0] <- veg.comp$X.LiveForb[veg.comp$X.LiveForb == 0] + 0.001
veg.comp$X.LiveForb[veg.comp$X.LiveForb == 1] <- veg.comp$X.LiveForb[veg.comp$X.LiveForb == 1] - 0.001
veg.comp$X.DeadAndLitter[is.na(veg.comp$X.DeadAndLitter)] <- 0
veg.comp$X.DeadAndLitter[veg.comp$X.DeadAndLitter == 0] <- veg.comp$X.DeadAndLitter[veg.comp$X.DeadAndLitter == 0] + 0.001
veg.comp$X.DeadAndLitter[veg.comp$X.DeadAndLitter == 1] <- veg.comp$X.DeadAndLitter[veg.comp$X.DeadAndLitter == 1] - 0.001
veg.comp$X.Bare[is.na(veg.comp$X.Bare)] <- 0
veg.comp$X.Bare[veg.comp$X.Bare == 0] <- veg.comp$X.Bare[veg.comp$X.Bare == 0] + 0.001
veg.comp$X.Bare[veg.comp$X.Bare == 1] <- veg.comp$X.Bare[veg.comp$X.Bare == 1] - 0.001
veg.comp$evenness[veg.comp$evenness == 0] <- 0.001
veg.comp$evenness[veg.comp$evenness >= 1] <- 0.999

### remove unneccessary columns 
veg.comp<-veg.comp[,-c(2,7)]

#veg.ht: dataframe includes vegetation heights
write.csv(veg.ht,"VegetationHeight_CleanedData.csv")
#veg.comp: dataframe includes vegetation composition and pres/abs
write.csv(veg.comp,"VegetationComposition_CleanedData.csv")

