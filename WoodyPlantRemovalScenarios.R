

##### create predictive rasters for each watershed #####

# function syntax:
#   func_WoodyRemovalExperiment<-function(name.ws, file.name, map.resolution, model, time.input, buffer,
#                                      shrub_island_xmin, shrub_island_xmax, shrub_island_ymin, shrub_island_ymax,
#                                      upland_tree_xmin, upland_tree_xmax, upland_tree_ymin, upland_tree_ymax)



# create and save plots for removal scenarios -----------------------------



##### 2D #####

func_WoodyRemovalExperiment("2D", "2D", 30, RSF_GRSP, 2267, 20,
                            711260, 711330, 4328090, 4328145,
                            711340, 711380, 4328750, 4328900)


##### PBG #####

func_WoodyRemovalExperiment(c("C3A","C3B","C3C"), "PBG", 2, RSF_GRSP, 2267, 20,
                            711650, 711750, 4329750, 4329850,
                            711400, 711500, 4328740, 4328760)



##### K1B #####

func_WoodyRemovalExperiment("K1B", "K1B", 2, RSF_GRSP, 2267, 20,
                            710750, 710850, 4330150, 433250,
                            711350, 711400, 4329000, 4329100)


##### N1B #####

func_WoodyRemovalExperiment("N1B", "N1B", 200, RSF_GRSP, 2267, 20,
                            710500, 710550, 4328250, 4328320,
                            709900, 710000, 4329000, 4329050)




# make plot of removal scenario ------------------------------------------

#load original raster produced by above code
#files are saved as WatershedName_Predictions_Date.gri, like below
sp.ws <- raster("2D_Predictions_Apr28.gri")

#load original raster produced by above code
#files are saved as WatershedName_Predictions_RemovalScenario_Date.gri, like below
sp.ws <- raster("2D_Predictions_TreeRemoval_Apr28.gri")

##### make plot of tree removal #####
#removal of one shrub
sp.ws.rem <- sp.ws
sp.ws.rem[] <- sp.ws.rem.shrubs[] - sp.ws.rem[] 
sp.ws.rem[sp.ws.rem<0]<-0
sp.ws.rem[] <- sp.ws.rem[] * 100
sp.ws.rem[sp.ws.rem[] >39] <- 40
df_sp.ws.rem<-data.frame(rasterToPoints(sp.ws.rem))

#plot predictions
plot_remshrub <- ggplot() +
  #geom_raster(data = df_sp.ws.rem, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = df_knz.PBG, aes(x = long, y = lat, group = group), fill = NA, col = "gray10") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colors = colorRampPalette(c("white","goldenrod1","orange", "sienna1"))(40), limits = c(0,40)) +
  labs(fill='% gained') +
  coord_fixed()


