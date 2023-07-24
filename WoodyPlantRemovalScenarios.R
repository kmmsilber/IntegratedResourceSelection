

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


# PLOT ALL REMOVAL SCENARIOS --------------------------------------------------------------------

# woody encroachment
Shrub_ha <- data.frame(Unit = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Bison-grazed, 1-year fire return", "Patch-burn grazed"), 
                       enc = c(1.03, 5.13, 9.93, 16.94),
                       cover = "Shrub")

Shrub_perc <- data.frame(Unit = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Bison-grazed, 1-year fire return", "Patch-burn grazed"), 
                         enc = c(2.1, 2.9, 8.2, 7.7),
                         cover = "Shrub")

Tree_ha <- data.frame(Unit = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Bison-grazed, 1-year fire return", "Patch-burn grazed"), 
                      enc = c(0.05, 4.56, 5.80, 7.70),
                      cover = "Tree")

Tree_perc <- data.frame(Unit = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Bison-grazed, 1-year fire return", "Patch-burn grazed"), 
                        enc = c(0.1, 2.5, 4.8, 3.5),
                        cover = "Tree")

woody_enc <- data.frame(rbind(Shrub_perc, Tree_perc))

woody_enc$Unit <- factor(woody_enc$Unit, levels = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Patch-burn grazed", "Bison-grazed, 1-year fire return"))

enc_plot <- ggplot(data = woody_enc, aes(x = cover, y = enc, fill = Unit)) + 
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  theme_pubr() +
  theme(legend.position = "right") +
  ylab("Area encroached  (% of pasture)") +
  xlab("") +
  scale_x_discrete(labels =  c("Shrubs", "Trees")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.7, size = 12), text = element_text(size = 13)) +
  scale_fill_manual(values = c("palegreen3", "palegreen4","navajowhite3", "lightsalmon4"), name = "Management Regime")


# hectares improvement
df_removal <- data.frame(rbind(rem_2D, rem_K1B, rem_N1B, rem_PBG))


df_removal$Mgmt <- factor(df_removal$Mgmt, levels = c("Ungrazed, 2-year fire return", "Ungrazed, 1-year fire return", "Patch-burn grazed", "Bison-grazed, 1-year fire return"))

improv_plot <- ggplot() + 
  geom_point(data = df_removal, aes(x = scenario, y = perc * 100, col = Mgmt), size = 5, alpha = 0.8) +
  theme_pubr() +
  theme(legend.position = "right") +
  ylab("Area improved  (% of pasture)") +
  xlab("") +
  scale_x_discrete(labels =  c("One shrub", "One tree", "All uplands shrubs", "All uplands trees")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.7, size = 12), text = element_text(size = 13)) +
  scale_color_manual(values = c("palegreen3", "palegreen4","navajowhite3", "lightsalmon4"), name = "Management Regime")

ggarrange(enc_plot, improv_plot, nrow = 1, ncol = 2, labels = c("A","B"), common.legend = TRUE, legend = "none", align = "hv")


