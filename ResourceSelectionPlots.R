
#### PLOT BETA ESTIMATES ####

# GRSP
Vars <- c("Intercept","Slope","VegHeight", "LiveGrass","LiveForbs","BareGround","DeadVeg",
          "DistToShrubs","DistToTrees", "PropShrubs", "PropTrees")

df.GRSP<-rbind(data.frame(Var = Vars,
                          Beta_Est = RSF_GRSP$coef[1:11], 
                          Beta_Lower = RSF_GRSP$coef[1:11] - (2 * summary(RSF_GRSP)$p.table[,2]),
                          Beta_Upper = RSF_GRSP$coef[1:11] + (2 * summary(RSF_GRSP)$p.table[,2])))

df.GRSP<-df.GRSP[-1,]

GRSP.plot <- ggplot(df.GRSP) +
  geom_point(aes(x=Var, y = Beta_Est), position = position_dodge(0.5),
             size = 3.5, col = "darkgoldenrod")  +
  geom_linerange(aes(x = Var, ymin=Beta_Lower, ymax=Beta_Upper),
                 position = position_dodge(0.5),size = 1.2,alpha = 0.8,
                 col = "darkgoldenrod") +
  geom_hline(yintercept=0, col = "gray40", linetype = "dashed") + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),legend.position = "right",legend.title = element_blank()) +
  ylim(-90,60) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip()

# EAME
df.EAME<-rbind(data.frame(Var = Vars,
                          Beta_Est = RSF_EAME$coef[1:11], 
                          Beta_Lower = RSF_EAME$coef[1:11] - (2 * summary(RSF_EAME)$p.table[,2]),
                          Beta_Upper = RSF_EAME$coef[1:11] + (2 * summary(RSF_EAME)$p.table[,2])))

df.EAME<-df.EAME[-1,]

EAME.plot <- ggplot(df.EAME) +
  geom_point(aes(x=Var, y = Beta_Est), position = position_dodge(0.5),size = 3.5, col = "skyblue3")  +
  geom_linerange(aes(x = Var, ymin=Beta_Lower, ymax=Beta_Upper),position = position_dodge(0.5),size = 1.2,alpha = 0.8, col = "skyblue3") +
  geom_hline(yintercept=0, col = "gray40", linetype = "dashed") + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),legend.position = "right",legend.title = element_blank()) +
  ylim(-90,60) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip()



# DICK
df.DICK<-rbind(data.frame(Var = Vars,
                          Beta_Est = RSF_DICK$coef[1:11], 
                          Beta_Lower = RSF_DICK$coef[1:11] - (2 * summary(RSF_DICK)$p.table[,2]),
                          Beta_Upper = RSF_DICK$coef[1:11] + (2 * summary(RSF_DICK)$p.table[,2])))

df.DICK<-df.DICK[-1,]

DICK.plot <- ggplot(df.DICK) +
  geom_point(aes(x=Var, y = Beta_Est), position = position_dodge(0.5),size = 3.5, col = "darkolivegreen4")  +
  geom_linerange(aes(x = Var, ymin=Beta_Lower, ymax=Beta_Upper),position = position_dodge(0.5),size = 1.2,alpha = 0.8, col = "darkolivegreen4") +
  geom_hline(yintercept=0, col = "gray40", linetype = "dashed") + 
  theme_minimal() +
  theme(legend.position = "right",legend.title = element_blank()) +
  ylim(-90,60) +
  ylab("Beta Coefficient") +
  xlab(NULL) +
  coord_flip()

AllSpp.plot<-ggarrange(GRSP.plot, EAME.plot, DICK.plot, labels = c("A","B","C"),
                       ncol = 1, nrow = 3)

AllSpp.plot

############# PLOT DIST TO WOODY VEG ###############

### create data frame to predict distance to shrubs
pred.DistToShrubs<-data.frame(slope = 0.2,
                              HT = 3,
                              LG = 0.3,
                              LF = .2,
                              BG = .15,
                              DG = .1,
                              DistToShrubs = seq(0,0.4,0.01),
                              DistToTrees = .2,
                              PropShrubs = 0.15,
                              PropTrees = 0,
                              obs = 0,
                              time = 2253)

### predict based on model and calculate 95% CI

#GRSP
grsp.ests <- data.frame(summary(RSF_GRSP)$coefficients)
g_linkinv <- family(RSF_GRSP)$linkinv


GRSP.pres.pred.shrubs <- bind_cols(pred.DistToShrubs, setNames(as_tibble(predict(RSF_GRSP, pred.DistToShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.shrubs <- mutate(GRSP.pres.pred.shrubs,
                                fit_resp = g_linkinv(fit_link),
                                ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.shrubs$spp <- "Grasshopper Sparrow"

#EAME
eame.ests <- data.frame(summary(RSF_EAME)$coefficients)
e_linkinv <- family(RSF_EAME)$linkinv

EAME.pres.pred.shrubs <- bind_cols(pred.DistToShrubs, setNames(as_tibble(predict(RSF_EAME, pred.DistToShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.shrubs <- mutate(EAME.pres.pred.shrubs,
                                fit_resp = e_linkinv(fit_link),
                                ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.shrubs$spp <- "Eastern Meadowlark"

#DICK
dick.ests <- data.frame(summary(RSF_DICK)$coefficients)
d_linkinv <- family(RSF_DICK)$linkinv

DICK.pres.pred.shrubs <- bind_cols(pred.DistToShrubs, setNames(as_tibble(predict(RSF_DICK, pred.DistToShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.shrubs <- mutate(DICK.pres.pred.shrubs,
                                fit_resp = d_linkinv(fit_link),
                                ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.shrubs$spp <- "Dickcissel"

All.spp.DistShrubs <- rbind(GRSP.pres.pred.shrubs, EAME.pres.pred.shrubs, DICK.pres.pred.shrubs)

### plot distance to shrubs for all species
distshrubs_plot <- 
  ggplot() + 
  geom_line(data = All.spp.DistShrubs, aes(x=DistToShrubs * 1000, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = All.spp.DistShrubs, aes(x = DistToShrubs * 1000, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.1) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Distance to Shrubs (m)") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")



### create data frame to predict distance to trees
pred.DistToTrees<-data.frame(slope = 0.2,
                             HT = 3,
                             LG = 0.3,
                             LF = .2,
                             BG = .15,
                             DG = .1,
                             DistToTrees = seq(0,0.4,0.01),
                             DistToShrubs = 0.05,
                             PropShrubs = 0,
                             PropTrees = 0,
                             obs = 0,
                             time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.trees <- bind_cols(pred.DistToTrees, setNames(as_tibble(predict(RSF_GRSP, pred.DistToTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.trees <- mutate(GRSP.pres.pred.trees,
                                fit_resp = g_linkinv(fit_link),
                                ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.trees$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.trees <- bind_cols(pred.DistToTrees, setNames(as_tibble(predict(RSF_EAME, pred.DistToTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.trees <- mutate(EAME.pres.pred.trees,
                                fit_resp = e_linkinv(fit_link),
                                ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.trees$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.trees <- bind_cols(pred.DistToTrees, setNames(as_tibble(predict(RSF_DICK, pred.DistToTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.trees <- mutate(DICK.pres.pred.trees,
                                fit_resp = d_linkinv(fit_link),
                                ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.trees$spp <- "Dickcissel"

All.spp.DistTrees <- rbind(GRSP.pres.pred.trees, EAME.pres.pred.trees, DICK.pres.pred.trees)

### plot distance to trees for all species
disttrees_plot <- 
ggplot() + 
  geom_line(data = All.spp.DistTrees, aes(x=DistToTrees * 1000, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = All.spp.DistTrees, aes(x = DistToTrees * 1000, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.1) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Distance to trees (m)") +
  ylab("Probability of territory selection") +
  ylim(0,1) +
  theme(legend.position = "right")

### Plot shrubs & trees together

GRSP.woody<-ggplot() + 
  geom_line(data = GRSP.pres.pred.shrubs, aes(x=DistToShrubs*1000, y=fit_resp), col = "goldenrod", size =1.25) +
  geom_line(data = GRSP.pres.pred.trees, aes(x=DistToTrees*1000, y=fit_resp), col = "goldenrod4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = GRSP.pres.pred.shrubs, aes(x = DistToShrubs*1000, ymin=ci_lwr, ymax=ci_upr), fill = "goldenrod", alpha = 0.2) +
  geom_ribbon(data = GRSP.pres.pred.trees, aes(x = DistToTrees*1000, ymin=ci_lwr, ymax=ci_upr), fill = "goldenrod4", alpha = 0.2) +
  theme_pubr() +
  scale_x_continuous(labels = NULL) +
  xlab(" ") +
  ylab("Prob.Selection") +
  ylim(0,1)


EAME.woody<-ggplot() + 
  geom_line(data = EAME.pres.pred.shrubs, aes(x=DistToShrubs*1000, y=fit_resp), col = "darkcyan", size =1.25) +
  geom_line(data = EAME.pres.pred.trees, aes(x=DistToTrees*1000, y=fit_resp), col = "lightblue4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = EAME.pres.pred.shrubs, aes(x = DistToShrubs*1000, ymin=ci_lwr, ymax=ci_upr), fill = "darkcyan", alpha = 0.2) +
  geom_ribbon(data = EAME.pres.pred.trees, aes(x = DistToTrees*1000, ymin=ci_lwr, ymax=ci_upr), fill = "lightblue4", alpha = 0.2) +
  theme_pubr() +
  scale_x_continuous(labels = NULL) +
  xlab(" ") +
  ylab("Prob.Selection") +
  ylim(0,1)

DICK.woody<-ggplot() + 
  geom_line(data = DICK.pres.pred.shrubs, aes(x=DistToShrubs*1000, y=fit_resp), col = "darkolivegreen4", size =1.25) +
  geom_line(data = DICK.pres.pred.trees, aes(x=DistToTrees*1000, y=fit_resp), col = "palegreen4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = DICK.pres.pred.shrubs, aes(x = DistToShrubs*1000, ymin=ci_lwr, ymax=ci_upr), fill = "darkolivegreen4", alpha = 0.4) +
  geom_ribbon(data = DICK.pres.pred.trees, aes(x = DistToTrees*1000, ymin=ci_lwr, ymax=ci_upr), fill = "palegreen4", alpha = 0.3) +
  theme_pubr() +
  theme(legend.position = c(0.65,0.1)) +
  xlab("Distance to Woody Vegetation (m)") +
  ylab("Prob.Selection  ") +
  ylim(0,1)

ggarrange(GRSP.woody, EAME.woody, DICK.woody, nrow = 3, ncol = 1, labels = c("A","B","C"))



############# PLOT PROPORTION OF WOODY VEG ###############

### create data frame to predict distance to shrubs
pred.PropShrubs<-data.frame(slope = 0.2,
                            HT = 3,
                            LG = 0.3,
                            LF = .2,
                            BG = .15,
                            DG = .1,
                            DistToShrubs = 0.05,
                            DistToTrees = .25,
                            PropShrubs = seq(0,0.8,0.01),
                            PropTrees = 0.01,
                            obs = 0,
                            time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.shrubs <- bind_cols(pred.PropShrubs, setNames(as_tibble(predict(RSF_GRSP, pred.PropShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.shrubs <- mutate(GRSP.pres.pred.shrubs,
                                fit_resp = g_linkinv(fit_link),
                                ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.shrubs$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.shrubs <- bind_cols(pred.PropShrubs, setNames(as_tibble(predict(RSF_EAME, pred.PropShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.shrubs <- mutate(EAME.pres.pred.shrubs,
                                fit_resp = e_linkinv(fit_link),
                                ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.shrubs$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.shrubs <- bind_cols(pred.PropShrubs, setNames(as_tibble(predict(RSF_DICK, pred.PropShrubs, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.shrubs <- mutate(DICK.pres.pred.shrubs,
                                fit_resp = d_linkinv(fit_link),
                                ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.shrubs$spp <- "Dickcissel"

All.spp.PropShrubs <- rbind(GRSP.pres.pred.shrubs, EAME.pres.pred.shrubs, DICK.pres.pred.shrubs)

### plot distance to shrubs for all species
propshrubs_plot <- ggplot() + 
  geom_line(data = All.spp.PropShrubs, aes(x=PropShrubs, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = All.spp.PropShrubs, aes(x = PropShrubs, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.1) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Proportion of shrubs") +
  ylab(" ") +
  ylim(0,1) +
  theme(legend.position = "right", axis.text.y = element_blank())

ggarrange(distshrubs_plot, propshrubs_plot, nrow = 1, ncol = 2, labels = c("A", "B"), 
          common.legend = TRUE, legend = "top")

### create data frame to predict distance to trees
pred.PropTrees<-data.frame(slope = 0.2,
                           HT = 3,
                           LG = 0.3,
                           LF = .2,
                           BG = .15,
                           DG = .1,
                           DistToShrubs = 0.05,
                           DistToTrees = .25,
                           PropShrubs = 0.1,
                           PropTrees = seq(0,0.8,0.01),
                           obs = 0,
                           time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.trees <- bind_cols(pred.PropTrees, setNames(as_tibble(predict(RSF_GRSP, pred.PropTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.trees <- mutate(GRSP.pres.pred.trees,
                                fit_resp = g_linkinv(fit_link),
                                ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.trees$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.trees <- bind_cols(pred.PropTrees, setNames(as_tibble(predict(RSF_EAME, pred.PropTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.trees <- mutate(EAME.pres.pred.trees,
                                fit_resp = e_linkinv(fit_link),
                                ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.trees$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.trees <- bind_cols(pred.PropTrees, setNames(as_tibble(predict(RSF_DICK, pred.PropTrees, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.trees <- mutate(DICK.pres.pred.trees,
                                fit_resp = d_linkinv(fit_link),
                                ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                                ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.trees$spp <- "Dickcissel"

All.spp.PropTrees <- rbind(GRSP.pres.pred.trees, EAME.pres.pred.trees, DICK.pres.pred.trees)

### plot distance to shrubs for all species
proptrees_plot <- ggplot() + 
  geom_line(data = All.spp.PropTrees, aes(x=PropTrees, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = All.spp.PropTrees, aes(x = PropTrees, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.1) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Proportion of trees") +
  ylab(" ") +
  ylim(0,1) +
  theme(legend.position = "right", axis.text.y = element_blank())

ggarrange(disttrees_plot, proptrees_plot, nrow = 1, ncol = 2, labels = c("A", "B"), 
          common.legend = TRUE, legend = "top")


### Plot shrubs & trees together

GRSP.woody.prop<-ggplot() + 
  geom_line(data = GRSP.pres.pred.shrubs, aes(x=PropShrubs*100, y=fit_resp), col = "goldenrod", size =1.25) +
  geom_line(data = GRSP.pres.pred.trees, aes(x=PropTrees*100, y=fit_resp), col = "goldenrod4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = GRSP.pres.pred.shrubs, aes(x = PropShrubs*100, ymin=ci_lwr, ymax=ci_upr), fill = "goldenrod", alpha = 0.2) +
  geom_ribbon(data = GRSP.pres.pred.trees, aes(x = PropTrees*100, ymin=ci_lwr, ymax=ci_upr), fill = "goldenrod4", alpha = 0.2) +
  theme_pubr() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") 
#ylim(0,1)


EAME.woody.prop<-ggplot() + 
  geom_line(data = EAME.pres.pred.shrubs, aes(x=PropShrubs*100, y=fit_resp), col = "darkcyan", size =1.25) +
  geom_line(data = EAME.pres.pred.trees, aes(x=PropTrees*100, y=fit_resp), col = "lightblue4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = EAME.pres.pred.shrubs, aes(x = PropShrubs*100, ymin=ci_lwr, ymax=ci_upr), fill = "darkcyan", alpha = 0.2) +
  geom_ribbon(data = EAME.pres.pred.trees, aes(x = PropTrees*100, ymin=ci_lwr, ymax=ci_upr), fill = "lightblue4", alpha = 0.2) +
  theme_pubr() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") 
#ylim(0,1)

DICK.woody.prop<-ggplot() + 
  geom_line(data = DICK.pres.pred.shrubs, aes(x=PropShrubs*100, y=fit_resp), col = "darkolivegreen4", size =1.25) +
  geom_line(data = DICK.pres.pred.trees, aes(x=PropTrees*100, y=fit_resp), col = "darkseagreen4", size =1.25, linetype = "dashed") +
  geom_ribbon(data = DICK.pres.pred.shrubs, aes(x = PropShrubs*100, ymin=ci_lwr, ymax=ci_upr), fill = "darkolivegreen4", alpha = 0.2) +
  geom_ribbon(data = DICK.pres.pred.trees, aes(x = PropTrees*100, ymin=ci_lwr, ymax=ci_upr), fill = "darkseagreen4", alpha = 0.5) +
  theme_pubr() +
  theme(legend.position = c(0.65,0.1)) +
  scale_y_continuous(labels = NULL) +
  xlab("Proportion of Woody Vegetation (%)") +
  ylab("") +
  #ylim(0,1) +
  xlim(0,80)

ggarrange(GRSP.woody.prop, EAME.woody.prop,DICK.woody.prop, nrow = 3, ncol = 1, labels = c("A","B","C"))


ggarrange(GRSP.woody, GRSP.woody.prop, EAME.woody, EAME.woody.prop, DICK.woody, DICK.woody.prop,
          nrow = 3, ncol = 2, labels = c("A","B","C","D","E","F"))

##### PLOT LIVE GRASS #####

pred.LG<-data.frame(LG = seq(0,1,0.01),
                    slope = 0.2,
                    HT = 3,
                    LF = .2,
                    BG = .15,
                    DG = .1,
                    DistToShrubs = 0.05,
                    DistToTrees = .25,
                    PropShrubs = 0.1,
                    PropTrees = 0.01,
                    obs = 0,
                    time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.LG <- bind_cols(pred.LG, setNames(as_tibble(predict(RSF_GRSP, pred.LG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.LG <- mutate(GRSP.pres.pred.LG,
                               fit_resp = g_linkinv(fit_link),
                               ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.LG$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.LG <- bind_cols(pred.LG, setNames(as_tibble(predict(RSF_EAME, pred.LG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.LG <- mutate(EAME.pres.pred.LG,
                               fit_resp = e_linkinv(fit_link),
                               ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.LG$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.LG <- bind_cols(pred.LG, setNames(as_tibble(predict(RSF_DICK, pred.LG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.LG <- mutate(DICK.pres.pred.LG,
                               fit_resp = d_linkinv(fit_link),
                               ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.LG$spp <- "Dickcissel"

### plot live grass ###
AllSpp.LG<-ggplot() + 
  geom_line(data = GRSP.pres.pred.LG, aes(x=LG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.LG, aes(x = LG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.LG, aes(x=LG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.LG, aes(x = LG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.LG, aes(x=LG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.LG, aes(x = LG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("% Live Grass") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.LG



##### PLOT LIVE FORBS #####

pred.LF<-data.frame(LF = seq(0,1,0.01),
                    slope = 0.2,
                    HT = 3,
                    LG = 0.3,
                    BG = .15,
                    DG = .1,
                    DistToShrubs = 0.05,
                    DistToTrees = .25,
                    PropShrubs = 0.1,
                    PropTrees = 0.01,
                    obs = 0,
                    time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.LF <- bind_cols(pred.LF, setNames(as_tibble(predict(RSF_GRSP, pred.LF, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.LF <- mutate(GRSP.pres.pred.LF,
                            fit_resp = g_linkinv(fit_link),
                            ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.LF$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.LF <- bind_cols(pred.LF, setNames(as_tibble(predict(RSF_EAME, pred.LF, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.LF <- mutate(EAME.pres.pred.LF,
                            fit_resp = e_linkinv(fit_link),
                            ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.LF$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.LF <- bind_cols(pred.LF, setNames(as_tibble(predict(RSF_DICK, pred.LF, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.LF <- mutate(DICK.pres.pred.LF,
                            fit_resp = d_linkinv(fit_link),
                            ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.LF$spp <- "Dickcissel"

### plot live forbs ###
AllSpp.LF<-ggplot() + 
  geom_line(data = GRSP.pres.pred.LF, aes(x=LF, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.LF, aes(x = LF, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.LF, aes(x=LF, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.LF, aes(x = LF, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.LF, aes(x=LF, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.LF, aes(x = LF, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("% Live Forbs") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.LF



##### PLOT BARE GROUND #####

pred.BG<-data.frame(BG = seq(0,1,0.01),
                    slope = 0.2,
                    HT = 3,
                    LG = 0.3,
                    LF = .2,
                    DG = .1,
                    DistToShrubs = 0.05,
                    DistToTrees = .25,
                    PropShrubs = 0.1,
                    PropTrees = 0.01,
                    obs = 0,
                    time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.BG <- bind_cols(pred.BG, setNames(as_tibble(predict(RSF_GRSP, pred.BG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.BG <- mutate(GRSP.pres.pred.BG,
                            fit_resp = g_linkinv(fit_link),
                            ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.BG$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.BG <- bind_cols(pred.BG, setNames(as_tibble(predict(RSF_EAME, pred.BG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.BG <- mutate(EAME.pres.pred.BG,
                            fit_resp = e_linkinv(fit_link),
                            ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.BG$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.BG <- bind_cols(pred.BG, setNames(as_tibble(predict(RSF_DICK, pred.BG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.BG <- mutate(DICK.pres.pred.BG,
                            fit_resp = d_linkinv(fit_link),
                            ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.BG$spp <- "Dickcissel"

### plot bare ground ###
AllSpp.BG<-ggplot() + 
  geom_line(data = GRSP.pres.pred.BG, aes(x=BG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.BG, aes(x = BG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.BG, aes(x=BG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.BG, aes(x = BG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.BG, aes(x=BG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.BG, aes(x = BG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("% Bare Ground") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.BG



##### PLOT DEAD GRASS #####

pred.DG<-data.frame(DG = seq(0,1,0.01),
                    slope = 0.2,
                    HT = 3,
                    LG = 0.3,
                    LF = .2,
                    BG = .15,
                    DistToShrubs = 0.05,
                    DistToTrees = .25,
                    PropShrubs = 0.1,
                    PropTrees = 0.01,
                    obs = 0,
                    time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.DG <- bind_cols(pred.DG, setNames(as_tibble(predict(RSF_GRSP, pred.DG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.DG <- mutate(GRSP.pres.pred.DG,
                            fit_resp = g_linkinv(fit_link),
                            ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.DG$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.DG <- bind_cols(pred.DG, setNames(as_tibble(predict(RSF_EAME, pred.DG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.DG <- mutate(EAME.pres.pred.DG,
                            fit_resp = e_linkinv(fit_link),
                            ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.DG$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.DG <- bind_cols(pred.DG, setNames(as_tibble(predict(RSF_DICK, pred.DG, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.DG <- mutate(DICK.pres.pred.DG,
                            fit_resp = d_linkinv(fit_link),
                            ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.DG$spp <- "Dickcissel"

### plot bare ground ###
AllSpp.DG<-ggplot() + 
  geom_line(data = GRSP.pres.pred.DG, aes(x=DG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.DG, aes(x = DG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.DG, aes(x=DG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.DG, aes(x = DG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.DG, aes(x=DG, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.DG, aes(x = DG, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("% Dead Grass") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.DG


ggarrange(AllSpp.LG, AllSpp.LF, AllSpp.BG, AllSpp.DG,
          nrow = 2, ncol = 2, labels = c("A","B","C","D"), common.legend = TRUE, legend = "right")


##### PLOT SLOPE #####

pred.Slope<-data.frame(slope = seq(0,0.65,0.01),
                       HT = 3,
                       LG = 0.3,
                       LF = .2,
                       BG = .15,
                       DG = .1,
                       DistToShrubs = 0.05,
                       DistToTrees = .25,
                       PropShrubs = 0.1,
                       PropTrees = 0.01,
                       obs = 0,
                       time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.Slope <- bind_cols(pred.Slope, setNames(as_tibble(predict(RSF_GRSP, pred.Slope, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.Slope <- mutate(GRSP.pres.pred.Slope,
                            fit_resp = g_linkinv(fit_link),
                            ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.Slope$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.Slope <- bind_cols(pred.Slope, setNames(as_tibble(predict(RSF_EAME, pred.Slope, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.Slope <- mutate(EAME.pres.pred.Slope,
                            fit_resp = e_linkinv(fit_link),
                            ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.Slope$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.Slope <- bind_cols(pred.Slope, setNames(as_tibble(predict(RSF_DICK, pred.Slope, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.Slope <- mutate(DICK.pres.pred.Slope,
                            fit_resp = d_linkinv(fit_link),
                            ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                            ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.Slope$spp <- "Dickcissel"


### plot dead grass ###
AllSpp.Slope<-ggplot() + 
  geom_line(data = GRSP.pres.pred.Slope, aes(x=slope*100, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.Slope, aes(x = slope*100, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.Slope, aes(x=slope*100, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.Slope, aes(x = slope*100, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.Slope, aes(x=slope*100, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.Slope, aes(x = slope*100, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Slope (%)") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.Slope



##### PLOT VEG HT #####

pred.HT<-data.frame(slope = 0.2,
                    HT = seq(0,10,0.1),
                    LG = 0.3,
                    LF = .2,
                    BG = .15,
                    DG = .1,
                    DistToShrubs = 0.05,
                    DistToTrees = .25,
                    PropShrubs = 0.1,
                    PropTrees = 0.01,
                    obs = 0,
                    time = 2253)

### predict based on model and calculate 95% CI

#GRSP
GRSP.pres.pred.HT <- bind_cols(pred.HT, setNames(as_tibble(predict(RSF_GRSP, pred.HT, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
GRSP.pres.pred.HT <- mutate(GRSP.pres.pred.HT,
                               fit_resp = g_linkinv(fit_link),
                               ci_upr = g_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = g_linkinv(fit_link - (1.96 * se_link)))

GRSP.pres.pred.HT$spp <- "Grasshopper Sparrow"

#EAME
EAME.pres.pred.HT <- bind_cols(pred.HT, setNames(as_tibble(predict(RSF_EAME, pred.HT, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
EAME.pres.pred.HT <- mutate(EAME.pres.pred.HT,
                               fit_resp = e_linkinv(fit_link),
                               ci_upr = e_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = e_linkinv(fit_link - (1.96 * se_link)))

EAME.pres.pred.HT$spp <- "Eastern Meadowlark"

#DICK
DICK.pres.pred.HT <- bind_cols(pred.HT, setNames(as_tibble(predict(RSF_DICK, pred.HT, se.fit = TRUE)[1:2]),c("fit_link","se_link")))
DICK.pres.pred.HT <- mutate(DICK.pres.pred.HT,
                               fit_resp = d_linkinv(fit_link),
                               ci_upr = d_linkinv(fit_link + (1.96* se_link)),
                               ci_lwr = d_linkinv(fit_link - (1.96 * se_link)))

DICK.pres.pred.HT$spp <- "Dickcissel"


### plot vegetation height ###
AllSpp.HT<-ggplot() + 
  geom_line(data = GRSP.pres.pred.HT, aes(x=HT, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = GRSP.pres.pred.HT, aes(x = HT, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = EAME.pres.pred.HT, aes(x=HT, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = EAME.pres.pred.HT, aes(x = HT, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  geom_line(data = DICK.pres.pred.HT, aes(x=HT, y=fit_resp, col = spp), size =1.25) +
  geom_ribbon(data = DICK.pres.pred.HT, aes(x = HT, ymin=ci_lwr, ymax=ci_upr, fill = spp), alpha = 0.2) +
  scale_color_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  scale_fill_manual(values = c("Grasshopper Sparrow" = "goldenrod","Eastern Meadowlark" = "skyblue3","Dickcissel" = "darkolivegreen4"), name = "") +
  theme_pubr() +
  xlab("Vegetation Height (dm)") +
  ylab("Probability of Territory Selection") +
  ylim(0,1) +
  theme(legend.position = "right")

AllSpp.HT
