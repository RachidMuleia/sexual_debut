


library(plotly)
library(sf)
library(stars)
library(ggspatial)
library(gridExtra)
library(ggpubr)
library(cowplot)

setwd('/Users/rachidmuleia/Dropbox/INS/ARTIGO_SEX_DEBUT/DATA')

path_map <- '/Users/rachidmuleia/Dropbox/INS/ARTIGO_SEX_DEBUT/DATA'
map <- st_read(paste(path_map, "MOZ-level_1.shp", sep = '/'))

# make a fishnet grid over the countries
grd <- st_make_grid(map, n = 200)
# visualize the grid
plot(grd)

# find which grid points intersect `polygons` (countries) 
# and create an index to subset from
index <- which(lengths(st_intersects(grd, map)) > 0)

# subset the grid to make a fishnet
fishnet <- grd[index]


plot(fishnet)



insida_sp_df1 <- na.omit(insida_sp_df[, c(variable)])

# first difine knots using space filling algorithm by Nytcha for the coordinates
set.seed(20000)
# The number of knots is defined to be K=max{20, min(n/4,150)}
space.filling = cover.design(R=unique(insida_sp_df1[,which(colnames(insida_sp_df1)%in%c("longitude", "latitude"))]), nd=150, nn=FALSE, nruns=1)
knots=space.filling$design
index.knots = space.filling$best.id
#index.knots
#tau=seq(0, 17,0.2)
x.dist1 = abs(outer(insida_sp_df1[,'longitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1],"-"))
y.dist1 = abs(outer(insida_sp_df1[,'latitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2],"-"))
dist.data = sqrt(x.dist1^2 + y.dist1^2)
x.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1],"-"))
y.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2],"-"))
dist.knots = sqrt(x.dist2^2 + y.dist2^2)

pred.loc = st_coordinates(fishnet)
x.d = abs(outer(pred.loc[,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1],"-"))
y.d = abs(outer(pred.loc[,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2],"-"))
dist.pred = sqrt(x.d^2 + y.d^2)

# SPHERICAL
phi=spherical_model$phi
Z.pred=create.Z.matrix(data=data_final, dist.data = dist.pred, dist.knots = dist.knots, model='spherical', phi=phi)
coeff=getSmo(spherical_model)
coeff=coeff$coefficients$random$Ida
spline.sph=Z.pred%*%t(coeff)


pred.loc<- st_coordinates(fishnet)
pred.loc1 <- as.data.frame(pred.loc[, c('X', 'Y')])
pred.loc1$resp <- as.data.frame(spline.sph[,1])[,1]

map <-st_set_crs(map, 4326)

overall_plot_map <- ggplot(pred.loc1)+geom_tile(aes(X,Y,fill = resp))+
  geom_sf(data =st_cast(map, "MULTILINESTRING"))+
  coord_sf(lims_method = "geometry_bbox")+
  scale_fill_gradientn("resp",colours = terrain.colors(10), limits = range(pred.loc1$resp), name = NULL)+
annotation_scale(location = "br", width_hint = 0.5, plot_unit = 'km')+
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = "Overall geoadditive model")+
  theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  #scale_fill_gradient(low = "blue", high = "red")






#-------------------------------- PLOT FEMALE MODEL ------------------------------------


#AGE_MARRIAGE
#MARITAL_STATUS
model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ latitude+longitude+
                          REGION+ RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM)


variable <- c("AGE_SEX_DEBUT", "STATUS_SEX", "latitude", "longitude", "SEX",
              "REGION", "RESIDENCE", "EDUCATION", "WEALTH_INDEX", "MARITAL_STATUS",
              "OCCUPATION", "AGEC", "HOUSEHOLD_HEAD", "DRINK_ALCOHOL", "TALK_SEX","TALK_HIV", "DRUG_CAT","PREV_PROGRAM" ,"intwt0")

agefit <- coxph(model.cox,data = insida_sp_df[insida_sp_df$SEX == '2_FEMALE', variable])
#stepAIC(agefit)
#summary(agefit)
exp.fit<-predict(agefit, type = "expected")
xbeta<-predict(agefit, type = "lp")
newtime<-exp(-xbeta)*exp.fit


insida_sp_df1 <- na.omit(insida_sp_df[insida_sp_df$SEX == '2_FEMALE', c(variable)])
insida_sp_df1$newtime  <- newtime

# ESPECIFY DESIGN MATRIX 

X <- model.matrix(~latitude+longitude+
                    REGION+ RESIDENCE+EDUCATION+ WEALTH_INDEX+ MARITAL_STATUS+
                    OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+ TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM + offset(log(newtime)), data=insida_sp_df1)












# first difine knots using space filling algorithm by Nytcha for the coordinates
set.seed(20000)
# The number of knots is defined to be K=max{20, min(n/4,150)}
space.filling = cover.design(R=unique(insida_sp_df1[,which(colnames(insida_sp_df1)%in%c("longitude", "latitude"))]), nd=150, nn=FALSE, nruns=1)
knots=space.filling$design
index.knots = space.filling$best.id
#index.knots
#tau=seq(0, 17,0.2)
x.dist1 = abs(outer(insida_sp_df1[,'longitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1],"-"))
y.dist1 = abs(outer(insida_sp_df1[,'latitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,2],"-"))
dist.data = sqrt(x.dist1^2 + y.dist1^2)
x.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1],"-"))
y.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,2],"-"))
dist.knots = sqrt(x.dist2^2 + y.dist2^2)


# ESPECIFIY THE RADIAL BASIS SPLINE

pred.loc = st_coordinates(fishnet)
x.d = abs(outer(pred.loc[,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1],"-"))
y.d = abs(outer(pred.loc[,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,2],"-"))
dist.pred = sqrt(x.d^2 + y.d^2)





# corrected AIC

AIC_corrected = function(x){
  eff_df =x$df.fit
  Deviance =x$G.deviance
  n_samp =dim(x$mu.x)[1]
  aic_c = Deviance+(2*(eff_df+1))/(n_samp-eff_df-2)
  return(aic_c)
}



# SPHERICAL
phi=spherical_model_f$phi
Z.pred=create.Z.matrix(data=insida_sp_df1, dist.data = dist.pred, dist.knots = dist.knots, model='spherical', phi = phi)
coeff=getSmo(spherical_model_f)
coeff=coeff$coefficients$random$Ida
spline.model=Z.pred%*%t(coeff)


pred.loc<- st_coordinates(fishnet)
pred.loc1 <- as.data.frame(pred.loc[, c('X', 'Y')])
pred.loc1$resp <- as.data.frame(spline.model[,1])[,1]

limites <- range(pred.loc1$resp)


female_plot_map <-ggplot(pred.loc1)+geom_tile(aes(X,Y,fill = resp))+
  geom_sf(data =st_cast(map, "MULTILINESTRING"))+
  coord_sf(lims_method = "geometry_bbox")+
  scale_fill_gradientn("resp",colours = terrain.colors(10))+
  annotation_scale(location = "br", width_hint = 0.5, plot_unit = 'km')+
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = "Female geoadditive model")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())
#scale_fill_gradient(low = "blue", high = "red")






#------------------------ ----- PLOT MAP FOR MALE INDIVIDUALS ------------------------------------



model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ latitude+longitude+
                          REGION+ RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM)


variable <- c("AGE_SEX_DEBUT", "STATUS_SEX", "latitude", "longitude", "SEX",
              "REGION", "RESIDENCE", "EDUCATION", "WEALTH_INDEX", "MARITAL_STATUS",
              "OCCUPATION", "AGEC", "HOUSEHOLD_HEAD", "DRINK_ALCOHOL", "TALK_SEX","TALK_HIV", "DRUG_CAT","PREV_PROGRAM" ,"intwt0")

agefit <- coxph(model.cox,data = insida_sp_df[insida_sp_df$SEX == '1_MALE', variable])
#stepAIC(agefit)
#summary(agefit)
exp.fit<-predict(agefit, type = "expected")
xbeta<-predict(agefit, type = "lp")
newtime<-exp(-xbeta)*exp.fit


insida_sp_df1 <- na.omit(insida_sp_df[insida_sp_df$SEX == '1_MALE', c(variable)])
insida_sp_df1$newtime  <- newtime

# ESPECIFY DESIGN MATRIX 

X <- model.matrix(~latitude+longitude+
                    REGION+ RESIDENCE+EDUCATION+ WEALTH_INDEX+ MARITAL_STATUS+
                    OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+ TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM + offset(log(newtime)), data=insida_sp_df1)










# first difine knots using space filling algorithm by Nytcha for the coordinates
set.seed(20000)
# The number of knots is defined to be K=max{20, min(n/4,150)}
space.filling = cover.design(R=unique(insida_sp_df1[,which(colnames(insida_sp_df1)%in%c("longitude", "latitude"))]), nd=150, nn=FALSE, nruns=1)
knots=space.filling$design
index.knots = space.filling$best.id
#index.knots
#tau=seq(0, 17,0.2)
x.dist1 = abs(outer(insida_sp_df1[,'longitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1],"-"))
y.dist1 = abs(outer(insida_sp_df1[,'latitude'], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2],"-"))
dist.data = sqrt(x.dist1^2 + y.dist1^2)
x.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,1],"-"))
y.dist2 = abs(outer(unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots ,2],"-"))
dist.knots = sqrt(x.dist2^2 + y.dist2^2)


# ESPECIFIY THE RADIAL BASIS SPLINE

pred.loc = st_coordinates(fishnet)
x.d = abs(outer(pred.loc[,1], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,1],"-"))
y.d = abs(outer(pred.loc[,2], unique(insida_sp_df1[,c("longitude",'latitude')])[index.knots,2],"-"))
dist.pred = sqrt(x.d^2 + y.d^2)



load(paste(path_load, "MALE_exponential_model1.RData", sep = '/'))
load(paste(path_load, "MALE_spherical_model1.RData", sep = '/'))

# SPHERICAL
phi=spherical_model_m$phi
Z.pred=create.Z.matrix(data=insida_sp_df1, dist.data = dist.pred, dist.knots = dist.knots, model='spherical', phi = phi)
coeff=getSmo(spherical_model_m)
coeff=coeff$coefficients$random$Ida
spline.model=Z.pred%*%t(coeff)


pred.loc<- st_coordinates(fishnet)
pred.loc1 <- as.data.frame(pred.loc[, c('X', 'Y')])
pred.loc1$resp <- as.data.frame(spline.model[,1])[,1]




male_plot_map <- ggplot(pred.loc1)+geom_tile(aes(X,Y,fill = resp))+
  geom_sf(data =st_cast(map, "MULTILINESTRING"))+
  coord_sf(lims_method = "geometry_bbox")+
  scale_fill_gradientn("resp",colours = terrain.colors(10))+
  annotation_scale(location = "br", width_hint = 0.5, plot_unit = 'km')+
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = "Male geoadditive model")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank(),
        )


combined_plot <- plot_grid(overall_plot_map, female_plot_map, male_plot_map, ncol = 3)

path_save <- '/Users/rachidmuleia/Dropbox/INS/ARTIGO_SEX_DEBUT/DATA/Plots_Kaplan_Meier'
ggsave(paste(path_save,"combine_spatial_new.png", sep='/'), combined_plot, 
       width = 15, height = 5, units = "in", dpi = 300)







