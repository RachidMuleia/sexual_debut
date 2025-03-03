############################################################################
## PROJECT : FACTORS ASSOCIATED TO EARLY SEXUAL DEBUT                     ##
## OWNER  : RACACHID MULEIA                                               ##
## AUTHOR: RACHID MULEIA 
## DATE:   01 APRIL 2024                                                  ##
############################################################################
setwd("/Users/rachidmuleia/Dropbox/INS/ARTIGO_SEX_DEBUT/DATA/R_CODE_AND_INSIDA_DATA/INSIDA 2021 Household Interview and Biomarker Datasets (CSV)")
filepath=getwd()

source(paste(filepath,"packages.R",sep="/")) # loading all the necessary packages (pagackages not installed will be installed automtically )
source(paste(filepath,"Rsource_code.R", sep="/"))
list.files()
insida_df  <- read.csv( "insida2021adultind.csv", header = TRUE)
insida_hh_df  <- read.csv("insida2021hh.csv", header = TRUE)
#View(insida_df)
#View(insida_hh_df)

insida_df1 <- merge(insida_df, insida_hh_df, by = 'householdid')

insida_df1$province.y <- NULL

insida_df1 <- insida_df1 |> rename(province = province.x, 
                                   urban = urban.x, wealthquintile = wealthscorecont.x) |>
  mutate(
    REGION = case_when(
    province %in% c(1,2,3) ~ "1_NORTH",
    province %in% c(4,5,6,7) ~ "2_CENTER",
    province %in% c(8,9,10,11) ~ '3_SOUTH',
    .default = NA
  ),
  SEX = case_when(
    gender == 1 ~ '1_MALE',
    gender == 2 ~ '2_FEMALE',
    .default = NA
    ),
  RESIDENCE = case_when(
    urban == 1 ~ '1_URBAN',
    urban == 2 ~ '2_RURAL',
    .default = NA
  ), 
 EDUCATION = case_when(
   education == 1 ~ '1_NO_EDUCATION',
   education == 2 ~ '2_PRIMARY',
   education == 3 ~  '3_SECONDARY',
   education == 4 ~ '4_HIGHER',
   .default = NA
 
 ),
 WEALTH_INDEX = case_when(
   wealthquintile.x %in% c(1,2) ~ '1_LOW',
   wealthquintile.x == 3 ~ '2_MIDDLE',
   wealthquintile.x %in% c(4,5) ~ '3_HIGH',
   .default = NA
 ),
 MARITAL_STATUS = case_when(
   married == 1 ~ '1_SINGLE',
   married == 2  ~ '2_MARRIED',
   married %in% c(3,4) ~ '3_DIVORCED_WIDOWED',
   .default = NA
 ),
 
 OCCUPATION = case_when(
   work12mo == 2 ~ '1_UNEMPLOYED',
   !(workind_mz %in% c(-9,-8)) ~ '2_EMPLOYED',
   .default = NA
 ),
 AGEC = case_when(
   age < 20 ~ '1_15-19',
   age > 19 & age < 25 ~ '2_20-24',
   .default = NA
 ),
 
 AGE_MARRIAGE = case_when(
   married == 1 ~ '1_NEVER_MARRIED',
   agemar < 20 ~ '2_<15',
   agemar >= 20 ~'3_>=15',
  .default = NA
 ),
 
 HOUSEHOLD_HEAD = case_when(
   householdheadgender == 1 ~ '1_MALE',
   householdheadgender == 2 ~ '2_FEMALE',
   .default = NA
 ),
 
ALCO_USE = case_when(
  alcfreq == 0 ~ '2_NO',
  alcfreq %in% 1:4 ~ '1_YES', 
  .default = NA
),

ALC_PT1 = replace(alcfreq, alcfreq %in% c(-8,-9),NA),
ALC_PT2 = replace(alcnumday,alcnumday %in% c(-8,-9), NA),
ALC_PT3 = replace(alcsixmore, alcsixmore %in% c(-8,-9),NA),
)


insida_df1 <- insida_df1 |>
    mutate(
PT_SCORE = rowSums(insida_df1[,c('ALC_PT1', 'ALC_PT2', 'ALC_PT3')],na.rm = TRUE),

BINGE_DRK = case_when( # CONSUMO ABUSIVO DE ALCOOL- AUDITC C
  SEX == '1_MALE' & PT_SCORE > 3 ~ '1_ABUSIVE',
  SEX == '1_MALE' & PT_SCORE <= 3 ~ '2_NON_ABUSIVE',
  SEX == '2_FEMALE' & PT_SCORE > 2 ~ '1_ABUSIVE',
  SEX == '2_FEMALE' & PT_SCORE <= 2 ~ '2_NON_ABUSIVE',
  .default = NA
),
STATUS_SEX = case_when(
  sexever == 1 ~ 1,
  sexever == 2 ~ 0,
  .default = NA
),

LIVE_OUT = case_when( # ever stayed out of your home for one or more month 
  monthoutever == 1 ~ '1_YES',
  monthoutever == 2 ~ '2_NO',
  .default = NA
),
DRUG_CAT = case_when( # Ever taken drugs for recreation in the last 12 months
  drug_mz == 1 ~ '2_YES',
  drug_mz == 2 ~ '1_NO',
  .default = NA
), 
TALK_SEX  = case_when( # talk with parent about sex
  adtpsx == 1 ~ '1_YES',
  adtpsx == 2 ~ '2_NO',
  .default = NA
),

TALK_HIV = case_when( # takl with parent about HIV
  addishiv_mz == 1 ~ '1_YES',
  addishiv_mz == 2 ~ '2_NO',
  .default = NA
),

PREV_PROGRAM = case_when(
  adhivprev_a == 1 | adhivprev_c == 1 | adhivprev_d == 1 |
    adhivprev_e == 1 | adhivprev_f == 1 | adhivprev_g == 1 |
    adhivprev_w == 1 | adhivprev_x == 1 ~ '1_YES', 
  
  adhivprev_a == 2 & adhivprev_c == 2 & adhivprev_d == 2 &
    adhivprev_e == 2 & adhivprev_f == 2 & adhivprev_g == 2 &
    adhivprev_w == 2 & adhivprev_x == 2 ~ '2_NO', 
  .default = NA
  
),
DRINK_ALCOHOL = case_when(
  alcfreq == 0 ~ '1_NO',
  alcfreq %in% c(1,2,3,4)~ '2_YES',
  .default = NA
)

)

insida_df1$AGE_SEX_DEBUT <- rep(NA, dim(insida_df1)[1])
insida_df1$AGE_SEX_DEBUT <- insida_df1$firstsxage
insida_df1$AGE_SEX_DEBUT[which(insida_df1$AGE_SEX_DEBUT == -96)] <- insida_df1$age[which(insida_df1$AGE_SEX_DEBUT == -96)]
insida_df1$AGE_SEX_DEBUT[which(insida_df1$AGE_SEX_DEBUT %in% c(-8,-9, -7))] <- NA
#insida_df1$AGE_SEX_DEBUT[which(is.na(insida_df1$AGE_SEX_DEBUT) & insida_df1$STATUS_SEX == 1)] <- insida_df1$age[which(is.na(insida_df1$AGE_SEX_DEBUT) & insida_df1$STATUS_SEX == 1)]
#insida_df1$AGE_SEX_DEBUT[which(is.na(insida_df1$AGE_SEX_DEBUT) & insida_df1$STATUS_SEX == 0)] <- insida_df1$age[which(is.na(insida_df1$AGE_SEX_DEBUT) & insida_df1$STATUS_SEX == 0)]



# CONSIDER INDIVIDUAL AGED 15-24

insida_15_24 <- insida_df1[insida_df1$AGEC %in% c("1_15-19", "2_20-24"),]





#################################### RUN MODEL- CHANGE SURVIVAL TO POISSON ################################

path_sp <- getwd()

coords_df <- read.csv(paste(path_sp, 'insida2021centroids.csv', sep = '/'), header = TRUE)


insida_sp_df <- merge(insida_15_24, coords_df, by.x = 'centroidid.x', by.y = 'CentroidID')



#AGE_MARRIAGE
#MARITAL_STATUS
model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ latitude+longitude+
                          REGION+ RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT)

model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ SEX+
                          RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT)

variable <- c("AGE_SEX_DEBUT", "STATUS_SEX", "latitude", "longitude", "SEX",
             "RESIDENCE", "EDUCATION", "WEALTH_INDEX", "MARITAL_STATUS",
              "OCCUPATION", "AGEC", "HOUSEHOLD_HEAD", "DRINK_ALCOHOL", "TALK_SEX","TALK_HIV", "DRUG_CAT","PREV_PROGRAM" ,"intwt0", 'LIVE_OUT')

agefit <- coxph(model.cox,data = insida_sp_df[, variable], weights = insida_sp_df$intwt0)
#stepAIC(agefit)
summary(agefit)
exp.fit<-predict(agefit, type = "expected")
xbeta<-predict(agefit, type = "lp")
newtime<-exp(-xbeta)*exp.fit

agefit$




insida_sp_df1 <- na.omit(insida_sp_df[, c(variable)])
insida_sp_df1$newtime  <- newtime

# ESPECIFY DESIGN MATRIX 

X <- model.matrix(~latitude+longitude+SEX+
                    RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                    OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT, data=insida_sp_df1)


# ESPECIFIY THE RADIAL BASIS SPLINE


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



# FUNCTION TO RUN GENERALIZED GEOADDITIVE MODEL FOR SURVIVAL OUTCOME 


loglik.function.ML <- function(phi.opt, data, dist.data, dist.knots, model, fit){
  get_rand=getSmo(fit)
  sigmasq.epsilon=get_rand$sigma^2
  sigmasq.u = sigmasq.epsilon*exp(2*unlist(get_rand$modelStruct))
  eta=predict(fit)
  mu=exp(eta)
  D=diag(as.numeric(1/mu)) ## Partial derivative for eta ###
  V=diag(as.numeric(mu)) ### V(m)=m
  w=as.numeric(mu)*as.numeric(1/(mu))^2
  W=diag(1/w)
  Z.opt=create.Z.matrix(data, dist.data, dist.knots, model, phi.opt)
  V=chol2inv(Rfast::cholesky(W,parallel = TRUE))+sigmasq.u* Rfast::Tcrossprod(Z.opt,Z.opt)
  inverseV=chol2inv(Rfast::cholesky(V, parallel = TRUE))
  Y_pseudo=eta+D%*%(data$STATUS_SEX-mu)
  #res=fit$residuals[,1]
  res=Y_pseudo-X%*%coef(fit)[!is.na(coef(fit))]
  ll=-0.5*(determinant(V, logarithm = TRUE)$modulus[1]+t(res)%*%inverseV%*%res)
  return(ll)
}

Ida=rep(1, nrow(insida_sp_df1))

run.doubly.iterative.method.ML <- function(data, phi.null, model, tol1, tol2, max.iter, phi.upper.limit){
  phi.old = phi.null
  stop = 0
  iteration = 0
  phi.opt.vec = vector()
  phi.opt.vec[1] = phi.old
  k = 1
  #y=resp
  while (stop!=1 & iteration < max.iter){
    Z<<-create.Z.matrix(data, dist.data, dist.knots, model, phi.old)
    #fit = glmmPQL(y~-1+X, random=list(Ida=pdIdent(~Z-1)), family = binomial,weights = na.omit(insida_sp_df1)[,"weight_correct"]*10000)
    fit=gamlss(data$STATUS_SEX~-1+X+offset(log(newtime))+re(random=list(Ida=pdIdent(~Z-1)),method="ML"),data=insida_sp_df1 ,family=PO(), n.cyc = 40)
    maximize.phi = optimize(loglik.function.ML, c(0.8,phi.upper.limit), data=data, dist.data=dist.data, dist.knots=dist.knots, model=model, fit = fit, tol=tol1, maximum=TRUE)
    
    phi.new = maximize.phi$maximum
    phi.opt.vec[k+1] = phi.new
    if (abs(phi.old - phi.new) < tol2) stop=1
    print(c(iteration, phi.old))
    phi.old = phi.new
    k = k+1
    iteration = iteration + 1
    cat('iteration',iteration,'\n')
  }
  fit$phi = tail(phi.opt.vec,1)
  return(fit)
}

spherical_model <- run.doubly.iterative.method.ML(data=insida_sp_df1, phi.null = 1 ,phi.upper.limit = 15,model='spherical',tol1=0.0001, tol2 = 0.0001, max.iter = 200)
save(spherical_model, file = "FULL_spherical_model1.RData")



###------------------------------MODEL FOR FEMALE INDIVIDUALS ---------------------------------------------




#AGE_MARRIAGE
#MARITAL_STATUS
model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ latitude+longitude+
                         RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT)


variable <- c("AGE_SEX_DEBUT", "STATUS_SEX", "latitude", "longitude", "SEX",
              "REGION", "RESIDENCE", "EDUCATION", "WEALTH_INDEX", "MARITAL_STATUS",
              "OCCUPATION", "AGEC", "HOUSEHOLD_HEAD", "DRINK_ALCOHOL", "TALK_SEX","TALK_HIV", "DRUG_CAT","PREV_PROGRAM" ,"intwt0", 'LIVE_OUT')

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
                   RESIDENCE+EDUCATION+ WEALTH_INDEX+ MARITAL_STATUS+
                    OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+ TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT, data=insida_sp_df1)


# ESPECIFIY THE RADIAL BASIS SPLINE


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


# geoadditive models
Ida=rep(1, nrow(insida_sp_df1))

spherical_model_f <- run.doubly.iterative.method.ML(data=insida_sp_df1, phi.null = 1 ,phi.upper.limit = 15,model='spherical',tol1=0.0001, tol2 = 0.0001, max.iter = 200)
save(spherical_model_f, file = "FEMALE_spherical_model1.RData")






#-------------------------------- RUN MODEL FOR MALE INDIVIDUALS----------------------------------------------------------



#AGE_MARRIAGE
#MARITAL_STATUS
model.cox <- as.formula(Surv(AGE_SEX_DEBUT, STATUS_SEX) ~ latitude+longitude+
                          RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                          OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT)




variable <- c("AGE_SEX_DEBUT", "STATUS_SEX", "latitude", "longitude", "SEX",
              "REGION", "RESIDENCE", "EDUCATION", "WEALTH_INDEX", "MARITAL_STATUS",
              "OCCUPATION", "AGEC", "HOUSEHOLD_HEAD", "DRINK_ALCOHOL", "TALK_SEX","TALK_HIV", "DRUG_CAT","PREV_PROGRAM" ,"intwt0", "LIVE_OUT")

agefit <- coxph(model.cox, data = insida_sp_df[insida_sp_df$SEX == '1_MALE', variable], weights = insida_sp_df[insida_sp_df$SEX == '1_MALE',"intwt0"])
#stepAIC(agefit)
#summary(agefit)
exp.fit<-predict(agefit, type = "expected")
xbeta<-predict(agefit, type = "lp")
newtime<-exp(-xbeta)*exp.fit


insida_sp_df1 <- na.omit(insida_sp_df[insida_sp_df$SEX == '1_MALE', c(variable)])
insida_sp_df1$newtime  <- newtime

# ESPECIFY DESIGN MATRIX 

X <- model.matrix(~latitude+longitude+
                    RESIDENCE+EDUCATION+MARITAL_STATUS+ WEALTH_INDEX+
                    OCCUPATION+AGEC+HOUSEHOLD_HEAD+ DRINK_ALCOHOL+TALK_SEX+TALK_HIV+DRUG_CAT+PREV_PROGRAM+LIVE_OUT, data=insida_sp_df1)






# ESPECIFIY THE RADIAL BASIS SPLINE


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


# geoadditive models

Ida=rep(1, nrow(insida_sp_df1))

spherical_model_m <- run.doubly.iterative.method.ML(data=insida_sp_df1, phi.null = 1 ,phi.upper.limit = 15,model='spherical',tol1=0.0001, tol2 = 0.0001, max.iter = 200)



#------------------------------------------- Extract the OR -------------------------------------------------
library(rio)


extract_OR<-function(x,dec_Or=1,dec=3){
  coeff<-na.omit(coef(x))
  lab<-labels(na.omit(coef(x)))
  li<-coeff-1.95*vcov(x, type="se")
  ls<-coeff+1.95*vcov(x, type="se")
  conf<-paste( paste( '(',round(exp(li),dec),sep=''),paste(round(exp(ls),dec),')',sep=''),sep='-')
  OR_conf<-paste(round(exp(coeff),dec_Or),conf, sep=' ' )
  data_OR<-data.frame(variabl_cat=lab, parameter=OR_conf, p_value=invisible(summary(x))[,4])
  return(data_OR)
  
}

export(extract_OR(spherical_model,dec_Or = 1, dec = 2), file ='FULL_spherical.xlsx')

export(extract_OR(spherical_model_m,dec_Or = 1, dec = 2), file ='MALE_spherical.xlsx')

export(extract_OR(spherical_model_f,dec_Or = 1, dec = 2), file ='FEMALE_spherical.xlsx')
