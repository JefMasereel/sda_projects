
###############################################
## Project 2 SDA - by Jef Masereel, r0631651 ##
## Focus on response variable 'Casual'       ##
###############################################

## NOTE ON CODE REFERENCES
# some code was written by reference to the provided exercise solutions from earlier sessions.
# besides this, some online sources such as Quick-R and Stackoverflow were consulted at times.

dev.off()
rm(list = ls())

library(car)
library(dplyr)
library(aod)
library(VGAM)
library(testit)
library(robustbase)
library(MASS)
library(leaps)

# Jef Masereel - Group 19
TST = read.csv("test.csv")
TRN = read.csv("train.csv")

# directory to write images to
img_dir <- '~/1_KUL/BME - 2e jaar/SEM2/SDA/Taak 2/images'
# filename=paste(dir,'/test.pdf',sep='')



# -------------------------------------------------------------------------
# 1 - Data inspection
# -------------------------------------------------------------------------

# keep an original copy (f.e. cor() can't handle factors)
ORG <- TRN

summary(TRN)


## 1.1 Structuring the variables

# categorical variables
cols_catg <- c("season","holiday","workingday","weathersituation")
# proportion variables
cols_prop <- c("temperature","feelingtemperature","humidity","windspeed")
# response variables
cols_resp <- c("casual","registered")

# set factors
for (i in cols_catg){
  TRN[[i]] <- as.factor(TRN[[i]])
}


## 1.2 Inspect univariate distributions

# categorical vars
for (i in cols_catg){
  hist(ORG[[i]],xlab=i,main='')
}
# proportion vars
for (i in cols_prop){
  boxplot(TRN[[i]],xlab=i,horizontal = TRUE)
  plot(density(TRN[[i]]),xlab=i,main='')
  qqnorm(TRN[[i]],main=i)
  qqline(TRN[[i]])
}
# response vars
for (i in cols_resp){
  boxplot(TRN[[i]],xlab=i,horizontal = TRUE)
  plot(density(TRN[[i]]),xlab=i,main='')
  qqnorm(TRN[[i]],main=i)
  qqline(TRN[[i]])
}


## 1.3 Inspect relationships between variables

# visual inspection
pairs(TRN)

# correlation
print(cor(ORG))

# check multicollinearity
crm <- cor(ORG[, -which(names(TRN)==cols_resp)])
diag(solve(crm))       # VIF values
max(diag(solve(crm)))  # 160.3748 > 10: strong multicollinearity    
mean(diag(solve(crm))) # 40.86969 >> 1: strong multicollinearity    

# removing the temperature variable shows:
crm2 <- cor(ORG[,-c(6,9,10)])
diag(solve(crm2))       # VIF values
max(diag(solve(crm2)))  # 1.838895 !> 10: low multicollinearity   
mean(diag(solve(crm2))) # 1.36116   > 1 : mild multicollinearity (see 1.4)

# similar conclusions from eigenvalues
# with temperature included
ev <- eigen(crm)
ev$values / sum(ev$values)              # some small values: indication of multicollinearity
min(ev$values / sum(ev$values))         # min = 0.0003930309
sqrt( max(ev$values) / min(ev$values) ) # 27.61943 < 30: near indication of strong multicollinearity
# without temperature included
ev2 <- eigen(crm2)
ev2$values / sum(ev2$values)              # less multicollinearity
min(ev2$values / sum(ev2$values))         # min = 0.04444702
sqrt( max(ev2$values) / min(ev2$values) ) # 2.40849 < 30: no indication of strong multicollinearity


## 1.4 Multicollinearity remains a problem in practice

# remove feelingtemperature to get rid of large multicollinearity
TRN2 <- dplyr::select(TRN, -feelingtemperature)
TRN_cas2 <- dplyr::select(TRN2, -registered)
TRN_reg2 <- dplyr::select(TRN2, -casual)

# try robust regression on all variables but feelingtemperature: computational issues
ltsReg(casual ~ ., data = TRN_cas2, alpha=0.5)
ltsReg(casual ~ ., data = TRN_cas2, alpha=0.99)
ltsReg(registered ~ ., data = TRN_reg2, alpha=0.5)
ltsReg(registered ~ ., data = TRN_reg2, alpha=0.99)

# continue with feelingtemperature included for complete variable selection
TRN_cas <- dplyr::select(TRN, -registered)
TRN_reg <- dplyr::select(TRN, -casual)


## 1.5 Reference models: direct linear regression

# Casual: R2=0.7166,	R2_adj=0.7068 
TRN_refcas <- lm(casual ~ ., data=TRN_cas)
summary(TRN_refcas)

# Registered: R2=0.8142, R2_adj=0.8077
TRN_refreg <- lm(registered ~ ., data=TRN_reg)
summary(TRN_refreg)



# -------------------------------------------------------------------------
# 2 - Transformations
# -------------------------------------------------------------------------

## 2.1 Exploring impact of transformations on proportion variables

## Proportion transformations
# aa <- logitlink(TRN$humidity)   # logit(P)
# bb <- probitlink(TRN$humidity)  # probit(P)
# cc <- asin(sqrt(TRN$humidity))  # asin(sqrt(P))

# TRN$humidity has zero-values, add a small translation
(1-max(TRN$humidity))/2
TRN$humidity <- TRN$humidity + 0.01

# original
or <- c(1:4)
for (i in c(1:4)){
  or[i] <- shapiro.test(TRN[[cols_prop[[i]]]])$p.value
}

# logit(P)
aa <- c(1:4)
TRNaa <- TRN
# TRNaa$humidity <- TRNaa$humidity + 0.025
for (i in c(1:4)){
  TRNaa[[cols_prop[[i]]]] <- logitlink(TRNaa[[cols_prop[[i]]]])
  aa[i] <- shapiro.test(TRNaa[[cols_prop[[i]]]])$p.value
}

# probit(P)
bb <- c(1:4)
TRNbb <- TRN
# TRNbb$humidity <- TRNaa$humidity + 0.025
for (i in c(1:4)){
  TRNbb[[cols_prop[[i]]]] <- probitlink(TRNbb[[cols_prop[[i]]]])
  bb[i] <- shapiro.test(TRNbb[[cols_prop[[i]]]])$p.value
}

# asin(sqrt(P))
cc <- c(1:4)
TRNcc <- TRN
for (i in c(1:4)){
  TRNcc[[cols_prop[[i]]]] <- asin(sqrt(TRNcc[[cols_prop[[i]]]]))
  cc[i] <- shapiro.test(TRNcc[[cols_prop[[i]]]])$p.value
}

# in summary:
pvals = data.frame(
  TRANSFORMATION = cols_prop,
  original = or,
  logit = aa,
  probit = bb,
  asin_sqrt = cc)
pvals


## 2.2 Applying chosen transformations to appropriate predictor variables

# apply transformations
TRN$temperature <- logitlink(ORG$temperature)
TRN$feelingtemperature <- logitlink(ORG$feelingtemperature)
TRN$windspeed <- asin(sqrt(ORG$windspeed))
# TRN$humidity = best to leave as is --> undo translation (line 156)
TRN$humidity <- TRN$humidity - 0.01

# inspecting results
for (i in cols_prop){
  boxplot(TRN[[i]],xlab=i,horizontal = TRUE)
  plot(density(TRN[[i]]),xlab=i,main='')
  qqnorm(TRN[[i]],main=i)
  qqline(TRN[[i]])
}

# Update datasets for separate regression models
TRN_cas <- dplyr::select(TRN, -registered)
TRN_reg <- dplyr::select(TRN, -casual)



# -------------------------------------------------------------------------
# 3 - Basic linear regression & transformation of response variables
# -------------------------------------------------------------------------

## 3.1a Linear regression, casual ~ all (after transforms of 2.2)

# get regression model
lm_cas <- lm(casual ~ . , data=TRN_cas)
summary(lm_cas)
plot(lm_cas)

# residual plots ifo. single variables
res_cas <- lm_cas$residuals
plot(TRN$temperature,res_cas,xlab='temperature',ylab='residuals')
plot(TRN$windspeed,res_cas,xlab='windspeed',ylab='residuals')
plot(TRN$humidity,res_cas,xlab='humidity',ylab='residuals')

plot(lm_cas$fitted.values, res_cas)

# testing normality of residuals (Gauss-Markov conditions) : pval=3.681574e-09
shapiro.test(stdres(lm_cas))$p.value

# index vs standardized residual: outliers present
plot(stdres(lm_cas), xlab = "index", ylab = "standardized residual", ylim = c(-3, 3))
abline(h = -2.5, lty = 2)
abline(h =  2.5, lty = 2)

# density plot of residuals
plot(density(lm_cas$residuals))


## 3.1b Apply transformation of casual and normalize

# try boxcox: pval to 
bcc <- boxcox(casual ~ . , data=TRN_cas)
bcc$x[which(bcc$y==max(bcc$y))]
lm_cas2 <- lm(bcPower(casual,lambda=0.26) ~ . , data=TRN_cas)

# check improvement: pval from 3.681574e-09 to 0.4014855
shapiro.test(stdres(lm_cas2))$p.value
plot(density(lm_cas2$residuals))
qqPlot(residuals(lm_cas2))
summary(lm_cas2)
plot(lm_cas2)

# apply power transform
TRN$casual <- bcPower(ORG$casual,lambda=0.26)

# standardisation
MEAN_CAS <- mean(TRN$casual)                                    # <--- apply same values to TST!
STDV_CAS <- sd(TRN$casual)                                      # <--- apply same values to TST!
TRN$casual <- (TRN$casual - MEAN_CAS)/STDV_CAS


# -------------------------------------------------------------------------


## 3.2a Linear regression, registered ~ all (after transforms of 2.2)

# get regression model
lm_reg <- lm(registered ~ . , data=TRN_reg)
summary(lm_reg)
plot(lm_reg)

# residual plots ifo. single variables
res_reg <- lm_reg$residuals
plot(TRN$temperature,res_reg,xlab='temperature',ylab='residuals')
plot(TRN$windspeed,res_reg,xlab='windspeed',ylab='residuals')
plot(TRN$humidity,res_reg,xlab='humidity',ylab='residuals')

# testing normality of residuals (Gauss-Markov conditions)
shapiro.test(stdres(lm_reg))$p.value

# index vs standardized residual: outliers present
plot(stdres(lm_reg), xlab = "index", ylab = "standardized residual", ylim = c(-3, 3))
abline(h = -2.5, lty = 2)
abline(h =  2.5, lty = 2)

# density plot of residuals
plot(density(lm_reg$residuals))


## 3.2b Try transformation of registered and normalize

# try boxcox
bcr <- boxcox(registered ~ . , data=TRN_reg)
bcr$x[which(bcr$y==max(bcr$y))]
lm_reg2 <- lm(bcPower(registered,lambda=0.79) ~ . , data=TRN_reg)

# check improvement: pval from 9.575064e-07 to 6.911854e-08
shapiro.test(lm_reg2$residuals)$p.value
plot(density(lm_reg2$residuals))
qqPlot(residuals(lm_reg2))
summary(lm_reg2)
plot(lm_reg2)

# original distribution still performs better --> NO power transformation
# but for consistency, apply standardisation
MEAN_REG <- mean(TRN$registered)                                  # <--- apply same values to TST!
STDV_REG <- sd(TRN$registered)                                    # <--- apply same values to TST!
TRN$registered <- (TRN$registered - MEAN_REG)/STDV_REG

# check impact for consistency: slightly better
lm_reg3 <- lm(registered ~ . -casual, data=TRN)
shapiro.test(lm_reg3$residuals)$p.value
plot(density(lm_reg3$residuals))
qqPlot(residuals(lm_reg3))
summary(lm_reg3)
plot(lm_reg3)


# -------------------------------------------------------------------------


## 3.3 Update datasets
TRN_cas <- dplyr::select(TRN, -registered)
TRN_reg <- dplyr::select(TRN, -casual)

## NOTE
# QQplots indicate outliers for casual (273,14) and registered (9,157)


# ## Export image
# png(filename = paste(img_dir,'/3.png',sep=''),
#     width = 1000,height = 600,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(2,2))
# # residuals for original response vars
# plot(lm_cas$fitted.values, lm_cas$residuals,xlab="fitted values",ylab="residuals",main="Casual (original)")
# plot(lm_reg$fitted.values, lm_reg$residuals,xlab="fitted values",ylab="residuals",main="Registered (original)")
# # residuals for transformed (& standardized) response vars
# plot(lm_cas2$fitted.values, lm_cas2$residuals,xlab="fitted values",ylab="residuals",main="Casual (after transformation)")
# plot(lm_reg3$fitted.values, lm_reg3$residuals,xlab="fitted values",ylab="residuals",main="Registered (after transformation)")
# dev.off()


# -------------------------------------------------------------------------
# 4 - Outlier detection
# -------------------------------------------------------------------------

## 4.1 Robust regression diagnostics, only non-categorical predictors

# casual
# leverage points (good & bad), some vertical outliers
lts_cas <- ltsReg(casual ~ feelingtemperature + humidity + windspeed, 
                  data=TRN_cas, alpha=0.5, seed=set.seed(123))
summary(lts_cas)
plot(lts_cas)

# registered
# 3 (neg) vertical outliers, 9 good lvg, one bad leverage point (21)
lts_reg <- ltsReg(registered ~ feelingtemperature + humidity + windspeed, 
                  data=TRN_reg, alpha=0.5, seed=set.seed(123))
summary(lts_reg)
plot(lts_reg)

# marked datapoints:
which(lts_cas$lts.wt==0)
which(lts_reg$lts.wt==0)


## 4.2 Robust regression diagnostics, all predictors (except temp due to multicollinearity)

# casual: 273 remains an outlier, two new outliers
lts_cas2 <- ltsReg(casual ~ -temperature, data=TRN_cas, alpha=0.5, seed=set.seed(123))
summary(lts_cas2)
plot(lts_cas2)

# registered: no remarkable residuals
lts_reg2 <- ltsReg(registered ~ -temperature, data=TRN_reg, alpha=0.5, seed=set.seed(123))
summary(lts_reg2)
plot(lts_reg2)

# marked datapoints:
which(lts_cas2$lts.wt==0)
which(lts_reg2$lts.wt==0)

## 4.3 Removing the strongest outliers

# casual
out_cas <- c(20,21,24,105,108,169,184,257,271,273,274,310)
TRN_cas_rmo <- TRN_cas[-out_cas,,drop=FALSE]

# registered
out_reg <- c(14,21,120,271)
TRN_reg_rmo <- TRN_reg[-out_reg,,drop=FALSE]


# ## Export images
# # Diagnostic plot Casual
# png(filename = paste(img_dir,'/4cas.png',sep=''),
#     width = 500,height = 400,units = 'px',
#     pointsize = 12,res = 100)
# plot(lts_cas,which='rdiag')
# dev.off()
# 
# # Diagnostic plot Registered
# png(filename = paste(img_dir,'/4reg.png',sep=''),
#     width = 500,height = 400,units = 'px',
#     pointsize = 12,res = 100)
# plot(lts_reg,which='rdiag')
# dev.off()



# -------------------------------------------------------------------------
# 5 - Variable selection
# -------------------------------------------------------------------------

## 5.1 Some hints from previous sections
summary(lm_cas)
summary(lm_reg)


## 5.2a Variable selection - casual

# interesting combinations/subsets
comb_cas <- regsubsets(casual ~ . , data = TRN_cas_rmo, nbest=1, nvmax=11)
for (diagn in c("bic","Cp","adjr2","r2")){
  plot(comb_cas,scale=diagn)
}

# choosing a set of variables based on R2
numvar <- as.numeric(row.names(summary(comb_cas)$which))
rsq <- summary(comb_cas)$rsq # also works for adjr2, cp
plot(numvar, rsq, pch = 16)
chosenR2 <- which.max(rsq)
summary(comb_cas)$which[chosenR2, ]

# idem previous, for AIC
aic <- summary(comb_cas)$bic + (2 - log(nrow(TRN_cas_rmo))) * numvar # BIC => AIC conversion
plot(numvar, aic, pch = 16)
chosenaic <- which.min(aic)
summary(comb_cas)$which[chosenaic, ]


## 5.2b Variable selection - registered

# interesting combinations/subsets
comb_reg <- regsubsets(registered ~ . , data = TRN_reg_rmo, nbest=1, nvmax=11)
for (diagn in c("bic","Cp","adjr2","r2")){
  plot(comb_reg,scale=diagn)
}

# choosing a set of variables based on R2
numvar <- as.numeric(row.names(summary(comb_reg)$which))
rsq <- summary(comb_reg)$rsq # also works for adjr2, cp
plot(numvar, rsq, pch = 16)
chosenR2 <- which.max(rsq)
summary(comb_reg)$which[chosenR2, ]

# idem previous, for AIC
aic <- summary(comb_reg)$bic + (2 - log(nrow(TRN_reg_rmo))) * numvar # BIC => AIC conversion
plot(numvar, aic, pch = 16)
chosenaic <- which.min(aic)
summary(comb_reg)$which[chosenaic, ]


## 5.3 Define final selections (relatively conservative given small datasets)

# choices based on conclusions from 5.2, summated in:
summary(comb_cas)$which[7, ]
summary(comb_reg)$which[7, ]

# casual, conservative choice based on 5.2a
TRN_selcas <- TRN_cas_rmo %>% dplyr::select(-holiday, -temperature, -humidity, -windspeed)

# registered, conservative choice based on 5.2b
TRN_selreg <- TRN_reg_rmo %>% dplyr::select(-holiday, -temperature, -humidity, -windspeed)


## Export images

# # regsubsets for varying metrics - casual
# png(filename = paste(img_dir,'/5a.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,4))
# for (diagn in c("bic","Cp","adjr2","r2")){
#   plot(comb_cas,scale=diagn)
# }
# dev.off()
# 
# # regsubsets for varying metrics - registered
# png(filename = paste(img_dir,'/5b.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,4))
# for (diagn in c("bic","Cp","adjr2","r2")){
#   plot(comb_reg,scale=diagn)
# }
# dev.off()
# 
# # regsubsets for varying metrics - both
# png(filename = paste(img_dir,'/5ab.png',sep=''),
#     width = 1000,height = 600,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(2,4))
# for (diagn in c("bic","Cp","adjr2","r2")){
#   plot(comb_cas,scale=diagn)
# }
# for (diagn in c("bic","Cp","adjr2","r2")){
#   plot(comb_reg,scale=diagn)
# }
# dev.off()



# -------------------------------------------------------------------------
# 6 - Final models
# -------------------------------------------------------------------------

## 6.1 Casual

# linear regression model
lm_selcas <- lm(casual ~ . , data=TRN_selcas)
summary(lm_selcas)

# testing normality: ok!
shapiro.test(stdres(lm_selcas))$p.value
plot(lm_selcas)


## 6.2 Registered

# linear regression model
lm_selreg <- lm(registered ~ . , data=TRN_selreg)
summary(lm_selreg)

# testing normality: not ok... but better than previous regressions
shapiro.test(stdres(lm_selreg))$p.value
plot(lm_selreg)


## Export images

# # casual plots
# png(filename = paste(img_dir,'/6cas.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,2))
# for (i in c(1,2)){
#   plot(lm_selcas,which=c(i))
# }
# dev.off()
# 
# # registered plots
# png(filename = paste(img_dir,'/6reg.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,2))
# for (i in c(1,2)){
#   plot(lm_selreg,which=c(i))
# }
# dev.off()
# 
# # plots above combined
# # registered plots
# png(filename = paste(img_dir,'/6cr.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,4))
# for (i in c(1,2)){
#   plot(lm_selcas,which=c(i))
# }
# for (i in c(1,2)){
#   plot(lm_selreg,which=c(i))
# }
# dev.off()



# -------------------------------------------------------------------------
# 7 - Performance
# -------------------------------------------------------------------------

## 7.1 Prepping test set

# original copy
TST_org <- TST
n <- length(TST$casual)

# set factors
for (i in cols_catg){
  TST[[i]] <- as.factor(TST_org[[i]])
}

# Apply same transformations : CASUAL                               # ref. 3.1b
TST$feelingtemperature <- logitlink(TST_org$feelingtemperature)
TST$casual <- bcPower(TST_org$casual,lambda=0.26)
TST$casual <- (TST$casual - MEAN_CAS)/STDV_CAS

# Apply same transformations : REGISTERED                           # ref. 3.2b
TST$registered <- (TST$registered - MEAN_REG)/STDV_REG

# Some indications that prediction will perform worse on TST set:
# deviation from zero-mean
mean(TRN$casual)
mean(TST$casual)
mean(TRN$registered)
mean(TST$registered)
# standard deviation amplified for TST$registered
sd(TRN$casual)
sd(TST$casual)
sd(TRN$registered)
sd(TST$registered)
# correlation (predictability) between casual and registered lower in TST set
cor(TRN$casual,TRN$registered)
cor(TST$casual,TST$registered)

# Make separate datasets
TST_selcas <- TST %>% dplyr::select(-holiday, -temperature, -humidity, -windspeed, -registered)
TST_selreg <- TST %>% dplyr::select(-holiday, -temperature, -humidity, -windspeed, -casual)


## 7.2 Get predictions & assess performance

# get predictions
pred_cas <- predict(lm_selcas,TST_selcas,interval='prediction')[,1]
pred_reg <- predict(lm_selreg,TST_selreg,interval='prediction')[,1]

# residuals
res_cas <- pred_cas - TST_selcas$casual
res_reg <- pred_reg - TST_selreg$registered
plot(res_cas)
plot(res_reg)

mean(res_cas)
sd(res_cas)
mean(res_reg)
sd(res_reg)

# Sum of Squares, casual
SST_cas <- sum((TST_selcas$casual - mean(TST_selcas$casual))^2)
SSR_cas <- sum((pred_cas - mean(TST_selcas$casual))^2)
SSE_cas <- sum((TST_selcas$casual - pred_cas)^2)
SST_cas
SSR_cas+SSE_cas

# Sum of Squares, registered
SST_reg <- sum((TST_selreg$registered - mean(TST_selreg$registered))^2)
SSR_reg <- sum((pred_reg - mean(TST_selreg$registered))^2)
SSE_reg <- sum((TST_selreg$registered - pred_reg)^2)
SST_reg
SSR_reg+SSE_reg

# R2
R2_cas <- SSR_cas/SST_cas
R2_reg <- SSR_reg/SST_reg
R2_cas
R2_reg

# adj R2
R2a_cas <- 1 - (n-1)/(n-7)*SSE_cas/SST_cas
R2a_reg <- 1 - (n-1)/(n-7)*SSE_reg/SST_reg
R2a_cas
R2a_reg

# RMSE
RMSE_cas <- sqrt(sum(res_cas^2)/n)
RMSE_reg <- sqrt(sum(res_reg^2)/n)
RMSE_cas
RMSE_reg

# MAE
MAE_cas <- sum(abs(res_cas))/n
MAE_reg <- sum(abs(res_reg))/n
MAE_cas
MAE_reg

# Pearson correlation
CP_cas <- cor(pred_cas,TST_selcas$casual,method='pearson')
CP_reg <- cor(pred_reg,TST_selreg$registered,method='pearson')
CP_cas
CP_reg


## 7.3 Overview of results

RESULTS <- data.frame(
  "RMSE" = c(RMSE_cas,RMSE_reg),
  "MAE"  = c(MAE_cas,MAE_reg),
  "R2"   = c(R2_cas,R2_reg),
  "R2a"  = c(R2a_cas,R2a_reg),
  "Cp"   = c(CP_cas,CP_reg))

RESULTS

# > RESULTS
#        RMSE      MAE        R2        R2a        Cp
# 1 0.6709274 0.549540 0.7996325  0.5416647 0.8840747
# 2 1.8633790 1.739227 2.0591360 -0.9938992 0.8579289



## Export images

# # residuals on predictions
# png(filename = paste(img_dir,'/7predres.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,4))
# plot(pred_cas,res_cas,xlab='predicted values',ylab='residuals',main='Casual')
# qqnorm(res_cas)
# qqline(res_cas)
# plot(pred_reg,res_reg,xlab='predicted values',ylab='residuals',main='Registered')
# qqnorm(res_reg)
# qqline(res_reg)
# dev.off()


