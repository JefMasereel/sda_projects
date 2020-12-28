
###############################################
## Project 1 SDA - by Jef Masereel, r0631651 ##
###############################################

## NOTE ON CODE REFERENCES
# some code was written by reference to the provided exercise solutions from earlier sessions.
# besides this, some online sources such as Quick-R and Stackoverflow were consulted at times.

library(car) # needed for functions powerTransform() and bcPower()
library(MASS) # needed for function boxcox()
library(readr)
library(dplyr)

rm(list = ls())

# Jef Masereel - Group 19
D = read.csv("Group19.csv")

# directory to write images to
dir <- '~/1_KUL/BME - 2e jaar/SEM2/SDA/Taak 1/images'
# filename=paste(dir,'/test.pdf',sep='')


# -------------------------------------------------------------------------
# Ex 1 - Explorative analysis and transformation to normality
# -------------------------------------------------------------------------

## 1.1 Explorative data analysis

# general inspection
summary(D)

# remove all non-continuous variables
Dc <- D %>% select(-TypeOfSteel_A300,-TypeOfSteel_A400,-Outside_Global_Index,-Flt)

# boxplots for each variable
for (i in colnames(Dc)){
  boxplot(Dc[i],xlab=i,horizontal = TRUE)
}

# pairwise plots (bivariate normality and dependencies)
pairs(Dc)


## 1.2 Transformation to normality

# check univariate normality
Pvals = vector(length = 22)
j = 1
for (i in colnames(Dc)){
  hist(Dc[[i]],xlab=i,main='')
  plot(density(Dc[[i]]),xlab=i,main='')
  qqnorm(Dc[[i]],main=i)
  qqline(Dc[[i]])
  
  Pvals[j] <- shapiro.test(Dc[[i]])$p.value
  j <- j+1
}

## NOTE
# Based on the previous plots, the transformations are considered
# Given the low results from Shapiro-Wilkes tests, no transformations are useful
# One transformation is performed below to demonstrate low impact
# full discussion in the report

Pvals # all extremely far from normal distribution, Empty_Index comes closest

## DEMO BOXCOX
# to apply boxcox, the variable should only have strictly positive observations
# Pvals (and plots) show that Empty_Index is closest to a normal disribution, 
# but it contains two outliers on zero (see boxplot). 
# These are removed for illustration of boxcox.

which(Dc$Empty_Index <= 0)
xx <- Dc %>% select(Empty_Index)
xp <- xx[-c(463,496),,drop=F]

lambda <- powerTransform(xp[[1]]~1)
bcxp <- bcPower(xp[[1]],coef(lambda))

# no useful improvement:
shapiro.test(xx[[1]])$p.value # before
shapiro.test(bcxp)$p.value    # after



# # export pairwise plots
# png(filename = paste(dir,'/11.png',sep=''),
#     width = 1000,height = 1000,units = 'px',
#     pointsize = 12,res = 100)
# pairs(Dc)
# dev.off()
# 
# # export plots for Length_of_Conveyer
# png(filename = paste(dir,'/12.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# hist(Dc$Length_of_Conveyer,xlab='Length_of_Conveyer',main='Histogram')
# plot(density(Dc$Length_of_Conveyer),main='Density plot')
# qqnorm(Dc$Length_of_Conveyer,main='QQ-Plot')
# qqline(Dc$Length_of_Conveyer)
# dev.off()
# 
# # export plots for Outside_X_Index
# png(filename = paste(dir,'/13.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# hist(Dc$Outside_X_Index,xlab='Outside_X_Index',main='Histogram')
# plot(density(Dc$Outside_X_Index),main='Density plot')
# qqnorm(Dc$Outside_X_Index,main='QQ-Plot')
# qqline(Dc$Outside_X_Index)
# dev.off()
# 
# # export plots for Empty_Index before BoxCox
# png(filename = paste(dir,'/14.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# hist(Dc$Empty_Index,xlab='Empty_Index (original)',main='Histogram')
# plot(density(Dc$Empty_Index),main='Density plot')
# qqnorm(Dc$Empty_Index,main='QQ-Plot')
# qqline(Dc$Empty_Index)
# dev.off()
# 
# # export plots for Empty_Index after BoxCox
# png(filename = paste(dir,'/15.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# hist(bcxp,xlab='Empty_Index (BoxCox)',main='Histogram')
# plot(density(bcxp),main='Density plot')
# qqnorm(bcxp,main='QQ-Plot')
# qqline(bcxp)
# dev.off()



# -------------------------------------------------------------------------
# Ex 2
# -------------------------------------------------------------------------

library(rrcov)

## 2.1 PCA analysis

# Apply PCA, scaled (corr)
DPCA <- PcaClassic(Dc, scale=TRUE)

# Insepct PCA results
screeplot(DPCA,type="lines",main='screeplot DPCA')
summary(DPCA)

# choose first 6 (84%) or 8 (91%) PCs
DPCA6 <- PcaClassic(Dc,k=6,scale=TRUE)
DPCA8 <- PcaClassic(Dc,k=8,scale=TRUE)


## 2.2 Inspect 3 largest PCs and corresponding scores

# collect 3 PCs
DPCA3 <- PcaClassic(Dc,k=3,scale=TRUE)

# define Flt groups
group <- NA
group[which(D$Flt=='Z_Scratch')] <- 1
group[which(D$Flt=='K_Scratch')] <- 2
group[which(D$Flt=='Other_Faults')] <- 3

# Scatter plots
pairs(DPCA3@scores,
      col=c(28,34,'green')[group],
      pch=c(1,2,3)[group],
      labels=c('PC1','PC2','PC3'),
      main='scatterplot 3 largest PCs')

# Diagnostic plot
# par(mfrow=c(2,2))
plot(DPCA3,
     col=c(28,34,'green')[group],
     pch=c(1,2,3)[group],
     main='Diagnostic k=3')

plot(DPCA6,
     col=c(28,34,'green')[group],
     pch=c(1,2,3)[group],
     main='Diagnostic k=6')

plot(DPCA8,
     col=c(28,34,'green')[group],
     pch=c(1,2,3)[group],
     main='Diagnostic k=8')

plot(DPCA,
     col=c(28,34,'green')[group],
     pch=c(1,2,3)[group],
     main='')



# # export screeplot DPCA
# png(filename = paste(dir,'/21.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# screeplot(DPCA,type="lines",main='',npcs=22)
# abline(v = 8)
# dev.off()
# 
# # export pairs DPCA3 scores
# png(filename = paste(dir,'/22.png',sep=''),
#     width = 1000,height = 600,units = 'px',
#     pointsize = 12,res = 100)
# pairs(DPCA3@scores,
#       col=c(28,34,'green')[group],
#       pch=c(1,2,3)[group],
#       labels=c('PC1','PC2','PC3'),
#       main='')
# dev.off()
# 
# # export diagnostic plots
# png(filename = paste(dir,'/23.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# plot(DPCA3,
#      col=c(28,34,'green')[group],
#      pch=c(1,2,3)[group],
#      main='Diagnostic k=3')
# plot(DPCA8,
#      col=c(28,34,'green')[group],
#      pch=c(1,2,3)[group],
#      main='Diagnostic k=8')
# plot(DPCA,
#      col=c(28,34,'green')[group],
#      pch=c(1,2,3)[group],
#      main='')
# dev.off()



# -------------------------------------------------------------------------
# Ex 3
# -------------------------------------------------------------------------

library("cluster")
source('code_utils.R')

## 3.1 Apply different clustering methods

# PAM (74.7% bac)
Dpam <- pam(DPCA8@scores,3)

# divisive (59.7% bac)
Ddiv <- diana(DPCA8@scores,3,diss=FALSE)

# # agglomerative (49.5% bac)
# Dagg <- agnes(DPCA8@scores,3,diss=FALSE,metric = 'manhattan',method='ward')
# mycomp(D$Flt,cutree(Dagg,k=3))[[1]]

# # fuzzy (very poor performance due to high uncertainty)
# Dfuz <- fanny(DPCA8@scores,3,metric='manhattan',memb.exp = 1.5)
# plot(Dfuz)


## 3.2 Inspect corresponding plots
plot(Dpam)
plot(Ddiv)


## 3.3 Compare to actual Flt clusters
# see utils.R for own analysis tools

# PAM
mycomp(D$Flt,Dpam$clustering)[[1]]

# diana
mycomp(D$Flt,cutree(Ddiv,k=3))[[1]]



# # export Dpam clusplot
# png(filename = paste(dir,'/31.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# clusplot(Dpam,xlab='PC1',ylab='PC2',main='')
# dev.off()
# 
# # export Ddiv dendrogram
# png(filename = paste(dir,'/32.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# plot(as.dendrogram(Ddiv))
# dev.off()



# -------------------------------------------------------------------------
# Ex 4
# -------------------------------------------------------------------------

library('robustbase')

## 4.1 Outlier removal

# largest cluster is #2 (Other_Faults)
length(which(Dpam$clustering==1))
length(which(Dpam$clustering==2))
length(which(Dpam$clustering==3))

# according to permutation matrix from mycomb, cluster 2 = 'Other_Faults'
mycomp(D$Flt,Dpam$clustering)[[2]][4,]

# select observation scores from largest cluster
Dscores_pam2 <- DPCA8@scores[which(Dpam$clustering==2),,drop=FALSE]

# compute MCD estimator
Dmcd <- covMcd(Dscores_pam2,alpha = 1)

# visual inspection
plot(Dmcd)

# 93 outliers detected
length(which(Dmcd$mcd.wt==0))

# remove outliers from scores
Drmo <- Dscores_pam2[-which(Dmcd$mcd.wt==0),,drop=FALSE]


## 4.2 Multivariate normality check

# QQ plot
qqnorm(Drmo)
qqline(Drmo)



# # export Dmcd plots
# png(filename = paste(dir,'/41.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# par(mfrow=c(1,3))
# plot(Dmcd,which = c("distance"))
# plot(Dmcd,which = c('dd'))
# plot(Dmcd,which = c("qqchi2"))
# dev.off()
# 
# # export Drmo QQ plot
# png(filename = paste(dir,'/42.png',sep=''),
#     width = 1000,height = 300,units = 'px',
#     pointsize = 12,res = 100)
# qqnorm(Drmo,main='')
# qqline(Drmo)
# dev.off()


