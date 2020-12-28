##############################
#  SMDA: EXERCISE SESSION 7  #
##############################


rm(list = ls())




library(DAAG)
library(leaps)
library(MASS)



cafe <- read.table("cafedata.txt", header = TRUE)

str(cafe)
head(cafe)
class(cafe$Day_of_Week)

acal <- cafe[1:30, ]     # calibration data
aval <- cafe[-c(1:30), ] # validation data
dim(acal) # 30 x 17
dim(aval) # 17 x 17



# EXERCISE 1 --------------------------------------------------------------------------------------

?regsubsets
?plot.regsubsets

salesall <- regsubsets(Sales ~ . , data = acal, nbest = 1, nvmax = 19)


## SUMMARY OF BEST MODELS
str(summary(salesall))

summary(salesall, matrix.logical = TRUE)
plot(salesall, scale = "r2")


## PLOT R2
summary(salesall)$which
numvar <- as.numeric(row.names(summary(salesall)$which))
numvar
rsq <- summary(salesall)$rsq
plot(numvar, rsq, pch = 16)
chosenR2 <- which.max(rsq)
chosenR2
# model 19 gives best result (as expected)


## PLOT R2adjusted
adjrsq <- summary(salesall)$adjr2
plot(numvar, adjrsq, pch = 16) 
chosenadjR2 <- which.max(adjrsq)
chosenadjR2
# model 13 gives best result
summary(salesall)$which[chosenadjR2, ]
max(adjrsq)
plot(salesall, scale = "adjr2")


## PLOT AIC
aic <- summary(salesall)$bic + (2 - log(nrow(acal))) * numvar # BIC => AIC conversion
plot(numvar, aic, pch = 16)
chosenaic <- which.min(aic)
chosenaic
#model 13 gives best result
summary(salesall)$which[chosenaic, ]


## PLOT Cp
cp <- summary(salesall)$cp
plot(numvar, cp, pch = 16)
chosencp <- which.min(cp)
chosencp
# model 5 gives best result
summary(salesall)$which[chosencp, ]


designcal <- model.matrix(lm(Sales ~ . , data = acal))
designval <- model.matrix(lm(Sales ~ . , data = aval))


## PRESS for best model according to R2
summary(salesall)$which[chosenR2, ]
selR2 <- names(which( summary(salesall)$which[chosenR2, !colnames(summary(salesall)$which) %in% c("(Intercept)")] == 1))
acal2 <- cbind(designcal[, selR2], acal[, "Sales", drop = FALSE])
aval2 <- cbind(designval[, selR2], aval[, "Sales", drop = FALSE])

mod2 <- lm(Sales ~ . , data = acal2)

cvpress2 <- press(mod2)
# or explicitly
X <- model.matrix(mod2)
cvpress2 <- sum( ( mod2$resid / (1 - diag( X%*% solve(crossprod(X))%*%t(X)) ) )^2 )
cvpress2
# CV-PRESS = 54603.62
mcvpress2 <- cvpress2/nrow(acal2) 
mcvpress2
# mean cv-press = 1820.121

## MSEP on validation set
fittedval2 <- predict(mod2, newdata = aval2, interval = "prediction")[, 1]
sum((fittedval2-aval2$Sales)^2)/nrow(aval2)
# MSEP = 1418.118


## PRESS for best model according to adjR2
seladjR2 <- names(which(summary(salesall)$which[chosenadjR2, !colnames(summary(salesall)$which) %in% c("(Intercept)")] == 1))
acal3 <- cbind(designcal[, seladjR2], acal[, "Sales", drop = FALSE])
aval3 <- cbind(designval[, seladjR2], aval[, "Sales", drop = FALSE])

mod3 <- lm(Sales ~ . , data = acal3)
cvpress3 <- press(mod3)
cvpress3
# CV-PRESS = 21522.61
mcvpress3 <- cvpress3/nrow(acal3)
mcvpress3
# mean cv-press = 717.4204, better than R2

## MSEP on validation set
fittedval3 <- predict(mod3, newdata = aval3, interval = "prediction")[, 1]
sum((fittedval3 - aval3$Sales)^2) / nrow(aval3)
# MSEP = 1173.864, better than R2


## PRESS for best model according to AIC 
selaic <- names(which(summary(salesall)$which[chosenaic, !colnames(summary(salesall)$which) %in% c("(Intercept)")] == 1))
acal4 <- cbind(designcal[, selaic], acal[, "Sales", drop = FALSE])
aval4 <- cbind(designval[, selaic], aval[, "Sales", drop = FALSE])

mod4 <- lm(Sales ~ . , data = acal4)
cvpress4 <- press(mod4)
cvpress4
# CV-PRESS = 21522.61
mcvpress4 <- cvpress4/nrow(acal4)
mcvpress4
# mean cv-press = 717.4204, same as adjR2 (as expected)

## MSEP on validation set
fittedval4 = predict(mod4,newdata=aval4,interval="prediction")[, 1]
sum((fittedval4-aval4$Sales)^2)/nrow(aval4)
# MSEP = 1173.864, same as adjR2 (as expected)


## PRESS for best model according to Cp.
selcp <- names(which(summary(salesall)$which[chosencp, !colnames(summary(salesall)$which) %in% c("(Intercept)")] == 1))
acal5 <- cbind(designcal[, selcp], acal[, "Sales", drop = FALSE])
aval5 <- cbind(designval[, selcp], aval[, "Sales", drop = FALSE])

mod5 <- lm(Sales ~ . , data = acal5)
cvpress5 <- press(mod5)
cvpress5
# CV-PRESS = 16149.82
mcvpress5 <- cvpress5/nrow(acal5)
mcvpress5
# mean cv-press = 538.3273, better than adjR2

## MSEP on validation set
fittedval5 <- predict(mod5, newdata = aval5, interval = "prediction")[, 1]
sum((fittedval5 - aval5$Sales)^2) / nrow(aval5)
# MSEP = 1865.547, worse than adjR2

# Cp: model 5
# (Intercept)  Day_of_WeekThu  Day_of_WeekTue  Day_of_WeekWed  Fruit_Cup_Sold           Chips  

# AIC & adjR2: model 13
#      (Intercept)    Day_of_WeekMon    Day_of_WeekThu    Day_of_WeekTue    Day_of_WeekWed   Bread_Sand_Sold  Bread_Sand_Waste       Wraps_Waste      
#      Muffins_Sold   Muffins_Waste     Cookies_Waste    Fruit_Cup_Sold             Chips           Coffees  

# R2: model 19
# (Intercept)             Day_of_WeekMon             Day_of_WeekThu             Day_of_WeekTue             Day_of_WeekWed            Bread_Sand_Sold  
# Bread_Sand_Waste                 Wraps_Sold                Wraps_Waste               Muffins_Sold              Muffins_Waste               Cookies_Sold  
# Cookies_Waste             Fruit_Cup_Sold            Fruit_Cup_Waste                      Chips                     Juices                      Sodas  
# Coffees  Max_Daily_Temperature_.F.  



# EXERCISE 2 --------------------------------------------------------------------------------------

## START FROM FULL MODEL
mod6 <- lm(Sales ~ . , data = acal2)


##  BACKWARDS
?stepAIC
modFB <- stepAIC(mod6, list(lower = ~ 1, upper = ~ .), direction = "back", k = 2)
summary(modFB)$coeff[, 1]
# (Intercept)   Day_of_WeekMon   Day_of_WeekThu   Day_of_WeekTue   Day_of_WeekWed  Bread_Sand_Sold Bread_Sand_Waste      Wraps_Waste
# Muffins_Sold    Muffins_Waste   Cookies_Waste   Fruit_Cup_Sold   Chips            Sodas             Coffees       Max_Daily_Temperature_.F.


## STEPWISE
modFS = stepAIC(mod6, list(lower = ~ 1,upper = ~ .), direction = "both", k = 2)
summary(modFS)$coeff[, 1]
# same selection


## START FROM EMPTY MODEL
mod7 <- lm(Sales ~ 1, data = acal2)


## CREATE FORMULA
listofvars <- colnames(designcal)
listofvars <- listofvars[2:length(listofvars)]  # remove intercept
fullformula <- as.formula(paste("~", paste(listofvars, collapse = "+")))


## FORWARDS
modEF <- stepAIC(mod7, list(lower = ~ 1, upper = fullformula), direction = "forward", k = 2)
summary(modEF)$coeff[, 1]
#  (Intercept) Fruit_Cup_Sold Day_of_WeekTue Day_of_WeekThu          Chips Day_of_WeekWed 
# same selection as Cp


## STEPWISE
modES <- stepAIC(mod7, list(lower = ~ 1, upper = fullformula), direction = "both", k = 2)
summary(modES)$coeff[, 1]
# same selection


predict(modES, newdata = aval2, interval = "prediction")
sum((predict(modES, newdata = aval2, interval = "prediction")[, 1] - aval2$Sales)^2) / nrow(aval)
# MSEP = 1865.547 = same result as Cp


# EXERCISE 3 --------------------------------------------------------------------------------------

library(cluster)
library(rgl)

## Read windsor
windsor <- read.table("windsor.txt", header = TRUE, quote = "\"")
dim(windsor)
head(windsor)

## (a) Scale variables
apply(windsor, 2, mean)
apply(windsor, 2, sd)
boxplot(windsor)
windsor <- scale(windsor, center = TRUE, scale = TRUE)
plot3d(windsor)

## (b) kmeans analysis
kmeans_windsor <- kmeans(windsor, centers = 2)
kmeans_windsor
plot(silhouette(kmeans_windsor$cluster, dist(windsor)))

kmeans_windsor <- kmeans(windsor, centers = 3)
plot(silhouette(kmeans_windsor$cluster, dist(windsor)))

## (c) pam analysis
plot(silhouette(pam(windsor, k = 2)))
plot(silhouette(pam(windsor, k = 3)))
plot(silhouette(pam(windsor, k = 4)))
plot(silhouette(pam(windsor, k = 5)))

## (d) comparison
kmeans_windsor <- kmeans(windsor, centers = 2)
pam_windsor <- pam(windsor, k = 2)
plot(silhouette(kmeans_windsor$cluster, dist(windsor)))
plot(silhouette(pam_windsor))

clusplot(windsor, kmeans_windsor$cluster)
clusplot(pam_windsor)

## (e) agglomerative nesting
agn1 <- agnes(windsor, diss = FALSE, metric = "euclidean", method = "single")
agn2 <- agnes(windsor, diss = FALSE, metric = "euclidean", method = "complete")
agn3 <- agnes(windsor, diss = FALSE, metric = "euclidean", method = "average")
plot(agn1)
plot(agn2)
plot(agn3)

plot(silhouette(cutree(agn2, k = 2), dist(windsor)))
clusplot(windsor,cutree(agn2, k = 2))

## (e) divisive analysis
dia1 <- diana(windsor, diss = FALSE, metric = "euclidean")
plot(dia1)
plot(silhouette(cutree(dia1, k = 2), dist(windsor)))
clusplot(windsor, cutree(dia1, k = 2))

## (f) plot
plot3d(windsor, col = kmeans_windsor$cluster)
plot3d(windsor, col = pam_windsor$cluster)
plot3d(windsor, col = cutree(agn1, k = 2))
plot3d(windsor, col = cutree(agn2, k = 2))
plot3d(windsor, col = cutree(agn3, k = 2))
plot3d(windsor, col = cutree(dia1, k = 2))

windsorL <- read.table("Lwindsor.txt", header = TRUE, quote = "\"")

xtabs( ~ windsorL$label + kmeans_windsor$cluster)
xtabs( ~ windsorL$label + pam_windsor$cluster)
xtabs( ~ windsorL$label + cutree(agn1, k = 2))
xtabs( ~ windsorL$label + cutree(agn2, k = 2))
xtabs( ~ windsorL$label + cutree(agn3, k = 2))
xtabs( ~ windsorL$label + cutree(dia1, k = 2))

table(true_label = windsorL$label, cluster_label = kmeans_windsor$cluster)
table(true_label = windsorL$label, cluster_label = pam_windsor$cluster)
table(true_label = windsorL$label, cluster_label = cutree(agn1, k = 2))
table(true_label = windsorL$label, cluster_label = cutree(agn2, k = 2))
table(true_label = windsorL$label, cluster_label = cutree(agn3, k = 2))
table(true_label = windsorL$label, cluster_label = cutree(dia1, k = 2))



# EXERCISE 4 --------------------------------------------------------------------------------------

## Read skull
skull <- read.table("skulls.txt", header = TRUE, quote = "\"")

## (a) Scale variables
str(skull)
summary(skull)
boxplot(skull)
skull <- scale(skull, center = TRUE, scale = TRUE)

## (b) kmeans analysis
kmeans_skull <- kmeans(skull, centers = 3)
plot(silhouette(kmeans_skull$cluster, dist(skull)))
kmeans_skull <- kmeans(skull, centers = 4)
plot(silhouette(kmeans_skull$cluster, dist(skull)))
kmeans_skull <- kmeans(skull, centers = 5)
plot(silhouette(kmeans_skull$cluster, dist(skull)))
kmeans_skull <- kmeans(skull, centers = 6)
plot(silhouette(kmeans_skull$cluster, dist(skull)))
kmeans_skull <- kmeans(skull, centers = 2)
plot(silhouette(kmeans_skull$cluster, dist(skull)))

## (c) pam analysis
plot(silhouette(pam(skull, k = 2)))
plot(silhouette(pam(skull, k = 3)))
plot(silhouette(pam(skull, k = 4)))
plot(silhouette(pam(skull, k = 5)))
plot(silhouette(pam(skull, k = 6)))
pam_skull <-pam(skull, k = 2)

## (d) comparison
clusplot(skull, kmeans_skull$cluster)
clusplot(pam_skull)

## (e) agglomerative nesting
agn1 <- agnes(skull, diss = FALSE, metric = "euclidean", method = "single")
agn2 <- agnes(skull, diss = FALSE, metric = "euclidean", method = "complete")
agn3 <- agnes(skull, diss = FALSE, metric = "euclidean", method = "average")
plot(agn1)
plot(agn2)
plot(agn3)
plot(silhouette(cutree(agn1, k = 2), dist(skull)))
plot(silhouette(cutree(agn2, k = 2), dist(skull)))
plot(silhouette(cutree(agn3, k = 2), dist(skull)))
clusplot(skull, cutree(agn2, k = 2))

## (e) divisive analysis
dia1 <- diana(skull, diss = FALSE, metric = "euclidean")
plot(dia1)
plot(silhouette(cutree(dia1, k = 2), dist(skull)))
clusplot(skull,cutree(dia1, k = 2))

## (f) comparison

Lskull <- read.table("Lskulls.txt", header = TRUE, quote = "\"")
Lskull$x

xtabs( ~ Lskull$x + kmeans_skull$cluster)
xtabs( ~ Lskull$x + pam_skull$cluster)
xtabs( ~ Lskull$x + cutree(agn1, k = 2))
xtabs( ~ Lskull$x + cutree(agn2, k = 2))
xtabs( ~ Lskull$x + cutree(agn3, k = 2))
xtabs( ~ Lskull$x + cutree(dia1, k = 2))

table(true_label = Lskull$x, cluster_label = kmeans_skull$cluster)
table(true_label = Lskull$x, cluster_label = pam_skull$cluster)
table(true_label = Lskull$x, cluster_label = cutree(agn1, k = 2))
table(true_label = Lskull$x, cluster_label = cutree(agn2, k = 2))
table(true_label = Lskull$x, cluster_label = cutree(agn3, k = 2))
table(true_label = Lskull$x, cluster_label = cutree(dia1, k = 2))



# EXERCISE 5 --------------------------------------------------------------------------------------

## Read pottery
pottery <- read.table("pottery.txt", header = TRUE, quote = "\"")

## (a) Scale variables
boxplot(pottery)
summary(pottery)
pottery <- scale(pottery, center = TRUE, scale = TRUE)

## (b) kmeans analysis
kmeans_pottery <- kmeans(pottery, centers = 2)
plot(silhouette(kmeans_pottery$cluster, dist(pottery)))
kmeans_pottery <- kmeans(pottery, centers = 4)
plot(silhouette(kmeans_pottery$cluster, dist(pottery)))
kmeans_pottery <- kmeans(pottery, centers = 5)
plot(silhouette(kmeans_pottery$cluster, dist(pottery)))
kmeans_pottery <- kmeans(pottery, centers = 6)
plot(silhouette(kmeans_pottery$cluster, dist(pottery)))
kmeans_pottery <- kmeans(pottery, centers = 3)
plot(silhouette(kmeans_pottery$cluster, dist(pottery)))

## (c) pam analysis
plot(silhouette(pam(pottery, k = 2)))
plot(silhouette(pam(pottery, k = 3)))
plot(silhouette(pam(pottery, k = 4)))
plot(silhouette(pam(pottery, k = 5)))
plot(silhouette(pam(pottery, k = 6)))
pam_pottery <-pam(pottery, k = 3)

## (d) comparison
clusplot(pottery,kmeans_pottery$cluster)
clusplot(pam(pottery, k = 3))

## (e) agglomerative nesting
agn1 <- agnes(pottery, diss = FALSE, metric = "euclidean", method = "single")
agn2 <- agnes(pottery, diss = FALSE, metric = "euclidean", method = "complete")
agn3 <- agnes(pottery, diss = FALSE, metric = "euclidean", method = "average")
plot(agn1)
plot(agn2)
plot(agn3)
plot(silhouette(cutree(agn1, k = 3), dist(pottery)))
plot(silhouette(cutree(agn2, k = 3), dist(pottery)))
plot(silhouette(cutree(agn3, k = 3), dist(pottery)))

## (e) divisive analysis
dia1 <- diana(pottery, diss = FALSE, metric = "euclidean")
plot(dia1)
plot(silhouette(cutree(dia1, k = 3), dist(pottery)))

## (f) comparison
Lpottery <- read.table("Lpottery.txt", header = TRUE, quote = "\"")

xtabs( ~ Lpottery$x + kmeans_pottery$cluster)
xtabs( ~ Lpottery$x + pam_pottery$cluster)
xtabs( ~ Lpottery$x + cutree(agn1, k = 3))
xtabs( ~ Lpottery$x + cutree(agn2, k = 3))
xtabs( ~ Lpottery$x + cutree(agn3, k = 3))
xtabs( ~ Lpottery$x + cutree(dia1, k = 3))

table(true_label = Lpottery$x, cluster_label = kmeans_pottery$cluster)
table(true_label = Lpottery$x, cluster_label = pam_pottery$cluster)
table(true_label = Lpottery$x, cluster_label = cutree(agn1, k = 2))
table(true_label = Lpottery$x, cluster_label = cutree(agn2, k = 2))
table(true_label = Lpottery$x, cluster_label = cutree(agn3, k = 2))
table(true_label = Lpottery$x, cluster_label = cutree(dia1, k = 2))





