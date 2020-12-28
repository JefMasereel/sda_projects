##############################
#  SMDA: EXERCISE SESSION 8  #
##############################


rm(list = ls())

library(car)
library(MASS)
library(pls)
library(robustbase)
library(rrcov)
library(SMPracticals)



# EXERCISE 1 --------------------------------------------------------------------------------------

?pollution
data(pollution)
a1 <- pollution
dim(a1)
names(a1)

a2 <- pollution
a2$hc  <- log(a1$hc)
a2$nox <- log(a1$nox)
a2$so  <- log(a1$so)

head(a2)


## QUESTION 1.1
crm <- cor(a2[, -which(names(a2) == "mort")]) # = cor(a2[, -16])
crm
diag(solve(crm))       # VIF values
max(diag(solve(crm)))  # > 10: strong multicollinearity
mean(diag(solve(crm))) # >> 1: strong multicollinearity

ev <- eigen(crm)
ev$values / sum(ev$values)              # some small values: indication of multicollinearity
sqrt( max(ev$values) / min(ev$values) ) # < 30: no indication of strong multicollinearity


## QUESTION 1.2
a3 <- a2[1:50, ]

## PCA
resultpca <- PcaClassic(a3[, -which(names(a3)=="mort")], scale = TRUE)
screeplot(resultpca, type = "lines")
summary(resultpca)
# 5 components have eigenvalue smaller than 1 and explain more than
# 80% of the total variance
# 7 components explain more than 90%

## PCR
?pcr
resultpcr5 <- pcr(mort ~ ., data = a3, scale = TRUE, ncomp = 5)
coef(resultpcr5, intercept = TRUE)

## manually, check how we obtain these values
pcascores <- getScores(resultpca)
pcat <- pcascores[, 1:5]
coefpca <- coefficients(lm(mort ~ pcat-1, data = a3)) # alpha-hat, page 141
pcaloadings <- getLoadings(resultpca)
pcaloadings[, 1:5] %*% coefpca # beta-plus, last page 141
# gives the same result!

## LS on original variables: coefs are very different
resultlm <- lm(mort ~ ., data = a3)
summary(resultlm)
t(t(coef(resultlm)))

## choose number of components based on LOO-CV (= leave-one-out cross-validation).
resultpcrLOO <- pcr(mort ~ ., data = a3, scale = TRUE, validation = "LOO")
summary(resultpcrLOO)
coef(resultpcrLOO)
# minimum at 7 components

?RMSEP
plot(RMSEP(resultpcrLOO, estimate = "train"), main = "training RMSEP")
plot(RMSEP(resultpcrLOO, estimate = "CV"), main = "CV MSEP")
plot(RMSEP(resultpcrLOO, newdata = a2[51:60, ], estimate = "test"), main = "validation RMSEP")

plot(RMSEP(resultpcrLOO, estimate = "all", newdata = a2[51:60, ]))
# black curve corresponds with training
# red/green with CV and adjCV
# blue with validation set
validationplot(resultpcrLOO, val.type = "RMSEP", estimate = "all", newdata = a2[51:60, ])

# note: validation set is very small here, so the results are quite variable.



# EXERCISE 2 --------------------------------------------------------------------------------------

## QUESTION 2.1

lambdaR <- exp(seq(log(1/1000), 0, length.out = 100))
lambdaR
plot(lambdaR)

## ridge trace
result.ridge <- lm.ridge(mort ~ ., data = a3, lambda = lambdaR)
plot(result.ridge)

## mean VIF values
result <- rep(NA, length(lambdaR))

for (i in 1:length(lambdaR)){
  d1 <- try(solve( crm + lambdaR[i] * diag(ncol(crm)) ))
  if(is.numeric(d1)){
    result[i] <- mean(diag(d1 %*% crm %*% d1))
  }
}

plot(lambdaR, result)
abline(h = 1)
lambdaS <- lambdaR[which(result < 1)[1]]
lambdaS


## QUESTION 2.2

result.ridgeS <- lm.ridge(mort ~ ., data = a3, lambda = lambdaS)
coef(result.ridgeS)


## QUESTION 2.3

# RMSEP on validation set
Xr <- data.matrix(a2[51:60,!(names(a3)=="mort")])
dim(Xr)
sqrt(mean( (cbind(1, Xr) %*% coef(result.ridgeS) - a2$mort[51:60])^2 ))

# note that coef(result.ridgeS) != result.ridgeS$coef
# (the former removes the standardization)
# you can compare the results of coef(result.ridgeS) when lambda = 0
# with coefficients(resultlm).



# EXERCISE 3 --------------------------------------------------------------------------------------

?hills

## QUESTION 3.1

## classical analysis
hills.lm <- lm(time ~ dist + climb, data = hills)

## some residual plots
studr <- studres(hills.lm)

plot(fitted(hills.lm), studr, ylim = c(-2.5, 8))
abline(h = 2.5, lty = 2)
abline(h = -2.5, lty = 2)
# With the 'identify' command you can select data points on the plot and then the
# labels of the data points appear (these labels are stored in row.names(hills)).
# This allows you to identify the outlying data points. Click with the left mouse button
# next to the data points you want to identify. Stop by pressing Esc.
identify(fitted(hills.lm), studr, row.names(hills))
qqnorm(studr)
qqline(studr)
# The data points with large studentized residual are 7 and 18.


## QUESTION 3.2

# some diagnostics
hills.lms <- summary(hills.lm)
hills.lmi <- lm.influence(hills.lm)
names(hills.lmi)
hills.infl <- influence.measures(hills.lm)
hills.hat <- hatvalues(hills.lm)
hills.dffits <- dffits(hills.lm)
hills.dfbetas <- dfbetas(hills.lm)
hills.cookd <- cooks.distance(hills.lm)

# diagonal elements of hat matrix h_{ii}
hii <- hills.hat

# identify data points whose leverage is larger than 2*p/n
p <- hills.lm$rank
n <- length(hii)
cbind(1:n, hills, lev = hii)[hii > 2*p/n, ]

# DFFITS: identify data points whose |DFFITS| is larger than 2*sqrt(p/n)
cbind(1:n, hills = hills.dffits)[abs(hills.dffits) > 2*sqrt(p/n),]

# DFBETAS: identify data points whose |DFBETAS| is larger than 2/sqrt(n)
cbind(1:n, hills, dfbetas = hills.dfbetas)[apply(abs(hills.dfbetas) > 2/sqrt(n), 1, any), ]

# Cook's distance: look at last plot or compute it exactly
plot(hills.lm)

# identify data points whose Cook's distance is larger than 1
cbind(1:n, hills, cd=hills.cookd)[hills.cookd > 1,]

# drop cases 7, 11 (influential) and 18 (large residual) and compare the results
hills.lm2 <- lm(time ~ dist + climb, data = hills[-c(7, 11, 18), ])
summary(hills.lm2)
summary(hills.lm)


## QUESTION 3.3

# Robust regression with approx. 50% breakdown value
# Fix the seed to be sure to have the same solution anytime

hills.lts = ltsReg(time ~ dist + climb, data = hills, seed = set.seed(2019))
coef(hills.lts)
hills.lts$raw.coefficients
hills.lts$lts.wt


## QUESTION 3.4

plot(hills.lts, id.n = 6)
# last plot is the diagnostic plot: observation 18 is found as a vertical outlier,
# observations 7, 11, 33, 35 as bad leverage points, obs. 31 as good leverage point.
ltsPlot(hills.lts)
plot(hills.lts, which = "rqq")
plot(hills.lts, which = "rdiag")


## QUESTION 3.5

mod1 <- covMcd(hills[, 1:2])
plot(hills[,1:2], xlim = c(-10, 25), ylim = c(-2000, 8000))
rd <- sqrt(qchisq(0.95,df = 2))
car::ellipse(center = colMeans(hills[, 1:2]),shape = cov(hills[, 1:2]), radius = rd)
car::ellipse(center = mod1$center, shape = mod1$cov, radius = rd, col = "blue")

plot(mod1, which = "dd")


## QUESTION 3.6
# compare using a smaller breakdown value
hills.lts2 = ltsReg(time ~ dist + climb, hills, alpha = 0.75, seed = set.seed(3))
coef(hills.lts2)


