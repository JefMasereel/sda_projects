##############################
#  SMDA: EXERCISE SESSION 6  #
##############################


rm(list = ls())

library(aod)
library(car)
library(faraway)
library(MASS)



# EXERCISE 1 --------------------------------------------------------------------------------------

## QUESTION 1.1
?coagulation
str(coagulation)
head(coagulation)

boxplot(coagulation$coag ~ coagulation$diet)
coagulation[which(coagulation$diet == "C"), ]
# observation 13 could be an outlier, but we'll ignore this as the dataset is very small


## QUESTION 1.2
lmCoag <- lm(coag ~ diet - 1, data = coagulation) # "-1" removes the intercept

summary(lmCoag)
coef(lmCoag)

meanA <- coef(lmCoag)[1]
meanB <- coef(lmCoag)[2]
meanC <- coef(lmCoag)[3]
meanD <- coef(lmCoag)[4]
# the mean of each group is given by the lm coefficient because in the lm formula
#we used -1 to remve the intercept. 


scalediet <- summary(lmCoag)$sigma
scalediet


## QUESTION 1.3
AOVcoag <- aov(coagulation$coag ~ coagulation$diet)
summary(AOVcoag)  # significant difference between means because p-value <0.05


## QUESTION 1.4
leveneTest(coagulation$coag ~ coagulation$diet - 1) # homoskedasticity ok
qqnorm(lmCoag$residuals)     
qqline(lmCoag$residuals)
shapiro.test(lmCoag$resid) # residuals ok


## QUESTION 1.5
wald.test(Sigma = vcov(lmCoag), b = coef(lmCoag), L = t(c(0, 1, -1, 0))) # not different

# Here we have to specify L = t(c(0, 1, -1, 0))) because we want to test if beta2-beta3 =0.
#Hence (beta1,beta2,beta3,beta4) %*% t(c(0, 1, -1, 0))) = beta2-beta3

t.test(x = coagulation$coag[which(coagulation$diet == "B")],
       y = coagulation$coag[which(coagulation$diet == "C")],
       var.equal = TRUE)


## QUESTION 1.6
# We'll do it for B-A:

# Uncorrected CI:  
diffAB <- meanB - meanA
nA <- length(which(coagulation$diet == "A"))
nB <- length(which(coagulation$diet == "B"))
diffAB + c(-1, 1) * qt(1 - 0.05/2, df = lmCoag$df) * scalediet * sqrt(1/nB + 1/nA)

# with Bonferroni correction: 4*3/2 = 6 possible tests:
diffAB + c(-1, 1) * qt(1 - 0.05/6, df = lmCoag$df) * scalediet * sqrt(1/nB + 1/nA)

# Scheffé:  
k <- 4 # = number of groups
critvalue <- sqrt((k-1) * qf(0.95, df1 = k - 1, df2 = lmCoag$df)) # = M in course notes
diffAB + c(-1, 1) * critvalue * scalediet * sqrt(1/nB + 1/nA)


# TukeyHSD
TukeyHSD(AOVcoag)
plot(TukeyHSD(AOVcoag))

# The confidence intervals for the difference of the means do not contain 0 so  there is a significant difference 
#between the means of groups A and B


## QUESTION 1.7
wald.test(b = coef(lmCoag), Sigma = vcov(lmCoag), L = t(c(1, 1, -1, -1)))
# not significantly different



# EXERCISE 2 --------------------------------------------------------------------------------------

car2 <- read.table(file.choose(), header = TRUE, sep = ",")
str(car2)


## QUESTION 2.1

#car origin should be a qualitative variable 
car2$origin <- factor(car2$origin)
summary(car2)


## QUESTION 2.2
boxplot(car2$acceleration)
qqnorm(car2$acceleration)
qqline(car2$acceleration)

boxplot(car2$displacement, main = "displacement")
boxplot(car2$horsepower,   main = "horsepower")
boxplot(car2$weight,       main = "weight")
boxplot(car2$class,        main = "class")

pairs(car2)

# a linear model might be appropriate
# the boxplot suggests some very mild outliers in Horsepower 
# they are probably not very influential


## QUESTION 2.3
Mod1 <- lm(acceleration ~ ., data = car2)

summary(Mod1)
# not all predictors seem significant

plot(Mod1)
stres1 <- stdres(Mod1)
qqnorm(stres1)
qqline(stres1)
shapiro.test(stres1)
plot(density(stres1))
# normality of the residuals is rejected (right-skewed)


## QUESTION 2.4
hist(car2$acceleration, breaks = sqrt(nrow(car2)))
plot(density(car2$acceleration))
shapiro.test(car2$acceleration)

boxcox(acceleration ~ ., data = car2)
# let's take the optimal transform to be ^(-1/2).

car3 <- car2
car3$acceleration <- bcPower(car2$acceleration, lambda = -0.5)
plot(density(car3$acceleration))  
shapiro.test(car3$acceleration)

Mod2 <- lm(acceleration ~ ., data = car3)

summary(Mod2)
# several predictor variables do not seem to be significant

plot(Mod2)
stres2 <- stdres(Mod2)
qqnorm(stres2)
qqline(stres2)
shapiro.test(stres2)
plot(density(stres2))
# the error distribution is now long-tailed, but this is less worse for inference
# than a skewed distribution


# Perform partial F-test on cylinders, model, origin and class
Mod3 <- lm(acceleration ~ displacement + horsepower + weight, data = car3)
anova(Mod3, Mod2)
stres3 <- stdres(Mod3)
qqnorm(stres3)
qqline(stres3)
shapiro.test(stres3)
plot(density(stres3))



# EXERCISE 3 --------------------------------------------------------------------------------------

?fpe
str(fpe)

pairs(fpe)
boxplot(fpe)
boxplot(fpe$F)
head(fpe)
# some observations might be outlying/influential. The sample with largest value for
# variable F (obs. 5) also distorts the relation with the other variables, so we remove it.
pairs(fpe[-5, ])
fpe <- fpe[-5, ]


## QUESTION 3.1
lmFpe <- lm(A2 ~ 0 + A + B + C + D + E + F + G + H + J + K + N , data = fpe)


## QUESTION 3.2
summary(lmFpe)
summary(lmFpe)$r.squared
# R-squared is almost one

cor(fpe[, -c(1, 13)])
# high R^2 caused by high correlations between A2 and the other variables


## QUESTION 3.3
qqnorm(stdres(lmFpe))
qqline(stdres(lmFpe))
plot(lmFpe)
plot(fitted(lmFpe), lmFpe$resid^2)
plot(fpe$EI, abs(lmFpe$resid))
# some degree of heteroskedasticity


## QUESTION 3.4
plot(fpe$EI)
# heteroskedasticity could be caused by these different sizes 
# => compensate this (e.g. by setting weights prop. to 1/EI.) 
wi <- 1/fpe$EI

lmFpe2<-lm(A2 ~ A + B + C + D + E + F + G + H + J + K + N - 1, weights = wi, data = fpe)
summary(lmFpe2) # check the difference between the estimates and their SE.

# lm returns the unweighted residuals
reslm <- fpe$A2 - fitted(lmFpe2)
residuals(lmFpe2) - reslm

# lm.influence returns the weighted residuals
lm.influence(lmFpe2)$wt.res - residuals(lmFpe2)*sqrt(wi)

# also the functions stdres (from MASS) and rstandard (from stats package) use the 
# weighted residuals (try: edit(lmwork) to check the code from stdres, 
# and look at the help file of rstandard)
stdres(lmFpe2) - rstandard(lmFpe2)

# normality of weighted residuals
qqnorm( residuals(lmFpe2) * sqrt(wi) )
qqnorm(stdres(lmFpe2)) # to be preferred

# check heteroscedasticity again
plot(fitted(lmFpe2), lmFpe2$resid)
plot(fpe$EI, abs(lmFpe2$resid))
plot(lmFpe2$resid * sqrt(wi))
plot(fpe$A2, lmFpe2$resid * sqrt(wi)) 
# somewhat better


