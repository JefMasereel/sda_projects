##############################
#  SMDA: EXERCISE SESSION 6  #
##############################


rm(list = ls())



# EXERCISE 1 --------------------------------------------------------------------------------------

library(rrcov)
library(MASS)

riverfish <- read.table("riverfish.txt", header = TRUE, quote = "\"")
str(riverfish)
head(riverfish)

riverfish$Site <- as.factor(riverfish$Site)
riverfish$Site
str(riverfish)
summary(riverfish)


## 1.1 Exploratory data analysis
boxplot(riverfish$Length,riverfish$Breadth, names = c("Length", "Breadth"))
plot(riverfish$Length, riverfish$Breadth)
plot(riverfish$Length, riverfish$Breadth,
     col = ifelse(riverfish$Site == 1, "red", "blue"),
     pch = ifelse(riverfish$Site == 1, 1, 19))


## 1.2 Regression analysis
fishlm <- lm(Breadth ~ ., data = riverfish) # . means include all variables
model.matrix(fishlm)
# R has created the variable Site2 which is equal to 0 if Site = 1, and equal to 1 if Site = 2

fishsum <- summary(fishlm)
fishsum
# breath = -1.90498 + 0.97941 * Length - 0.45422 * Site2 = -1.90498 + 0.97941*Length - 0.45422 * (Site-1)
# thus breath = -1.45076 + 0.97941*Length - 0.45422*Site
# residual standard error = 1.295


## 1.3
## Residual plots
e <- fishlm$residuals # = riverfish$Breadth - fitted(fishlm)
plot(e, xlab = "index", ylab = "Residual")                      #residual vs index
plot(fitted(fishlm), e, xlab = "fitted", ylab = "Residual")     #residual vs fitted
plot(riverfish$Length, e, ylab = "Residuals", xlab = "Length")  #residual vs independent var
abline(h = 0, lty = 2)   

## test normality of standardized residuals
es <- stdres(fishlm)

# Extra: check standardized residuals
# fishinf <- lm.influence(fishlm)
# h <- fishinf$hat #diagonal of the 'hat' matrix
# es - e/(sqrt(sum(e^2)/(46-3))*(1-h)^.5)

qqnorm(es)
qqline(es)
shapiro.test(es) # p-value = 0.9649 H0: normally distributed

plot(es, xlab = "index", ylab = "standardized residual", ylim = c(-3, 3))  #index vs standardized residual
abline(h = -2.5, lty = 2)
abline(h =  2.5, lty = 2)

plot(fishlm) #provides some common lm plots


## 1.4 Points together with fitted lines
plot(riverfish$Length, riverfish$Breadth, col = ifelse(riverfish$Site == 1, "red", "blue"), pch = 19)
abline(coef(fishlm)[[1]], coef(fishlm)[[2]], col = "red")
abline(coef(fishlm)[[1]] + coef(fishlm)[[3]], coef(fishlm)[[2]], col = "blue")


## 1.5 PCA
fishPCA <- PcaClassic(riverfish[, 1:2])
summary(fishPCA)
intercept <- fishPCA@center[1] - fishPCA@center[2] * fishPCA@loadings[2, 1]/fishPCA@loadings[1, 1]
slope     <- fishPCA@loadings[2,  1] / fishPCA@loadings[1, 1]
abline(intercept, slope, col = "black")

plot(fishPCA@scores[,1], fishPCA@scores[,2], col=ifelse(riverfish$Site == 1, "red", "blue"), pch = 19)
abline(v = 0, lty = 2)

## 1.6 Appropriate?
# There is clearly a linear relation between Breadth and Length, which justifies the use of a linear model.
# But:
# 1. the 'Site' variable is not significant (see also the p-value of Site2 in 'fishsum').
#    We may assume one global model for both types of fishes.
# 2. the PCA analysis clearly shows that the distinction between the two species is prominent in the first score. 
# 3. the variables Breadth and Length play the same role in this dataset (one could also try to predict 
#    Length from Breadth), so orthogonal regression is more appropriate.
#    This corresponds with the first black PCA loading. 



# EXERCISE 2 --------------------------------------------------------------------------------------

chicago <- read.table("chicago.txt", header = TRUE, quote = "\"")
str(chicago)
head(chicago)


## 2.1 Data analysis
boxplot(chicago[, 1:4])
boxplot(chicago[, 5], main = "involact")
boxplot(chicago[, 6], main = "income")
pairs(chicago)


# two outliers in theft and income, it would be best to remove those with the following code
outliers = which(chicago$theft > boxplot(chicago$theft)$stats[5, ])
chicago2 = chicago[-outliers, ]
pairs(chicago2)
# removing the outliers changes the whole analysis and makes most predictor variables not significant (try it!)
# just for illustrating the code we nevertheless continue with the full data set
chic2lm <- lm(involact ~ ., data = chicago2)
summary(chic2lm)


## 2.2 Model
chiclm <- lm(involact ~ ., data = chicago)
chicsum <- summary(chiclm)
chicsum


## 2.3 Appropriate plots
e <- residuals(chiclm)  
plot(e, xlab = "index", ylab = "residual") #index vs residual
plot(chiclm, which = 1)                    #residual vs fitted
plot(chiclm)

es <- stdres(chiclm)
# chicinf <- lm.influence(chiclm)
# h <- chicinf$hat        #diagonal of the 'hat' matrix
# e/(chicsum$sigma*(1-h)^.5) - es

qqnorm(es)
qqline(es)
shapiro.test(es)  

plot(es, xlab = "index", ylab = "standardized residual", ylim = c(-3, 3))  #index vs standardized residual
abline(h = -2.5, lty = 2)
abline(h =  2.5, lty = 2)


## 2.4 Fitted vs Observed response values
plot(chicago$involact, chiclm$fitted.values, xlab = "observed response values", ylab = "fitted values")
abline(0, 1)
cor(chicago$involact, chiclm$fitted.values)


## 2.5 Design matrix 
X <- model.matrix(chiclm)
X

# Correlation matrix of the predictor variables
X <- X[, -1]
# or: X <- chicago[, -5]
cor(X)    # multicollinearity is not a big issue


## 2.6 Is at least one of the regression slopes different from 0?
chicsum   # income is not significant (p-value much larger than 5%)


## 2.7 Refit without income
chiclm <- lm(involact ~ race + fire + theft + age, data = chicago)
chicsum <- summary(chiclm)
chicsum

e <- residuals(chiclm)  
es <- stdres(chiclm)
plot(e, xlab = "index", ylab = "residual")                #index vs residual
plot(chiclm, which = 1)                                #residual vs fitted
plot(es, xlab = "index", ylab = "standardized residual")  #index vs standardized residual
abline(h = -2.5, lty = 2)
abline(h =  2.5, lty = 2)

qqnorm(es)
qqline(es)
shapiro.test(es)  


## 2.8 95% interval voor beta_1
confint(chiclm, "race", 0.95)


## 2.9 anova tabel
chicanova <- anova(chiclm)
chicanova
1 - (chicanova[nrow(chicanova), "Mean Sq"] / var(chicago$involact))  #adjusted R2
1 - (chicanova[nrow(chicanova), "Sum Sq"] / (var(chicago$involact) * (nrow(chicago) - 1)) ) #R2
# identical to
# summary(chiclm)$r.squared
# summary(chiclm)$adj.r.squared


## 2.10 SSR(X3|X1,X2) and SSR(X1|X2,X3)
anova(chiclm)
# SSR(X3|X1,X2) = 0.8213
chiclm2 <- lm(involact ~ fire + theft + race + age, data = chicago)
anova(chiclm2)
# SSR(X1|X2,X3) = 0.6816


## 2.11 (beta_1,beta_2) = (0,0) ?

# partial F-test
reducedlm <- lm(involact ~ theft + age, data = chicago)
anova(chiclm, reducedlm) #reject H0 => (beta_1, beta_2) != (0,0)

# check!
chiclm3 <- lm(involact ~ theft + age + race + fire, data = chicago)
anova(chiclm3)
# F = 1/2 * ( SSR(X3|X1,X2) + SSR(X4|X1,X2,X3) / MSE(X1,X2,X3,X4)
# F = 1/2 * (4.7983 + 3.3860) / 0.0664 = 61.62877 # p-value << 0.05 => reject H0: (B1,B2) != (0,0)

summary(chiclm)

#                Estimate  Std. Error t value Pr(>|t|) 
#   race         0.005146   0.001562   3.295 0.002101 **    => < alpha/2 = 0.025: reject beta_1=0
#   fire         0.046917   0.006570   7.141 1.37e-08 ***   => < alpha/2 = 0.025: reject beta_2=0 
#   theft       -0.008108   0.002108  -3.846 0.000433 ***   
#   age          0.004876   0.001925   2.533 0.015441 *     

#same results/conclusion


## 2.12
x0 <- data.frame(race = 50, fire = 11, theft = 32, age = 60, income = 11000)
predict(chiclm, x0, interval = "confidence", level = 0.95)


## 2.13
x0 <- data.frame(race = 100, fire = 11, theft = 32, age = 60, income = 21000)
predict(chiclm, x0, interval = "prediction", level = 0.99)


## 2.14
cov2cor(vcov(chiclm))[2, 3]


## 2.15
chiclm4 <- lm(involact ~ race + fire + theft + age +
                race*fire + race*theft + race*age + fire*theft + fire*age + theft*age,
              data = chicago)
# easier: chiclm4 <- lm(involact ~ .^2, data = chicago[, -6])

summary(chiclm4)
anova(chiclm4, chiclm)
# all interaction terms jointly not significant

chiclm5 <- lm(involact ~ race + fire + theft + age +
                I(race^2) + I(fire^2) + I(theft^2) + I(age^2),
              data = chicago)
summary(chiclm5)
anova(chiclm5,chiclm)
# all quadratic terms jointly not significant



# EXERCISE 3 --------------------------------------------------------------------------------------

firm <- read.table("firm.txt", header = TRUE,sep = ",")
str(firm)
head(firm)

# not needed here: firm$Type <- as.factor(firm$Type)
firm$Type
levels(firm$Type)
firmlm <- lm(Months ~ ., data = firm)
summary(firmlm)
model.matrix(firmlm)
coef(firmlm)
# (Intercept)        Size   TypeStock 
# 33.8740690  -0.1017421   8.0554692 
# stock: E[Y] = (B0+B2) + B1 X1 = (33.874069 + 8.055469) - 0.101742 X1
# mutual: E[Y] = B0 + B1 X1 = 33.874069 - 0.101742 X1
plot(firm$Size, firm$Months, col = ifelse(firm$Type == "Mutual", "red", "blue"), pch = 19)
abline(coef(firmlm)[[1]], coef(firmlm)[[2]], col = "red")
abline(coef(firmlm)[[1]] + coef(firmlm)[[3]], coef(firmlm)[[2]], col = "blue")

firm$Type <- relevel(firm$Type, ref = "Stock")
firmlm2 <- lm(Months ~ ., data = firm)
model.matrix(firmlm2)
summary(firmlm2 )
# (Intercept)        Size  TypeMutual 
# 41.9295382  -0.1017421  -8.0554692 
# stock: E[Y] = B0 + B1 X1 = 41.9295382 - 0.1017421 X1
# mutual: E[Y] = (B0+B2) + B1 X1 = (41.9295382 -8 .0554692) -0.101742 X1
abline(coef(firmlm2)[[1]], coef(firmlm2)[[2]], col = "blue", lwd = 3, lty = 2)
abline(coef(firmlm2)[[1]] + coef(firmlm2)[[3]], coef(firmlm2)[[2]], col = "red", lwd = 3, lty = 2)


firmlm3 <- lm(Months ~ Size * Type, data = firm)
summary(firmlm3)
#The interaction term is not significant


