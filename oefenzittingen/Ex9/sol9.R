###############################
#  SMDA: EXERCISE SESSION 9  #
###############################


rm(list = ls())



# EXERCISE 1 --------------------------------------------------------------------------------------
library(MASS)

X <- read.table("titanic.txt", header = TRUE)
str(X)
summary(X)

# We could also declare Pclass as a categorical variable
# which would lead to 2 binary regressors.

VIF <- diag(solve(cor(X[, -c(1, 3)])))
round(VIF, 2) # small, hence no multicollinearity.


## QUESTION 1.1
fullmod <- glm(Surv ~ ., data = X, family = "binomial") # full model
summary(fullmod)


## QUESTION 1.2
redmod <- stepAIC(fullmod, list(lower = ~ 1, upper = ~ .), direction = "both")


## QUESTION 1.3
summary(redmod)

# Each of the regressors has a negative sign, so survival probability
# was lowered by being male, increasing age etc.

# Residual deviance: 636.72  on 709  degrees of freedom
qchisq(0.95, 709) # 772.05 > 636.72 => do not reject this model

# Deviance of fit: 964.52 - 636.72 = 327.8 on 4 degrees of freedom
# So this fit explains a lot more than a constant probability.


## QUESTION 1.4
redmod.cint <- summary(redmod)$coef[-1, ]
redmod.cint
redmod.ci <- cbind(redmod.cint[, 1] - 1.96 * redmod.cint[, 2],
                   redmod.cint[, 1] + 1.96 * redmod.cint[, 2])
redmod.ci
redmod.or <- exp(redmod.cint[, 1])
redmod.cior <- exp(redmod.ci)
cbind(redmod.or, redmod.cior)

# The OR are all below 1, as all these variables decrease
# the probability of survival...


## QUESTION 1.5
# ?predict
fitted.link <- predict(redmod, newdata = X, type = "link")
summary(fitted.link)

fitted.probs <- predict(redmod, newdata = X, type = "response") # = fitted(redmod)
summary(fitted.probs)

plot(fitted.link, X$Surv, xlab = "fitted eta", ylab = "survival",
     main = "logistic regression fit to Titanic data")
points(fitted.link, fitted.probs, type = "p", col = "blue")

survivors.index <- which(X$Surv == 1)
plot(fitted.probs, col = "blue")
points(survivors.index, fitted.probs[survivors.index], col = "red", pch = 19)


## QUESTION 1.6
plot(residuals(redmod, "deviance"))
# or:
devResid <- sign(X$Surv - fitted(redmod)) * sqrt(-2 * (X$Surv * log(fitted(redmod)) + (1 - X$Surv) * log(1 - fitted(redmod)) ))
sum(devResid^2)
#  636.7193
plot(devResid, ylab = "Deviance Residuals")



## QUESTION 1.7
table(X$Surv)
prop.table(table(X$Surv))

Surv <- X$Surv
cutoff <- mean(Surv)
cutoff
# 0.4061625

pred.Surv <- ifelse(fitted.probs > cutoff, 1, 0)
table(Surv, pred.Surv)
ptable <- prop.table(table(Surv, pred.Surv))
# prop.table normalizes to a total of 1
ptable

APER <- 1 - sum(diag(ptable))
APER
# 0.210084



# EXERCISE 2 --------------------------------------------------------------------------------------
library(klaR)

abTrain <- read.table("abalone1.txt", header = TRUE)
str(abTrain)
summary(abTrain)

abTest <- read.table("abalone2.txt", header = TRUE)
str(abTest)
summary(abTest)

plot(abTrain[, -1], col = ifelse(abTrain$Gender == "F", "red", "blue"))


## QUESTION 2.1
LDAfit <- lda(Gender ~ ., data = abTrain, CV = FALSE)
LDAfit



## QUESTION 2.2
# takes a lot of time:
# partimat(Gender ~ ., data = abTrain, method = "lda", plot.matrix = TRUE)


## QUESTION 2.3
LDApred <- predict(LDAfit, newdata = abTrain)
gender <- abTrain$Gender
pred   <- LDApred$class

LDAtable <- table(gender, pred)
LDAtable
round(prop.table(LDAtable), 2)
1 - sum(diag(prop.table(LDAtable))) # APER
# 0.1826667


LDAfitCV <- lda(Gender ~ ., data = abTrain, CV = TRUE)
LDAcvTable <- table(gender, LDAfitCV$class)
LDAcvTable
1 - sum(diag(prop.table(LDAcvTable))) # LOO
# 0.1833333 


LDApredTest <- predict(LDAfit,newdata = abTest)
genderT <- abTest$Gender
predT <- LDApredTest$class
LDAtestTable <- table(genderT,predT)
LDAtestTable
1 - sum(diag(prop.table(LDAtestTable))) # Misclassification error of test data
# 0.1940299

# The three misclassification rates are similar for LDA.


## QUESTION 2.4
QDAfit <- qda(Gender ~ ., data = abTrain, CV = FALSE)
QDApred <- predict(QDAfit, newdata = abTrain)
QDAtable <- table(abTrain$Gender, QDApred$class)
QDAtable
1 - sum(diag(prop.table(QDAtable))) # APER
# 0.174 

# takes a lot of time:
# partimat(Gender ~ ., data = abTrain, method = "qda", plot.matrix = TRUE)

QDAfitCV <- qda(Gender ~ ., data = abTrain, CV = TRUE)
QDAcvtable <- table(gender, QDAfitCV$class)
QDAcvtable
1 - sum(diag(prop.table(QDAcvtable))) # LOO
# 0.1753333

QDApredTest <- predict(LDAfit, newdata = abTest)
QDAtestTable <- table(genderT, QDApredTest$class)
QDAtestTable
1 - sum(diag(prop.table(QDAtestTable))) # Error rate on test data
# 0.1940299

# The three misclassification rates are similar for QDA.

# Conclusion: results of LDA and QDA are very similar here.



# EXERCISE 3 --------------------------------------------------------------------------------------

library(class)
## using knn from package class

## QUESTION 3.1
# Remarks in course: k = 1 and k = n

# What if k = 1 ?
# ?knn
outknn.1 <- knn(train = abTrain[, -1], test = abTrain[, -1], cl = abTrain[, 1], k = 1)
length(outknn.1)
# 1500
gender <- abTrain$Gender
table(gender, outknn.1) # looks `perfect' due to cheat!


# What if k = n ?
#
# crashes with k = n = 1500 !!!
outknn.n <- knn(train = abTrain[1:95, -1], test = abTrain[1:95, -1], cl = abTrain[1:95, 1], k = 95) 
length(outknn.n)
# 95
gendr <- gender[1:95]
sum(gendr == "F") # 49 # has the majority
table(gendr, outknn.n) # every `test' case was allocated to largest group! 


## QUESTION 3.2
EAER <- rep(0, 19) # initialize with zeroes
ks <- rep(0, 19)   # initialize with zeroes
for (i in 1:19) { # so k ranges from 2 to 20
  outknncv <- knn.cv(train = abTrain[, -1], abTrain[, 1], k = i+1) # LOO
  outtable <- table(abTrain[, 1], outknncv)
  EAER[i] <- (outtable[1, 2] + outtable[2, 1]) / nrow(abTrain)
  # misclassification rate for this k
  ks[i] <- i + 1
}
plot(ks, EAER) # lowest LOO-EAER at k = 7

ks[which.min(EAER)]
# 7 # LOO-CV selects this value of k
min(EAER)
# 0.162


## QUESTION 3.3
# kNN with k = 7: result for test data
outknn <- knn(train = abTrain[, -1], test = abTest[, -1], abTrain[, 1], k = 7)
outtable <- table(abTest[, 1], outknn)
outtable

(outtable[1, 2] + outtable[2, 1]) / nrow(abTest)
# 0.192673
# Test set performance is similar to LOO-CV which gave 0.162


## QUESTION 3.4
# From the partimat plots it appeared that the explanatory variables
# are highly correlated. This can be confirmed by PCA:

library(rrcov)
load <- PcaClassic(abTrain[2:8], scale = TRUE)
round(load@loadings[, 1], 3)
summary(load)

screeplot(load, type = "lines") # PC1 dominates totally

# So, in spite of the 7 dimensions, the regressor is basically
# univariate. This exlain why there is so little difference 
# between LDA, QDA, kNN here.
# (Redoing the computation on a small number of PCA scores instead
# of the original regressors makes little difference either.)

