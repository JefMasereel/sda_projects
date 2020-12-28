##############################
#  SMDA: EXERCISE SESSION 3  #
##############################


rm(list = ls())



# EXERCISE 1 --------------------------------------------------------------------------------------

par(mfrow = c(1, 3))
x1 <- rnorm(n = 20,  mean = 2, sd = 3)
x2 <- rnorm(n = 100, mean = 2, sd = 3)
x3 <- rnorm(n = 500, mean = 2, sd = 3)
qqnorm(x1, main = paste0("Normal Q-Q Plot (n = ", length(x1), ")"))
qqline(x1)
qqnorm(x2, main = paste0("Normal Q-Q Plot (n = ", length(x2), ")"))
qqline(x2)
qqnorm(x3, main = paste0("Normal Q-Q Plot (n = ", length(x3), ")"))
qqline(x3)

# OR:

for (n in c(20, 100, 500)) {
  x <- rnorm(n, mean = 2, sd = 3)
  qqnorm(x, main = paste0("Normal Q-Q Plot (n = ", n, ")"))
  qqline(x)
}

shapiro.test(x)



# EXERCISE 2 --------------------------------------------------------------------------------------

library(MASS) # needed for function mvrnorm()

for (n in c(20, 100, 500)) {
  p <- 4
  mu <- rep(0, p)
  Sigma <- diag(p)
  X <- mvrnorm(n, mu, Sigma)
  y <- mahalanobis(X, mu, Sigma)
  # OR:
  # y <- mahalanobis(X, colMeans(X), var(X))
  probs <- ppoints(n) # = probabilities
  qqplot(qchisq(probs, df = p), y)
  qqline(y, distribution = function(pp) {qchisq(pp, df = p)}, col = "red", lwd = 2)
  abline(h = qchisq(0.975, df = p), col = "gray", lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))



# EXERCISE 3 --------------------------------------------------------------------------------------

library(car) # needed for functions powerTransform() and bcPower()
library(MASS) # needed for function boxcox()

ces <- read.csv("ces.csv")

X1 <- ces$FDHO
X2 <- ces$FDAW
wantedRows <- which(X1 > 0 & X2 > 0)
X <- cbind(X1, X2)[wantedRows, ] # X = FDHOFDAW

# OR:
# library(readr)
# library(dplyr)
# ces <- read_csv("ces.csv")
# ces <- as.data.frame(ces)
# X <- ces %>% select(FDHO, FDAW) %>% filter(FDHO > 0 & FDAW > 0)

print(dim(X))
n <- nrow(X)
p <- ncol(X)

X1 <- X[, 1] # = FDHO (> 0)
X2 <- X[, 2] # = FDAW (> 0)
plot(X, xlab = "X1 (FDHO)", ylab = "X2 (FDAW)")



# For each variable, investigate the (univariate) normality:
hist(X1)
plot(density(X1))
shapiro.test(X1)
qqnorm(X1)
qqline(X1)

hist(X2)
plot(density(X2))
shapiro.test(X2)
qqnorm(X2)
qqline(X2)



# For each variable, determine the optimal Box-Cox transformation
# using powerTransform() and bcPower():
lambdaX1 <- powerTransform(X1 ~ 1)
lambdaX1 # = 0.2472131
summary(lambdaX1)

lambdaX2 <- powerTransform(X2 ~ 1)
lambdaX2 # = 0.2083138
summary(lambdaX2)

bcX1 <- bcPower(X1, coef(lambdaX1))
bcX2 <- bcPower(X2, coef(lambdaX2))
bc_X <- cbind(bcX1, bcX2)



# For each transformed variable, check the normality:
hist(bcX1)
plot(density(bcX1))
shapiro.test(bcX1)
qqnorm(bcX1)
qqline(bcX1)

hist(bcX2)
plot(density(bcX2))
shapiro.test(bcX2)
qqnorm(bcX2)
qqline(bcX2)



# For each variable, determine the optimal Box-Cox transformation
# using boxcox():
bc2X1 <- boxCox(X1 ~ 1) # also gives the log-likelihood plot
bc2X1
lambdaX1_2 <- bc2X1$x[which.max(bc2X1$y)]
lambdaX1_2

bc2X2 <- boxCox(X2 ~ 1)
lambdaX2_2 <- bc2X1$x[which.max(bc2X2$y)]
lambdaX2_2



# Not the same as powerTransform(), use a finder grid
bcs_X1 <- boxCox(X1 ~ 1, lambda = seq(0, 0.5, 0.01))
bcs_X1$x[which.max(bcs_X1$y)]



# Check bivariate normality of the original variables:
pairs(X)
MDs <- mahalanobis(X, colMeans(X), var(X))
qqplot(qchisq(ppoints(n), df = p), MDs) # ppoints(n) are probabilities
qqline(MDs, distribution = function(pp) {qchisq(pp, df = p)}, col = "red", lwd = 2)
abline(h = qchisq(0.975, df = 2), col = "gray", lty = 2, lwd = 2)



# Check bivariate normality of the transformed variables:
pairs(bc_X)
MDs_t <- mahalanobis(bc_X, colMeans(bc_X), var(bc_X))
qqplot(qchisq(ppoints(n), df = p), MDs_t) 
qqline(MDs_t, distribution = function(pp) {qchisq(pp, df = p)}, col = "red", lwd = 2)
abline(h = qchisq(0.975, df = 2), col = "gray", lty = 2, lwd = 2)



# EXERCISE 4 --------------------------------------------------------------------------------------

library(MASS) # needed for dataset hills
library(robustbase) # needed for function covMcd()

dim(hills)
head(hills)

plot(hills[, 1:2], xlim = c(-10, 25), ylim = c(-2000, 8000))

mcd <- covMcd(hills[, 1:2], alpha = 0.5)

rd <- sqrt(qchisq(0.95, df = 2))
car::ellipse(center = colMeans(hills[,1:2]), shape = cov(hills[,1:2]), radius = rd, col = "red")
car::ellipse(center = mcd$center,            shape = mcd$cov,          radius = rd, col = "blue")

plot(mcd, which = "dd")
plot(mcd)



# EXERCISE 5 --------------------------------------------------------------------------------------

library(stats4) # needed for function mle()

n <- 100



# (a) MLE Poisson distribution ----------------------------------
set.seed(2019)
x1 <- rexp(n, rate = 4)

negloglikelihood_exp <- function(lambda) {
  densities <- dexp(x1, lambda)
  negative_log_likelihood <- -sum(log(densities))
  return(negative_log_likelihood)
}

(MLE <- mle(negloglikelihood_exp, start = list(lambda = 1)))
MLE@coef
1/mean(x1)



# (b) MLE normal distribution -----------------------------------
set.seed(2019)
x2 <- rnorm(n, mean = 3, sd = 2)

negloglikelihood_norm <- function(mu, sigma) {
  densities <- dnorm(x2, mu, sigma)
  negative_log_likelihood <- -sum(log(densities))
  return(negative_log_likelihood)
}

mle(negloglikelihood_norm, start = list(mu = 1, sigma = 1))
# NaN's are produced when negative values are attempted for the standard deviation.

mean(x2)
sqrt(1/n * sum((x2 - mean(x2))^2))
sqrt((n-1) / n) * sd(x2)
# sd(x2) # = sqrt(1/(n-1)*sum((x2 - mean(x2))^2))



# (c) MLE multivariate normal distribution ----------------------
library(mvtnorm) # needed for functions rmvnorm() and rmvnorm()
set.seed(2019)
x3 <- rmvnorm(n, mean = c(3, 4), sigma = matrix(c(2, -0.5, -0.5, 1), ncol = 2))

negloglikelihood_multinorm <- function(mu1, mu2, s11, s22, s12) {
  densities <- dmvnorm(x3, mean = c(mu1, mu2), sigma = matrix(c(s11, s12, s12, s22), ncol = 2))
  negative_log_likelihood <- -sum(log(densities))
  return(negative_log_likelihood)
}

mle(negloglikelihood_multinorm, start = list(mu1 = 2, mu2 = 3, s11 = 1, s22 = 0.5, s12 = -0.3))

mean(x3[, 1])
mean(x3[, 2])
(n - 1) / n * cov(x3)


