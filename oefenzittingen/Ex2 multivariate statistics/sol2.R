##############################
#  SMDA: EXERCISE SESSION 2  #
##############################


rm(list = ls())



# EXERCISE 1 --------------------------------------------------------------------------------------

library(MASS) # needed for function mvrnorm()

s12 <- 0 * sqrt(5 * 2)
Sigma_matrix <- matrix(c(5, s12, s12, 2), nrow = 2)

X <- mvrnorm(n = 100, mu = c(2, 3), Sigma = Sigma_matrix)

plot(X, main = paste("r_12 =", s12/sqrt(5 * 2)))

par(mfrow = c(2, 2))

for (s12 in c(0, 0.3, 0.6, 0.9)*sqrt(5*2) ) {
  Sigma_matrix <- matrix(c(5, s12, s12, 2), nrow = 2)
  X <- mvrnorm(n = 100, mu = c(2, 3), Sigma = Sigma_matrix)
  plot(X, main = paste("r_12 =", s12/sqrt(5*2)))
}


set.seed(2019)

r12 <- 0.6
s12 <- r12 * sqrt(5*2)
Sigma <- matrix(c(5, s12, s12, 2), ncol=2)
X <- mvrnorm(n = 100, mu = c(2, 3), Sigma = Sigma)
X



# EXERCISE 2 --------------------------------------------------------------------------------------

colMeans(X) # or: apply(X, 2, mean)
cov(X)

Z <- scale(X, center = colMeans(X), scale = TRUE)
# Z <- scale(X, center = colMeans(X), scale = apply(X, 2 ,sd) )
# Z <- scale(X, center = colMeans(X), scale = sqrt(diag(cov(X))) )

par(mfrow = c(1, 2))
plot(X, main = "X")
plot(Z, main = "Z")

colMeans(Z)
cov(Z)
cor(X)



# EXERCISE 3 --------------------------------------------------------------------------------------

set.seed(2019)

A <- matrix(c(3, 5, -2, 1), ncol = 2)
A
d <- rnorm(2)
d
Y <- t( t(X%*%A) + d )
# Y <- sweep(X%*%A, 2, d, FUN = "+")
Y

colMeans(Y)
colMeans(X)%*%A + d

cov(Y)
t(A) %*% cov(X) %*% A



# EXERCISE 4 --------------------------------------------------------------------------------------

mahalanobis(Y, center = colMeans(Y), cov = cov(Y))
mahalanobis(X, center = colMeans(X), cov = cov(X))
plot(mahalanobis(Y, center = colMeans(Y), cov = cov(Y)), ylab = "MD(Y)", main = "MD(Y)")
plot(mahalanobis(X, center = colMeans(X), cov = cov(X)), ylab = "MD(X)", main = "MD(X)")



# EXERCISE 5 (b) ----------------------------------------------------------------------------------

library(ellipse) # needed for function ellipse()

Xstand <- scale(X, center = TRUE, scale = TRUE)

e <- eigen(cov(X))
V <- e$vectors
V %*% diag(e$values) %*% t(V)
cov(X)

Sroot <- V %*% diag(e$values^-0.5) %*% t(V)
Sroot %*% Sroot
solve(cov(X))

Xsphered <- X %*% Sroot

par(mfrow = c(2, 2))
par(pty = "s")  # squared figure

plot(X, xlim = c(-6, 10), ylim=c(-6, 10), col = "red", main = "original data")
lines(ellipse(x = Sigma,  centre = c(2, 3),      level = 0.95, t = sqrt(qchisq(0.95, 2)), npoints = 1000), col = "green")
lines(ellipse(x = cov(X), centre = colMeans(X),  level = 0.95, t = sqrt(qchisq(0.95, 2)), npoints = 1000))

plot(Xstand, xlim = c(-6, 10), ylim = c(-6, 10), col = "darkgreen", main = "standardized data")
lines(ellipse(x = cov2cor(Sigma),  centre = c(0, 0),           level = 0.95, t = sqrt(qchisq(0.95, 2)), npoints = 1000), col = "green")
lines(ellipse(x = cov(Xstand),     centre = colMeans(Xstand),  level = 0.95, t = sqrt(qchisq(0.95, 2)), npoints = 1000))

plot(Xsphered, xlim = c(-6, 10), ylim = c(-6, 10), col = "blue", main = "sphered data")
lines(ellipse(x = Sroot %*% Sigma %*% t(Sroot), centre = c(2,3) %*% Sroot,   npoints = 1000), col = "green")
lines(ellipse(x = cov(Xsphered),                centre = colMeans(Xsphered), npoints = 1000))

# other axes:
plot(Xsphered, xlim = c(-3, 6), ylim = c(-3,6), col = "blue", main = "gesfeerde data")
lines(ellipse(x = Sroot %*% Sigma %*% t(Sroot), centre = c(2,3)%*% Sroot,    npoints = 1000), col="green")
lines(ellipse(x = cov(Xsphered),                centre = colMeans(Xsphered), npoints = 1000))

par(mfrow = c(1,1))
par(pty = "m")


