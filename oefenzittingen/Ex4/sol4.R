##############################
#  SMDA: EXERCISE SESSION 4  #
##############################


rm(list = ls())



# EXERCISE 1 --------------------------------------------------------------------------------------

library(rrcov) # needed for function PcaClassic()


# 1.1 Read in Dblood.dat
Dblood <- read.table("Dblood.dat", header = TRUE, quote = "\"")
dim(Dblood)
str(Dblood)
summary(Dblood)
colMeans(Dblood)


# 1.2 Compute covariance and correlation matrices
apply(Dblood,2, sd)
Sigm <- cov(Dblood)
Rr <- cor(Dblood)


# 1.3 Compute loading matrices by applying PCA on (a) Sigm and (b) Rr
outCov <- PcaClassic(Dblood, scale = FALSE) # non-scaled PCA on covariance matrix Sigm
outCor <- PcaClassic(Dblood, scale = TRUE)  # scaled PCA on correlation matrix Rr
str(outCov)

outCov@loadings[, 1]
outCor@loadings[, 1]

eigen(Sigm)$vector[, 1]
eigen(Rr)$vector[, 1]

outCov@eigenvalues
outCor@eigenvalues

eigen(Sigm)$values
eigen(Rr)$values

outCov@loadings[, 1] * sqrt(outCov@eigenvalues[1]) / sqrt(diag(Sigm))
outCor@loadings[, 1] * sqrt(outCor@eigenvalues[1])

# The difference in the first loading vector PC1 is that for the non-scaled PCA almost all
# information comes from the single variable 'plate', while for the scaled PCA the information
# comes rom several variables. This is caused by the large variance in 'plate'.
# Therefore, we choose for PCA based on the correlation matrix.


# 1.4 Best PCA
getCenter(outCor)
summary(outCor) # Cumulative Proportion > 80%, 90%
cumsum(outCor@eigenvalues)/sum(outCor@eigenvalues)

screeplot(outCor, type = "lines")
# screeplot(outCov, type = "lines")

plot(outCor)
biplot(outCor)
# biplot(outCov)



# EXERCISE 2 --------------------------------------------------------------------------------------

# 2.1 Load headsize.dat
headsize <- read.table("headsize.dat", header = TRUE, quote = "\"")


# 2.2 Heads
heads <- cbind(headsize$head1, headsize$head2)
class(heads)
dim(heads)


# 2.3 Cor or Cov?
# Both variables are in the same scale/unit (cm), so we don't want to scale the data
# such that we can keep interpreting the data in cm.


# 2.4 PCA
load_heads <- PcaClassic(heads, scale = FALSE)


# 2.5 Equation of PC's: slope-center representation of first two eigenvectors.
headsMeans <- colMeans(heads)
headsLoadings <- load_heads@loadings

slope1 <- headsLoadings[2, 1] / headsLoadings[1, 1]
slope2 <- headsLoadings[2, 2] / headsLoadings[1, 2]

intercept1 <- headsMeans[2] - (slope1 * headsMeans[1]) 
intercept2 <- headsMeans[2] - (slope2 * headsMeans[1]) 


# 2.6 Plot
plot(heads, asp = 1, xlim = c(150, 220), ylim = c(150, 220), xlab = "heads1", ylab = "heads2")
abline(intercept1, slope1, lwd = 2)
abline(intercept2, slope2, lty = 2, lwd = 2)
legend("top", c("PC1", "PC2"), lty = c(1, 2), lwd = 2)


# 2.7 Tolerance ellipse 97.5%
library(car) # needed for function ellipse()

X <- heads
n <- nrow(X)
p <- ncol(X)
alpha <- 0.975
r <- sqrt(qchisq(0.975, df = 2))
ellipse(center = colMeans(X), shape = var(X), radius = r)

# with a slightly smaller radius
r <- sqrt(qchisq(0.9, df = 2))
ellipse(center = colMeans(X), shape = var(X), radius = r)
# the PC are the major and minor axes of the ellipse
abline(lsfit(x = heads[, 1], y = heads[, 2])$coefficients, col = "orange", lwd = 2)
legend("top", c("PC1", "PC2", "LS"), lty = c(1, 2, 1), lwd = 2, col = c("black", "black", "orange"))


# 2.8 Correlation between PC and original variables

# correlation between first PC and X1:
S <- cov(heads)
headsLoadings[1, 1] * sqrt(load_heads@eigenvalues[1]) / sqrt(S[1, 1])
cor(load_heads@scores[, 1], X[, 1])

# correlation between second PC and X1:
headsLoadings[1, 2] * sqrt(load_heads@eigenvalues[2]) / sqrt(S[1, 1])
cor(load_heads@scores[, 2], X[, 1])

# correlation between first PC and X2:
headsLoadings[2, 1] * sqrt(load_heads@eigenvalues[1]) / sqrt(S[2, 2])
cor(load_heads@scores[, 1], X[, 2])

# correlation between second PC and X2:
headsLoadings[2, 2] * sqrt(load_heads@eigenvalues[2]) / sqrt(S[2, 2])
cor(load_heads@scores[, 2], X[, 2])



# EXERCISE 3 --------------------------------------------------------------------------------------

# 3.1 Load heptathlon
heptathlon <- read.table("heptathlon.dat", header = TRUE, quote = "\"")
head(heptathlon)

hept <- heptathlon[, 1:7] # = heptathlon[, -8]
str(hept)
print(hept)
pairs(hept)


# 3.2 Cor or Cov?
apply(heptathlon, 2, sd) #PCA on cov because the variables have different units (corr? JEF)
load_hept <- PcaClassic(hept, scale = TRUE)


# 3.3 How many components?
summary(load_hept) # 3 PCS's provide > 80% of variance
screeplot(load_hept, type = "lines")
load_hept_3 <- PcaClassic(hept, k = 3, scale = TRUE)


# 3.4 Interpret first PC
load_hept_3@loadings[, 1]
load_hept_3@loadings[, 1] * sqrt(load_hept_3@eigenvalues[1])


# 3.5 Scatterplot
pairs(load_hept_3@scores)


# 3.6 Plot 1st principal score vs. score variable from data
plot(load_hept_3@scores[, 1], heptathlon$score, ylab = "score", xlab = "1st principal score")
abline(lm(heptathlon$score ~ load_hept_3@scores[, 1]), col = "red")

# Strong correlation between PC1 and end score. This means that we can "predict" the end score
# based on the first principal component.
# The least squares regression line shows a linear dependence between PC1 and the 'score' variable.


# 3.7 Plot the orthogonal distances vs the score distances
plot(load_hept_3)
plot(load_hept)
print(heptathlon)


