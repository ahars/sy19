# UV : SY19 - TP02
# Exercice 3
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice3.R

library(MASS)
library(cluster)

source("mixtmult.R")

pi = c(0.35, 0.25, 0.4)

mu1 = c(3, 1)
mu2 = c(-1, 2)
mu3 = c(1, -3)

sigma1 = diag(c(1, 1))
sigma2 = diag(c(2, 1))
sigma3 = diag(c(1, 2))

prop <- rmultinom(1, 3000, pi)
X <- rbind(mvrnorm(prop[1], mu1, sigma1), mvrnorm(prop[2], mu2, sigma2), mvrnorm(prop[3], mu3, sigma3))

