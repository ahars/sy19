# UV : SY19 - TP00
# 1. Loi normale bidimensionnelle
# 2) Génération d'un échantillon de réalisations d'une variable multinormale
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)

n = 1000
mu = c(1, 2)
sigma = matrix(c(2, 1, 1, 2), 2)

# Génération de l'échantillon de n réalisations d'une variable normale.
ech = mvrnorm(n, mu, sigma)

# Calcul de la moyenne empirique sur cet échantillon.
moy = c(0, 0)
moy[1] = mean(ech[1])
moy[2] = mean(ech[2])

# Calcul de la matrice de covariance empirique sur cet échantillon.
covar = cov(ech)

