# UV : SY19 - TP02
# Exercice 2
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)
library(cluster)

source("mixtmono.R")

pi = 1 / 2
mu <- matrix(c(0, 6), nrow = 2, ncol = 1)
sigma2 <- matrix(c(1, 5), nrow = 2, ncol = 1)

# 2
x = c(rnorm(1000), rnorm(1000, mean = 6, sd = 5))

# Centrage et réduction des données
sc = scale(x)

r1 = gmixtmono(x, pi, NULL, NULL, FALSE)
#r1$param

#r2 = gmixtmono(sc, pi, NULL, NULL, FALSE)
#r2$param

c1 = gmixtmono(x, pi, NULL, NULL, TRUE)
#c1$param

#c2 = gmixtmono(sc, pi, NULL, NULL, TRUE)
#c2$param

# 4
#d = matrix(x, ncol = 2)
#k2 =  kmeans(d, 2, nstart = 20)
#clusplot(d, k2$cluster, color = TRUE, shade = TRUE)

