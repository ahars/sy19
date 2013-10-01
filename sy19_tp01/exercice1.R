# UV : SY19 - TP01
# 1. Exercice théorique
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)

x = matrix((8.5, 1.5, 3.5, 5.0, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5), 2)
x = t(x)

# Première partie : ACP
cent = scale(x, center = TRUE, scale = FALSE)
vari = (1/8) * t(cent) %*% cent
diag = eigen(vari)

# princomp(x)

a = sum(diag$values)
axe1 = diag$values[1] / a * 100
axe2 = diag$values[2] / a * 100


compx = princomp(x)
biplot(compx, main = "Représentation des individus dans le premier plan factoriel", ylab = "axe 2", xlab = "axe 1"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)

# 3) à finir
# il faut transformer la colonne en matrice avec un as.matrix
# pour pouvoir effectuer un calcul de matrices.
sumpca1 = diag$values[1]*t(compx$loadings[,1])
sumpca2 = diag$values[2]*t(compx$loadings[,2])


# Deuxième partie : MDS
w1 = x %*% t(x)

id8 = diag(8)
d = dist(x, method = "euclidean")
distance <- as.matrix(d)
un = matrix(rep(1, 64), 8)
q8 = id8 - (1/8) * un
w2 = -(1/2) * q8 %*% distance %*% q8

