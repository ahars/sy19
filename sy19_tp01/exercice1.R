# UV : SY19 - TP01
# 1. Exercice théorique
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)

x = matrix(c(8.5, 1.5, 3.5, 5.0, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5), 2)
x = t(x)

# Première partie : ACP
cent = scale(x, center = TRUE, scale = FALSE)
vari = (1/8) * t(cent) %*% cent
diag = eigen(vari)

a = sum(diag$values)
axe1 = diag$values[1] / a * 100
axe2 = diag$values[2] / a * 100

# ACP
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
# 1)
d = dist(x, method = "euclidean")

# 2)
w1 = cent %*% t(cent)

id8 = diag(8)
distance <- as.matrix(d)
un = matrix(rep(1, 64), 8)
q8 = id8 - (1/8) * un
w2 = -(1/2) * q8 %*% distance^2 %*% q8

# 3)
n = 1 / 8
cent = scale(n * w1, center = TRUE, scale = FALSE)
vari = (1/8) * t(cent) %*% cent
diag = eigen(vari)
# Il faut montrer que les valeurs propres sont positives ou nulles.


# 4)
dp = diag$vectors
l = diag$values * id8

# 5)
aftd = cmdscale(d, 7)

# 6)
plot(aftd$points)
# Faire la comparaison

# 7)

aftd <- function (d) {

#	k =  dim(as.matrix(d))[1] - 1
#	result = cmdscale(d, k)
	return (result)
}

