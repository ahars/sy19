# UV : SY19 - TP00
# 2. Mélanges de lois normales
# 1) Génération d'une classe suivant un mélange de lois normales
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)

n = 1000
pi1 = 1 / 2
pi2 = 1 / 4

mu1 = c(-3, 8)
mu2 = c(-5, 10)
mu3 = c(-1, 10)

landa1 = 2
landa2 = 1

teta1 = -pi / 3
teta2 = -pi / 6
teta3 = pi / 6

D1 = matrix(c(cos(teta1), sin(teta1), -sin(teta1), cos(teta1)), 2)
D2 = matrix(c(cos(teta2), sin(teta2), -sin(teta2), cos(teta2)), 2)
D3 = matrix(c(cos(teta3), sin(teta3), -sin(teta3), cos(teta3)), 2)

# Transposée des matrices D.
D1T = t(D1)
D2T = t(D2)
D3T = t(D3)

a = 1.5
A = matrix(c(a, 0, 0, 1 / a), 2)

# Calcul des Sigma.
# MAT1 %*% MAT2 effectue une vraie multiplication entre matrices.
# MAT1 * MAT2 effectue une multiplication de matrice terme à terme.
sigma1 = landa1 * D1T %*% A %*% D1
sigma2 = landa2 * D2T %*% A %*% D2
sigma3 = landa2 * D3T %*% A %*% D3

# Fonction de génération d'un échantillon de données suivant un mélange de lois normales.
melangeLoisNormales1 <- function (n, pi1, pi2, pi3, mu1, mu2, mu3, sigma1, sigma2, sigma3) {

	result = matrix(nrow = n, ncol = 3)

	# Répartition des éléments de l'échantillon entre les 3 lois normales (à améliorer pour rendre la fonction plus générique).
	for (k in 1:n) {
		rand = sample(0:3, 1)
		if (rand == 1 || rand == 0) {
			result[k, 3] = 0
		} else {
			if (rand == 2) {
				result[k, 3] = 1
			} else {
				result[k, 3] = 2
			}
		}

		# Génération de l'échantillon suivant les 3 lois normales
		if (result[k, 3] == 0) {
			result[k, c(1, 2)] = mvrnorm(1, mu1, sigma1)
		} else {
			if (result[k, 3] == 1) {
				result[k, c(1, 2)] = mvrnorm(1, mu2, sigma2)
			} else {
				result[k, c(1, 2)] = mvrnorm(1, mu3, sigma3)
			}
		}
	}
	return (result)
}

ech = melangeLoisNormales1(n, mu1, mu2, mu3, sigma1, sigma2, sigma3)

# Courbes de niveau de sa fonction de densité.


# Calcul de la moyenne empirique sur cet échantillon.
moy = c(0, 0)
moy[1] = mean(ech[1])
moy[2] = mean(ech[2])

# Calcul de la matrice de covariance empirique sur cet échantillon.
covar = cov(ech)

