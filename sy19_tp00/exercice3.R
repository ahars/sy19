# UV : SY19 - TP00
# 2. Mélanges de lois normales
# 2) Variation des proportions du mélange
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice3.R

library(MASS)

n = 1000

pi1 = 0.6
pi2 = 0.2
pi3 = 0.8
pi4 = 0.1
pi5 = 0.98
pi6 = 0.01

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

