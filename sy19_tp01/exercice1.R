# UV : SY19 - TP01
# 1. Exercice théorique
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

# http://lotybo.free.fr/upload/File/SY09/SY09%20TP3%20Positionnement%20multidimensionnel.pdf

library(MASS)
x = matrix(c(8.5, 1.5, 3.5, 5.0, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5), 2)
x = t(x)

# Première partie : ACP
cent = scale(x, center = TRUE, scale = FALSE)
s = (1 / 8) * t(cent) %t*% cent
diag = eigen(s)

# Les axes factoriels
u1 = diag$vectors[,1]
u2 = diag$vectors[,2]

landa1 = diag$values[1]
landa2 = diag$values[2]

# princomp(x)

# Calcul des pourcentages d'inertie expliquée
a = sum(diag$values)
axe_inertie1 = diag$values[1] / a * 100
axe_inertie2 = diag$values[2] / a * 100

compx = princomp(x)

png(file = "plots/biplot_exo1_princomp.png")
biplot(compx, main = "Représentation des individus dans le premier plan factoriel", ylab = "axe 2", xlab = "axe 1")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

# 3)
m = diag(1, 2)
u = diag$vectors

# Matrice des composantes principales
C = x %*% m %*% u

# Calcul qui nous donne la matrice X
somme = C[,1] %*% t(u1) + C[,2] %*% t(u2)



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
#dp = diag$vectors
#l = diag$values * id8

# 5)
#aftd = cmdscale(d, 2)

# 6)
#plot(aftd$points)
# Faire la comparaison

# 7)

aftd <- function (d) {

	# Transformation de la matrice de distances en matrice
	dim_mat = as.matrix(d)
	d2 = dim_mat^2

	# Récupération de la première dimension de la matrice (nbre de lignes = nbre d'individus)
	dimension = diag(dim(dim_mat))[1]

	# Création de la matrice identité
	id = diag(dimension)

	# Création de la matrice unitaire
	un = matrix(rep(1, dimension^2), dimension)

	# Création de la matrice Q de centrage
	q = id - (1/dimension) * un

	# Calcul de la matrice W
	w = -(1/2) * q %*% d2 %*% q

	# cent = scale((1 / dimension) * w, center = TRUE, scale = FALSE)
	# vari = (1 / dimension) * t(cent) %*% cent
	# diag = eigen(vari)

	# Matrice associée aux vecteurs propres
	V = eigen(1 / dimension * w)$vectors[,1:7]
	V = sqrt(dimension) * V

	# Matrice associées aux valeurs propres
	L = diag(c(eigen(1 / dimension * W)$values[1:7]))

	# Calcul composante principale
	C = V %*% sqrt(L)

	png(file = "plots/plot_mutations_aftd.png")
	plot(C, main = "AFTD : Représentation de mutations sur les 2 premiers axes factoriels",xlab = "axe1", ylab = "axe2", type = "n")
	text(C[,1], C[,2], letters[1:dimension])
	dev.off()
	
	# Calcul du pourcentage d'inertie pour les 2 premières valeurs propres
	quality2 = (diag(L)[1] + diag(L)[2]) / sum(eigen(1 / dimension * W) $values) * 100

	# Calcul du pourcentage d'inertie pour les 7 premières valeurs propres
	quality7 = (diag(L)[1] + diag(L)[2] + diag(L)[3] + diag(L)[4] + diag(L)[5] + diag(L)[6] + diag(L)[7]) / sum(eigen(1 / dimension * W) $values) * 100
	list(points = C, quality2 = quality2, quality7 = quality7)
}

