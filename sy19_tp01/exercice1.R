# UV : SY19 - TP01
# 1. Exercice théorique
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)
x = matrix(c(8.5, 1.5, 3.5, 5.0, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5), 2)
x = t(x)

# Première partie : ACP
cent = scale(x, center = TRUE, scale = FALSE)
s = (1 / 8) * t(cent) %*% cent
diag = eigen(s)

# Les axes factoriels
u1 = diag$vectors[,1]
u2 = diag$vectors[,2]

landa1 = diag$values[1]
landa2 = diag$values[2]

# princomp(x)

# Calcul des pourcentages d'inertie expliquée
inertie_explique = function(d) {

	l = length(d)
	result = matrix(ncol = l)
	a = sum(abs(d))
	for (k in 1:l) {

		result[k] = d[k] /a * 100
	}
	for (k in 2:l) {
		result[k] = result[k] + result[k-1]
	}
	return (result)
}
inerties = inertie_explique(diag$values)

# a = sum(diag$values)
# axe_inertie1 = diag$values[1] / a * 100
# axe_inertie2 = diag$values[2] / a * 100

# Calcul de la matrice des composantes principales.
m = diag(1, 2)
u = diag$vectors

C = x %*% m %*% u
#compx = princomp(x)
#comp = cent %*% diag$vectors

png(file = "plots/biplot_exo1_princomp.png")
plot(C, main = "Représentation des individus dans le premier plan factoriel", ylab = "axe 2", xlab = "axe 1", col = "red")
text(C, pos = 1, labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
dev.off()

# 3)
# Calcul qui nous donne la matrice X
somme = C[,1] %*% t(u1) + C[,2] %*% t(u2)



# Deuxième partie : MDS
# 1)
d = dist(x, method = "euclidean")
d2 = d^2

# 2)
w1 = cent %*% t(cent)

id8 = diag(8)
distance <- as.matrix(d2)
un = matrix(rep(1, 64), 8)
q8 = id8 - (1/8) * un
w2 = -(1/2) * q8 %*% distance %*% q8

# 3)
n = 1 / 8
cent = scale(n * w1, center = TRUE, scale = FALSE)
vari = (1/8) * t(cent) %*% cent
diag = eigen(vari)
# Il faut montrer que les valeurs propres sont positives ou nulles pour pouvoir dire que W est une matrice semi-définie positive.

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

	# Calcul de la matrice w
	w = -(1/2) * q %*% d2 %*% q

	# Matrice associée aux vecteurs propres
	V = eigen(1 / dimension * w)$vectors[,1:7]
	V = sqrt(dimension) * V

	# Matrice associées aux valeurs propres
	L = diag(c(eigen(1 / dimension * w)$values[1:7]))

	# Calcul composante principale
	C = V %*% sqrt(L)

	# Calcul du pourcentage d'inertie pour les 7 premières valeurs propres
	quality = sum(diag(L)) / sum(eigen(1 / dimension * w) $values) * 100
	
	result <- new.env()
	result$quality <- quality
	result$C <- C

	return(result)
}

a <- new.env()
a = aftd(d)

cc = cmdscale(d)
png(file = "plots/plot_exo1_aftd.png")
plot(cc, main = "Représentation de X par la fonction aftd()",xlab = "axe1", ylab = "axe2", type = "n")
text(a$C[,1], a$C[,2])
dev.off()

png(file = "plots/plot_exo1.png")
plot(x, main = "Représentation de X",xlab = "axe1", ylab = "axe2")
dev.off()

