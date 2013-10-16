# UV : SY19 - TP01
# 2. Les données de mutations
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)

# setwd("Z:/")
mutations = read.table("mutations2.txt", header = F, row.names = 1)

mutmat = as.matrix(mutations)
mutdist = as.dist(mutations)

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
a = aftd(as.dist(mutations))

aftd2 = cmdscale(as.dist(mutations), 2)

png(file = "plots/plot_mutations_aftd.png")
plot(a$C, main = "AFTD : Représentation de mutations sur les 2 premiers axes factoriels",xlab = "axe1", ylab = "axe2", type = "n")
text(a$C[,1], a$C[,2], labels(mutmat[,1]))
dev.off()

png(file = "plots/plot_mutations_cmdscale.png")
plot(aftd2, main = "CMDSCALE : Représentation de mutations sur les 2 premiers axes factoriels",xlab = "axe1", ylab = "axe2", type = "n")
text(aftd2[,1], aftd2[,2], labels(mutmat[,1]))
dev.off()

# Question 2

sammonmat = sammon(mutdist)
png(file = "plots/plot_mutations_sammon.png")
plot(sammonmat$points, type = "n", main = "Représentation de mutations par Sammon", xlab = "axe1", ylab = "axe2")
text(sammonmat$points, labels(mutmat[,1]))
dev.off()

# Projection de Kruskal
kruskalmat = isoMDS(mutdist)
png(file = "plots/plot_mutations_kruskal.png")
plot(kruskalmat$points, type = "n", main = "Représentation de mutations par Kruskal", xlab = "axe1", ylab = "axe2")
text(kruskalmat$points, labels(mutmat[,1]))
dev.off()

### Question 3

# Diagrammes de Shepard
s1 = Shepard(mutdist, aftd2)
png(file = "plots/plot_mutations_shepard_cmdscale.png")
plot(s1, main = "diagramme de Shepard de Mutations (cmdscale)", pch = "*")
abline(0, 1)
dev.off()

# TODO: shepard sur aftd()

s2 = Shepard(mutdist, sammonmat$points)
png(file = "plots/plot_mutations_shepard_sammon.png")
plot(s2, main = "diagramme de Shepard de Mutations (sammon)", pch = "*")
abline(0, 1)
dev.off()

s3 = Shepard(mutdist, kruskalmat$points)
png(file = "plots/plot_mutations_shepard_kruskal.png")
plot(s3, main = "diagramme de Shepard de Mutations (kruskal)", pch = "*")
abline(0, 1)
dev.off()

