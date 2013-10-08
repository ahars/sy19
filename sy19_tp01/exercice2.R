# UV : SY19 - TP01
# 2. Les données de mutations
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)

# setwd("Z:/")
mutations = read.table("mutations2.txt", header = F, row.names = 1)

aftd <- function (d) {

	dim_mat = as.matrix(d)
	dimension = diag(dim(dim_mat))[1]
	id = diag(dimension)

	un = matrix(rep(1, dimension^2), dimension)
	q = id - (1/dimension) * un
	w = -(1/2) * q %*% dim_mat^2 %*% q

	cent = scale((1 / dimension) * w, center = TRUE, scale = FALSE)
	vari = (1 / dimension) * t(cent) %*% cent
	diag = eigen(vari)

	sort(diag$values, decreasing = TRUE)
	sort(diag$vectors, decreasing = TRUE)

	return (diag)
}

aftd1 = aftd(as.dist(mutations))
aftd2 = cmdscale(as.dist(mutations), 14)

