# UV : SY19 - TP02
# Exercice 1
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)
library(cluster)

# Chargement des données, sélection des variables quantitatives et normalisation 
data(iris)
donnees <- NULL
donnees$num <- iris[, c(1:4)]
donnees$cls <- iris[, 5]

# Question 1
# http://www.grappa.univ-lille3.fr/~ppreux/ensg/miashs/fouilleDeDonneesII/tp/k-moyennes/

# On part du 20ème point pour avoir une représentation plus fidèle du nuage que si nous étions partis du premier point
# pour obtenir les centres mobiles.
k2 =  kmeans(donnees$num, 2, nstart = 20)
k3 =  kmeans(donnees$num, 3, nstart = 20)
k4 =  kmeans(donnees$num, 4, nstart = 20)

png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue')[k2$cluster])
dev.off()

png(file = "plots/clusplot_exo1_kmeans_2.png")
clusplot(donnees$num, k2$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 2")
dev.off()

png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue', 'green')[k3$cluster])
dev.off()

png(file = "plots/clusplot_exo1_kmeans_3.png")
clusplot(donnees$num, k3$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 3")
dev.off()

png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue', 'green', 'black')[k4$cluster])
dev.off()

png(file = "plots/clusplot_exo1_kmeans_4.png")
clusplot(donnees$num, k4$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 4")
dev.off()

# Question 2
n = 100
tab = matrix(c(0, 0, 0), n, 3)
for (i in 1:n) {
	tab[i, 1] = kmeans(donnees$num,3)$withinss[1]
	tab[i, 2] = kmeans(donnees$num,3)$withinss[2]
	tab[i, 3] = kmeans(donnees$num,3)$withinss[3]
}
#	inertie = sum(c$withinss) / 150
# Question 3
for (K in 2:5) {

	for (n in 1:100) {

		kK = kmeans(donnees$num,K)
		inertie = sum(kK$withinss) / 150
	}
}

# Question 4

