# UV : SY19 - TP02
# Exercice 1
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)
library(cluster)

# Chargement des données, sélection des variables quantitatives et normalisation 
data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]

# Matrice de distances
distmat = dist(donnees$num, method = "euclidean")
# Variance des données
vari = var(donnees$num)
# Nombre d'individus
dim = dim(donnees$num)

# Question 1
# http://www.grappa.univ-lille3.fr/~ppreux/ensg/miashs/fouilleDeDonneesII/tp/k-moyennes/

cl2 =  kmeans(donnees$num, 2)$cluster
png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue')[cl2])
dev.off()
png(file = "plots/clusplot_exo1_kmeans_2.png")
clusplot(donnees$num, cl2, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 2")
dev.off()

cl3 =  kmeans(donnees$num, 3)$cluster
png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue', 'green')[cl3])
dev.off()
png(file = "plots/clusplot_exo1_kmeans_3.png")
clusplot(donnees$num, cl3, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 3")
dev.off()

cl4 =  kmeans(donnees$num, 4)$cluster
png(file = "plots/plot_exo1_kmeans_2.png")
plot(donnees$num, col = c('red', 'blue', 'green', 'black')[cl4])
dev.off()
png(file = "plots/clusplot_exo1_kmeans_4.png")
clusplot(donnees$num, cl4, color = TRUE, shade = TRUE, labels = 2, main = "Clusplot iris K = 4")
dev.off()

# Question 2
tab = list()
for (n in 1:100) {
	k3 = kmeans(donnees$num,3)
	inertie = sum(k3$withinss) / 150

	tab = list()
}

# Question 3
for (K in 2:5) {

	for (n in 1:100) {

		kK = kmeans(donnees$num,K)
		inertie = sum(kK$withinss) / 150
	}
}

# Question 4

