# UV : SY19 - TP02
# Exercice 1
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)
library(cluster)

# Chargement des donnees, selection des variables quantitatives et normalisation 
data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]

# Matrice de distances
distmat = dist(donnees$num, method = "euclidean")
# Variance des données
vari = var(donnees$num)
# Nombre d'individus
dim1 = dim(donnees$num)
# Nombre de critères
dim(donnees$num)

# Question 1
# http://www.grappa.univ-lille3.fr/~ppreux/ensg/miashs/fouilleDeDonneesII/tp/k-moyennes/

k2 =  kmeans(donnees$num,2)
plot(donnees$num, col = c('red', 'blue')[k2$cluster])

k3 =  kmeans(donnees$num,3)
plot(donnees$num, col = c('red', 'blue', 'green')[k3$cluster])

k4 =  kmeans(donnees$num,4)
plot(donnees$num, col = c('red', 'blue', 'green', 'black')[k4$cluster])


# Question 2

tab = list()
for (n in 1:100)
{
	k3 = kmeans(donnees$num,3)
	inertie = sum(k3$withinss)/150

tab = list()
}   

# Question 3

for (K in 2:5)
{
	for (n in 1:100)
	{
		kK = kmeans(donnees$num,K)
		inertie = sum(kK$withinss)/150
	}
}

# Question 4
