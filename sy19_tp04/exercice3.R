# UV : SY19 - TP04
# Exercice 3
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice3.R

library(MASS)
install.packages(repos = NULL, pkgs = "e1071_1.6-1.tar.gz")
library(e1071)

# Points 
x1 = 1
x2 = 2
x3 = 6
x4 = 4
x5 = 5

# Classes
classes = c(1,1,1,-1,-1)
classes = t(classes) 
couleur = c("green", "green", "green", "blue", "blue")

# Matrice des points
obs = matrix(c(1,2,6,4,5))

# Méthode SVM
model = svm(obs, as.factor(classes), degree = 2, scale = FALSE, kernel = "polynomial", coef0 = 1, cost = 100, gamma = 1)

# Question 5
K <- function(xi, x) {
	return ((xi * x + 1)^2)
}

g <- function(model,obs,x) {
	n = length(x)
	r = c(1:n)
	for (i in 1:n) {
		r[i] <- model$coefs[1] * K(obs[model$index[1]], x[i]) + model$coefs[2] * K(obs[model$index[2]], x[i]) + model$coefs[3] * K(obs[model$index[3]], x[i]) - model$rho
	}
	return (r)
}

g2 <- function(x) {	
	classes = c(1,1,1,-1,-1)
	classes = t(classes) 
	obs = matrix(c(1,2,6,4,5))
	model = svm(obs, as.factor(classes), degree = 2, scale = FALSE, kernel = "polynomial", coef0 = 1, cost = 100, gamma = 1)

	n = length(x)
	r = c(1:n)
	for (i in 1:n) {
		r[i] <- model$coefs[1] * K(obs[model$index[1]], x[i]) + model$coefs[2] * K(obs[model$index[2]], x[i]) + model$coefs[3] * K(obs[model$index[3]], x[i]) - model$rho
	}
	return (r)
}

x = seq(from = 0, to = 7, by = 0.5)
res = g(model, obs, x)

png("plots/ex3_graphe.png")
plot(x, res, type = 'l', col = "pink", lwd = 3, xlab = "x", ylab = "g(x)", main = "Visualisation de la fonction d'apprentissage g(x)")
points(obs, classes, col = "black", lwd = 2)
points(obs[2], classes[2], col = "red", lwd = 4)
points(obs[3], classes[3], col = "red", lwd = 4)
points(obs[5], classes[5], col = "red", lwd = 4)
abline(1, 0, col = "green", lwd = 1)
abline(-1, 0, col = "blue", lwd = 1)
abline(-1, 0, col = "blue", lwd = 1)
text(0.5, 1.5, expression(g(x) == 1), col = "green")
text(0.5, -0.5, expression(g(x) == -1), col = "blue")
dev.off()

png("plots/ex3_graphe.png")
plot(x, res, type = 'l', col = "green", lwd = 3, xlab = "x", ylab = "g(x)", main = "Visualisation de la fonction d'apprentissage g(x)")
abline(0, 0, col = "black", lwd = 1)
text(1, 6, expression(g(x)), col = "green")
text(0.2, 0.5, expression(axe_x), col = "black")
points(obs[1], 0, col = "red", lwd = 1)
points(obs[2], 0, col = "red", lwd = 5)
points(obs[3], 0, col = "red", lwd = 5)
points(obs[4], 0, col = "blue", lwd = 1)
points(obs[5], 0, col = "blue", lwd = 5)
points(5.58, 0, col = "cyan", lwd = 2)
points(2.42, 0, col = "cyan", lwd = 2)
abline(v = 5.58, col = "cyan")
abline(v = 2.42, col = "cyan")
text(3, 6, expression(x == 2.42), col = "cyan")
text(6.2, 6, expression(x == 5.58), col = "cyan")
dev.off()

uniroot(g2, interval = c(0, 4))
uniroot(g2, interval = c(4, 7))

