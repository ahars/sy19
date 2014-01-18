# UV : SY19 - TP04
# Exercice 1
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice1.R

library(MASS)
install.packages(repos = NULL, pkgs = "e1071_1.6-1.tar.gz")
library(e1071)

# Points 
x1 = c(2,0)
x2 = c(0,2)
x3 = c(-2,2)
x4 = c(-1,3)

# Matrice des points
obs = matrix(c(2,0,-2,-1,0,2,2,3), ncol = 2)

# Vecteur des classes
classes = c(1,-1,-1,-1)
classes = t(classes) 
couleur = c("blue", "green", "green", "green")

# Plot des points
png("plots/ex1_hyperplan.png")
plot(obs, col = couleur, xlim = c(-3,3), ylim = c(-2,3), xlab = "", ylab ="", main = "Données d'apprentissage et hyperplan séparateur")
abline(0, 1, col = "red", lwd = 3)
abline(2, 1, col = "green", lwd = 1)
abline(-2, 1, col = "blue", lwd = 1)
text(0.35, 0, expression(x == y), col = "red")
text(-2.65, 0, expression(y == x + 2), col = "green")
text(2.65, 0, expression(y == x - 2 ), col = "blue")
dev.off()

# Méthode SVM
model = svm(obs, as.factor(classes), scale = FALSE, type = NULL, kernel = "linear")

# Affichage des vecteurs supports
model$SV

# Affichage des coefficients de lagrange * traininglevels
model$coefs

