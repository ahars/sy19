# UV : SY19 - TP03
# Exercice 2
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)
library(nnet)

# QUESTION 1

# 1.
p1 <- 0.25
p2 <-0.25
p3 <- 0.25
p4 <- 0.25
n <- 200

s1 <- c(1, 2)
s2 <- c(2, 1)
s3 <- c(1.5, 2)
s4 <- c(1, 1)

s <- rbind(s1, s2, s3, s4)

m1 <- c(4, 6)
m2 <- c(6, 1)
m3 <- c(-4, -4)
m4 <- c(0, 0)

m <- rbind(m1, m2, m3, m4)

c <- sample(c(1, 2, 3, 4), size = n, prob = c(p1, p2, p3, p4), replace = TRUE)
x <- cbind(rnorm(n, m[c,1], s[c,1]), rnorm(n, m[c,2], s[c,2]))

couleur <- rep("red", n)
couleur[c==2] <- "blue"
couleur[c==3] <- "green"
couleur[c==4] <- "yellow"

png("plots/frontiere_bayes_q1_1.png")
plot(x, col = couleur, main = "frontières de décision sur les observations de 4 lois normales")

# Frontiere de Bayes
len <- 50

xp <- seq(min(x[,1]), max(x[,1]), length = len)
yp <- seq(min(x[,2]), max(x[,2]), length = len)

grille <- expand.grid(z1 = xp, z2 = yp)

Z <- p1 * dnorm(grille[,1], m[1,1], s[1,1]) * dnorm(grille[,2], m[1,2], s[1,2])
Z <- cbind(Z, p2 * dnorm(grille[,1], m[2,1], s[2,1]) * dnorm(grille[,2], m[2,2], s[2,2]))
Z <- cbind(Z, p3 * dnorm(grille[,1], m[3,1], s[3,1]) * dnorm(grille[,2], m[3,2], s[3,2]))
Z <- cbind(Z, p4 * dnorm(grille[,1], m[4,1], s[4,1]) * dnorm(grille[,2], m[4,2], s[4,2]))

zp <- Z[,4] - pmax(Z[,3], Z[,2], Z[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

zp <- Z[,1] - pmax(Z[,2], Z[,3], Z[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

zp <- Z[,2] - pmax(Z[,1], Z[,3], Z[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
dev.off()

# 2.
# Donner la règle de Bayes

# 3.
# Développer la règle de Bayes pour arriver à la solution

##############################################################################

# QUESTION 2
T <- class.ind(couleur)
model1 <- nnet(x, T, size = 1, decay = 0, softmax = TRUE, maxit = 500)

# Valeur des poids
model1$wts

# Proba à postériori d'appartenance aux classes
Z1 <- predict(model1, grille)

boxplot(model1$wts)


# 1.
model2 <- nnet(x, T, size = 5, decay = 0, softmax = TRUE, maxit = 500)
model2$wts
Z2 <- predict(model2, grille)

plot(x, col = couleur)
zp <- Z2[,4] - pmax(Z2[,3], Z2[,2], Z2[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,1] - pmax(Z2[,2], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - pmax(Z2[,1], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

# 2.
boxplot(model2$wts)
# Dessin de chaque composante du poids.
plot(model2$wts)


# 3.
# à répéter plusieurs fois.
model2 <- nnet(x, T, size = 5, decay = 0, softmax = TRUE, maxit = 500)
model2$wts
Z2 <- predict(model2, grille)

plot(x, col = couleur)
zp <- Z2[,4] - pmax(Z2[,3], Z2[,2], Z2[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,1] - pmax(Z2[,2], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - pmax(Z2[,1], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

boxplot(model2$wts)

# Nous n'obtenons pas les mêmes résultats à chaque fois pour l'estimation
# des poids et des frontières de décision.

# 4.
# Le générateur de nombre aléatoire de R n'est pas réinitialisé
# donc nous n'obtenons pas les mêmes frontières.

##############################################################################

# QUESTION 3.

# 1.
set.seed(1)
T <- class.ind(couleur)
model1 <- nnet(x, T, size = 1, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model2 <- nnet(x, T, size = 2, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model3 <- nnet(x, T, size = 3, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model4 <- nnet(x, T, size = 4, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model5 <- nnet(x, T, size = 5, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model6 <- nnet(x, T, size = 6, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model7 <- nnet(x, T, size = 7, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model8 <- nnet(x, T, size = 8, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model9 <- nnet(x, T, size = 9, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model10 <- nnet(x, T, size = 10, decay = 0, softmax = TRUE, maxit = 500)

boxplot(model1$wts, model2$wts, model3$wts, model4$wts, model5$wts, model6$wts, model7$wts, model8$wts, model9$wts, model10$wts)
plot(model1$wts, ylim = c(-5, 5))
plot(model2$wts, ylim = c(-5, 5))
plot(model3$wts, ylim = c(-5, 5))
plot(model4$wts, ylim = c(-5, 5))
plot(model5$wts, ylim = c(-5, 5))
plot(model6$wts, ylim = c(-5, 5))
plot(model7$wts, ylim = c(-5, 5))
plot(model8$wts, ylim = c(-5, 5))
plot(model9$wts, ylim = c(-5, 5))
plot(model10$wts, ylim = c(-5, 5))

# Commenter le nombre de poids à estimer..

# 2.
Z1 <- predict(model1, grille)
Z2 <- predict(model2, grille)
Z3 <- predict(model3, grille)
Z4 <- predict(model4, grille)
Z5 <- predict(model5, grille)
Z6 <- predict(model6, grille)
Z7 <- predict(model7, grille)
Z8 <- predict(model8, grille)
Z9 <- predict(model9, grille)
Z10 <- predict(model10, grille)

plot(x, col = couleur)
zp <- Z1[,4] - pmax(Z1[,3], Z1[,2], Z1[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,1] - pmax(Z1[,2], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,2] - pmax(Z1[,1], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z2[,4] - pmax(Z2[,3], Z2[,2], Z2[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,1] - pmax(Z2[,2], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - pmax(Z2[,1], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z3[,4] - pmax(Z3[,3], Z3[,2], Z3[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,1] - pmax(Z3[,2], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,2] - pmax(Z3[,1], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z4[,4] - pmax(Z4[,3], Z4[,2], Z4[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,1] - pmax(Z4[,2], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,2] - pmax(Z4[,1], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z5[,4] - pmax(Z5[,3], Z5[,2], Z5[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,1] - pmax(Z5[,2], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,2] - pmax(Z5[,1], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z6[,4] - pmax(Z6[,3], Z6[,2], Z6[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,1] - pmax(Z6[,2], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,2] - pmax(Z6[,1], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z7[,4] - pmax(Z7[,3], Z7[,2], Z7[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z7[,1] - pmax(Z7[,2], Z7[,3], Z7[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z7[,2] - pmax(Z7[,1], Z7[,3], Z7[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z8[,4] - pmax(Z8[,3], Z8[,2], Z8[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z8[,1] - pmax(Z8[,2], Z8[,3], Z8[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z8[,2] - pmax(Z8[,1], Z8[,3], Z8[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z9[,4] - pmax(Z9[,3], Z9[,2], Z9[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z9[,1] - pmax(Z9[,2], Z9[,3], Z9[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z9[,2] - pmax(Z9[,1], Z9[,3], Z9[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z10[,4] - pmax(Z10[,3], Z10[,2], Z10[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z10[,1] - pmax(Z10[,2], Z10[,3], Z10[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z10[,2] - pmax(Z10[,1], Z10[,3], Z10[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

# Calcul de la probabilité d'erreur
pred1 <- predict(model1, x)
pred2 <- predict(model2, x)
pred3 <- predict(model3, x)
pred4 <- predict(model4, x)
pred5 <- predict(model5, x)
pred6 <- predict(model6, x)
pred7 <- predict(model7, x)
pred8 <- predict(model8, x)
pred9 <- predict(model9, x)
pred10 <- predict(model10, x)

classes_predict <- function(pred) {

	n = dim(pred)[1]
	m = dim(pred)[2]
	for (i in 1:n) {
		mm = max(pred[i,])
		for (j in 1:m) {
			if (pred[i,j] == mm) {
				pred[i,j] = 1
			} else {
				pred[i,j] = 0
			}
		}
	}
	return(pred)
}

cp1 = classes_predict(pred1)
cp2 = classes_predict(pred2)
cp3 = classes_predict(pred3)
cp4 = classes_predict(pred4)
cp5 = classes_predict(pred5)
cp6 = classes_predict(pred6)
cp7 = classes_predict(pred7)
cp8 = classes_predict(pred8)
cp9 = classes_predict(pred9)
cp10 = classes_predict(pred10)

proba_erreur_emp <- function(T, CP) {

	proba = 0
	n = dim(T)[1]
	m = dim(T)[2]
	for (i in 1:n) {
		CP[i,] = T[i,] - CP[i,]
		comp = 0
		for (j in 1:m) {
			if (CP[i,j] == 0) {
				comp = comp + 1
			}
		}
		if (comp != m) {
			proba = proba + 1
		}
	}
	return(proba / n)
}

proba1 = proba_erreur_emp(T, cp1)
proba2 = proba_erreur_emp(T, cp2)
proba3 = proba_erreur_emp(T, cp3)
proba4 = proba_erreur_emp(T, cp4)
proba5 = proba_erreur_emp(T, cp5)
proba6 = proba_erreur_emp(T, cp6)
proba7 = proba_erreur_emp(T, cp7)
proba8 = proba_erreur_emp(T, cp8)
proba9 = proba_erreur_emp(T, cp9)
proba10 = proba_erreur_emp(T, cp10)

# 3.
set.seed(1)
T <- class.ind(couleur)
model1 <- nnet(x, T, size = 1, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model2 <- nnet(x, T, size = 2, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model3 <- nnet(x, T, size = 3, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model4 <- nnet(x, T, size = 4, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model5 <- nnet(x, T, size = 5, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model6 <- nnet(x, T, size = 6, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model7 <- nnet(x, T, size = 7, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model8 <- nnet(x, T, size = 8, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model9 <- nnet(x, T, size = 9, decay = 0, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model10 <- nnet(x, T, size = 10, decay = 0, softmax = TRUE, maxit = 500)

boxplot(model1$wts, model2$wts, model3$wts, model4$wts, model5$wts, model6$wts, model7$wts, model8$wts, model9$wts, model10$wts)

Z1 <- predict(model1, grille)
Z2 <- predict(model2, grille)
Z3 <- predict(model3, grille)
Z4 <- predict(model4, grille)
Z5 <- predict(model5, grille)
Z6 <- predict(model6, grille)
Z7 <- predict(model7, grille)
Z8 <- predict(model8, grille)
Z9 <- predict(model9, grille)
Z10 <- predict(model10, grille)

plot(x, col = couleur)
zp <- Z1[,4] - pmax(Z1[,3], Z1[,2], Z1[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,1] - pmax(Z1[,2], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,2] - pmax(Z1[,1], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z2[,4] - pmax(Z2[,3], Z2[,2], Z2[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,1] - pmax(Z2[,2], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - pmax(Z2[,1], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z3[,4] - pmax(Z3[,3], Z3[,2], Z3[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,1] - pmax(Z3[,2], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,2] - pmax(Z3[,1], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z4[,4] - pmax(Z4[,3], Z4[,2], Z4[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,1] - pmax(Z4[,2], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,2] - pmax(Z4[,1], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z5[,4] - pmax(Z5[,3], Z5[,2], Z5[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,1] - pmax(Z5[,2], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,2] - pmax(Z5[,1], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z6[,4] - pmax(Z6[,3], Z6[,2], Z6[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,1] - pmax(Z6[,2], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,2] - pmax(Z6[,1], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z7[,4] - pmax(Z7[,3], Z7[,2], Z7[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z7[,1] - pmax(Z7[,2], Z7[,3], Z7[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z7[,2] - pmax(Z7[,1], Z7[,3], Z7[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z8[,4] - pmax(Z8[,3], Z8[,2], Z8[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z8[,1] - pmax(Z8[,2], Z8[,3], Z8[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z8[,2] - pmax(Z8[,1], Z8[,3], Z8[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z9[,4] - pmax(Z9[,3], Z9[,2], Z9[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z9[,1] - pmax(Z9[,2], Z9[,3], Z9[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z9[,2] - pmax(Z9[,1], Z9[,3], Z9[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z10[,4] - pmax(Z10[,3], Z10[,2], Z10[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z10[,1] - pmax(Z10[,2], Z10[,3], Z10[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z10[,2] - pmax(Z10[,1], Z10[,3], Z10[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

# 4.
# Commentaires sur la variabilité des modèles en fonction de size.
# Graphe Pe en fonction de Size

# Calcul de la probabilité d'erreur
pred1 <- predict(model1, x)
pred2 <- predict(model2, x)
pred3 <- predict(model3, x)
pred4 <- predict(model4, x)
pred5 <- predict(model5, x)
pred6 <- predict(model6, x)
pred7 <- predict(model7, x)
pred8 <- predict(model8, x)
pred9 <- predict(model9, x)
pred10 <- predict(model10, x)

cp1 = classes_predict(pred1)
cp2 = classes_predict(pred2)
cp3 = classes_predict(pred3)
cp4 = classes_predict(pred4)
cp5 = classes_predict(pred5)
cp6 = classes_predict(pred6)
cp7 = classes_predict(pred7)
cp8 = classes_predict(pred8)
cp9 = classes_predict(pred9)
cp10 = classes_predict(pred10)

proba1 = proba_erreur_emp(T, cp1)
proba2 = proba_erreur_emp(T, cp2)
proba3 = proba_erreur_emp(T, cp3)
proba4 = proba_erreur_emp(T, cp4)
proba5 = proba_erreur_emp(T, cp5)
proba6 = proba_erreur_emp(T, cp6)
proba7 = proba_erreur_emp(T, cp7)
proba8 = proba_erreur_emp(T, cp8)
proba9 = proba_erreur_emp(T, cp9)
proba10 = proba_erreur_emp(T, cp10)

probas = c(proba1, proba2, proba3, proba4, proba5, proba6, proba7, proba8, proba9, proba10)
plot(probas)

##############################################################################

# QUESTION 4.
set.seed(1)
T <- class.ind(couleur)
model1 <- nnet(x, T, size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model2 <- nnet(x, T, size = 5, decay = 0.01, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model3 <- nnet(x, T, size = 5, decay = 0.1, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model4 <- nnet(x, T, size = 5, decay = 1, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model5 <- nnet(x, T, size = 5, decay = 10, softmax = TRUE, maxit = 500)
set.seed(1)
T <- class.ind(couleur)
model6 <- nnet(x, T, size = 5, decay = 100, softmax = TRUE, maxit = 500)

Z1 <- predict(model1, grille)
Z2 <- predict(model2, grille)
Z3 <- predict(model3, grille)
Z4 <- predict(model4, grille)
Z5 <- predict(model5, grille)
Z6 <- predict(model6, grille)

plot(x, col = couleur)
zp <- Z1[,4] - pmax(Z1[,3], Z1[,2], Z1[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,1] - pmax(Z1[,2], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,2] - pmax(Z1[,1], Z1[,3], Z1[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z2[,4] - pmax(Z2[,3], Z2[,2], Z2[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,1] - pmax(Z2[,2], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - pmax(Z2[,1], Z2[,3], Z2[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z3[,4] - pmax(Z3[,3], Z3[,2], Z3[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,1] - pmax(Z3[,2], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,2] - pmax(Z3[,1], Z3[,3], Z3[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z4[,4] - pmax(Z4[,3], Z4[,2], Z4[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,1] - pmax(Z4[,2], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,2] - pmax(Z4[,1], Z4[,3], Z4[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z5[,4] - pmax(Z5[,3], Z5[,2], Z5[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,1] - pmax(Z5[,2], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,2] - pmax(Z5[,1], Z5[,3], Z5[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(x, col = couleur)
zp <- Z6[,4] - pmax(Z6[,3], Z6[,2], Z6[,1])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,1] - pmax(Z6[,2], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z6[,2] - pmax(Z6[,1], Z6[,3], Z6[,4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

# decay : paramètre de pénalisation / régularisation, qui influence la précision du modèle.
# Plus il est petit, plus le modèle proposé est complexe, plus il est grand, moins il est complexe et généraliste.

# 2.
boxplot(model1$wts, model2$wts, model3$wts, model4$wts, model5$wts, model6$wts)
plot(model1$wts, ylim = c(-5, 5))
plot(model2$wts, ylim = c(-5, 5))
plot(model3$wts, ylim = c(-5, 5))
plot(model4$wts, ylim = c(-5, 5))
plot(model5$wts, ylim = c(-5, 5))
plot(model6$wts, ylim = c(-5, 5))

# 3.
# Calcul de la probabilité d'erreur
pred1 <- predict(model1, x)
pred2 <- predict(model2, x)
pred3 <- predict(model3, x)
pred4 <- predict(model4, x)
pred5 <- predict(model5, x)
pred6 <- predict(model6, x)

cp1 = classes_predict(pred1)
cp2 = classes_predict(pred2)
cp3 = classes_predict(pred3)
cp4 = classes_predict(pred4)
cp5 = classes_predict(pred5)
cp6 = classes_predict(pred6)

proba1 = proba_erreur_emp(T, cp1)
proba2 = proba_erreur_emp(T, cp2)
proba3 = proba_erreur_emp(T, cp3)
proba4 = proba_erreur_emp(T, cp4)
proba5 = proba_erreur_emp(T, cp5)
proba6 = proba_erreur_emp(T, cp6)

probas = c(proba1, proba2, proba3, proba4, proba5, proba6)
plot(probas)

# La valeur optimale est 4 d'après le schéma.

##############################################################################

# Séparation selon le sexe
n <- 200
x_sexe <- log(cbind(crabs$FL, crabs$RW))
T_sexe <- class.ind(crabs$sex)
couleur1 <- rep("pink", n)
couleur1[crabs$sex=="M"] <- "cyan"

plot(x_sexe, col = couleur1)

# Séparation selon la couleur
x_color <- log(cbind(crabs$FL, crabs$RW))
T_color <- class.ind(crabs$sp)
couleur2 <- rep("orange", n)
couleur2[crabs$sp == "O"] <- "blue"

plot(x_color, col = couleur2)

# QUESTION 5

# 1.
p = 5

# À CHECKER POUR LA CLASSE 5
simul <- function (x, t, n, p) {

        data = matrix(nrow = n, ncol = 3)
        classe = matrix(nrow = n, ncol = 2)
        i = 1
        m = n / p
		
        for (ki in 1:p) {
        
        	for (j in 1:m) {

				d = dim(x)[1]
				if (is.null(d)) {
					d = 1
					rand = 1
					data[i,c(1,2)] = x
					classe[i, c(1,2)] = t
				} else {
					rand = sample(1:d, 1)
					data[i,1] = x[rand,1]
					data[i,2] = x[rand,2]
					x = x[-rand,]
					
					classe[i,1] = t[rand,1]
					classe[i,2] = t[rand,2]
					t = t[-rand,]
				}
				data[i,3] = ki
				i = i + 1
			}
        }
        result = new.env()
        result$data <- data
        result$classe <- classe
        
        return (result)
}

t_sexe = simul(x_sexe, T_sexe, n, p)
couleur3 <- rep("pink", n)
couleur3[t_sexe$classe[,1] == "0"] <- "cyan"

t_color = simul(x_color, T_color, n, p)
couleur4 <- rep("orange", n)
couleur4[t_color$classe[,1] == "0"] <- "blue"

plot(t_sexe$data, col = couleur3)
plot(t_color$data, col = couleur4)

# Estimation sur la probabilité d'erreur sur le SEXE
set.seed(1)
model1 <- nnet(t_sexe$data[-1:-40,1:2], t_sexe$classe[-1:-40,], size = 6, decay = 0.001, softmax = TRUE, maxit = 500)
pred1 <- predict(model1, t_sexe$data[1:40,1:2])
cp1 = classes_predict(pred1)
proba1 = proba_erreur_emp(t_sexe$classe[1:40], cp1)

set.seed(1)
model2 <- nnet(t_sexe$data[-41:-80,1:2], t_sexe$classe[-41:-80,], size = 6, decay = 0.001, softmax = TRUE, maxit = 500)
pred2 <- predict(model2, t_sexe$data[41:80,1:2])
cp2 = classes_predict(pred2)
proba2 = proba_erreur_emp(t_sexe$classe[41:80], cp2)

set.seed(1)
model3 <- nnet(t_sexe$data[-81:-120,1:2], t_sexe$classe[-81:-120,], size = 6, decay = 0.001, softmax = TRUE, maxit = 500)
pred3 <- predict(model3, t_sexe$data[81:120,1:2])
cp3 = classes_predict(pred3)
proba3 = proba_erreur_emp(t_sexe$classe[81:120], cp3)

set.seed(1)
model4 <- nnet(t_sexe$data[-121:-160,1:2], t_sexe$classe[-121:-160,], size = 6, decay = 0.001, softmax = TRUE, maxit = 500)
pred4 <- predict(model4, t_sexe$data[121:160,1:2])
cp4 = classes_predict(pred4)
proba4 = proba_erreur_emp(t_sexe$classe[121:160], cp4)

set.seed(1)
model5 <- nnet(t_sexe$data[-161:-200,1:2], t_sexe$classe[-161:-200,], size = 6, decay = 0.001, softmax = TRUE, maxit = 500)
pred5 <- predict(model5, t_sexe$data[161:200,1:2])
cp5 = classes_predict(pred5)
proba5 = proba_erreur_emp(t_sexe$classe[161:200], cp5)

probas_sexe = c(proba1, proba2, proba3, proba4, proba5)
pe = mean(probas_sexe)
plot(probas_sexe)

# 2.
xp <- seq(min(t_sexe$data[,1]), max(t_sexe$data[,1]), length = len)
yp <- seq(min(t_sexe$data[,2]), max(t_sexe$data[,2]), length = len)

grille <- expand.grid(z1 = xp, z2 = yp)

Z1 <- predict(model1, grille)
Z2 <- predict(model2, grille)
Z3 <- predict(model3, grille)
Z4 <- predict(model4, grille)
Z5 <- predict(model5, grille)

plot(t_sexe$data[41:200], col = couleur3[41:200])
zp <- Z1[,1] - max(Z1[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,2] - max(Z1[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_sexe$data[-41:-80], col = couleur3)
zp <- Z2[,1] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_sexe$data[-81:-120], col = couleur3)
zp <- Z3[,1] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,2] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_sexe$data[-121:-160], col = couleur3)
zp <- Z4[,1] - max(Z4[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,2] - max(Z4[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_sexe$data[-161:-200], col = couleur3)
zp <- Z5[,1] - max(Z5[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,2] - max(Z5[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)


##############################################################################

# QUESTION 6

# 1.
# Estimation sur la probabilité d'erreur sur la COULEUR
set.seed(1)
model1 <- nnet(t_color$data[-1:-40,1:2], t_color$classe[-1:-40,], size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
pred1 <- predict(model1, t_color$data[1:40,1:2])
cp1 = classes_predict(pred1)
proba1 = proba_erreur_emp(t_color$classe[1:40], cp1)

set.seed(1)
model2 <- nnet(t_color$data[-41:-80,1:2], t_color$classe[-41:-80,], size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
pred2 <- predict(model2, t_color$data[41:80,1:2])
cp2 = classes_predict(pred2)
proba2 = proba_erreur_emp(t_color$classe[41:80], cp2)

set.seed(1)
model3 <- nnet(t_color$data[-81:-120,1:2], t_color$classe[-81:-120,], size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
pred3 <- predict(model3, t_color$data[81:120,1:2])
cp3 = classes_predict(pred3)
proba3 = proba_erreur_emp(t_color$classe[81:120], cp3)

set.seed(1)
model4 <- nnet(t_color$data[-121:-160,1:2], t_color$classe[-121:-160,], size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
pred4 <- predict(model4, t_color$data[121:160,1:2])
cp4 = classes_predict(pred4)
proba4 = proba_erreur_emp(t_color$classe[121:160], cp4)

set.seed(1)
model5 <- nnet(t_color$data[-161:-200,1:2], t_color$classe[-161:-200,], size = 5, decay = 0.001, softmax = TRUE, maxit = 500)
pred5 <- predict(model5, t_color$data[161:200,1:2])
cp5 = classes_predict(pred5)
proba5 = proba_erreur_emp(t_color$classe[161:200], cp5)

probas_color = c(proba1, proba2, proba3, proba4, proba5)
pe = mean(probas_color)
plot(probas_color)

# 2.
len <- 50

xp <- seq(min(t_color$data[,1]), max(t_color$data[,1]), length = len)
yp <- seq(min(t_color$data[,2]), max(t_color$data[,2]), length = len)

grille <- expand.grid(z1 = xp, z2 = yp)

Z1 <- predict(model1, grille)
Z2 <- predict(model2, grille)
Z3 <- predict(model3, grille)
Z4 <- predict(model4, grille)
Z5 <- predict(model5, grille)

plot(t_color$data, col = couleur4)
zp <- Z1[,1] - max(Z1[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z1[,2] - max(Z1[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_color$data, col = couleur4)
zp <- Z2[,1] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z2[,2] - max(Z2[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_color$data, col = couleur4)
zp <- Z3[,1] - max(Z3[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z3[,2] - max(Z3[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_color$data, col = couleur4)
zp <- Z4[,1] - max(Z4[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z4[,2] - max(Z4[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

plot(t_color$data, col = couleur4)
zp <- Z5[,1] - max(Z5[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z5[,2] - max(Z5[,2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)

