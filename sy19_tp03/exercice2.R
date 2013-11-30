# UV : SY19 - TP03
# Exercice 2
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice2.R

library(MASS)
library(nnet)

# QUESTION 1
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
plot(x, col = couleur)

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
# Phénomène à expliquer..

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

