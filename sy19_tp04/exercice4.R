# UV : SY19 - TP04
# Exercice 4
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice4.R

library(MASS)
install.packages(repos = NULL, pkgs = "e1071_1.6-1.tar.gz")
library(e1071)

################################################

# lire " breast cancer data " :
bcdata <- read.csv("breast-cancer-wisconsin.data", head = TRUE)

# Regarder les noms des colonnes.
names(bcdata)

# regarder une colonne, par exemple:
bcdata$ClumpThickness # ou bcdata$Samplecodenumber

# regarder les classes:
bcdata$Class

# Enlever de bcdata la colonne "Samplecodenumber" et la colonne
# "Class". Le signe moins pour dire unselect les deux colonnes:
databcall <- subset(bcdata, select = c(-Samplecodenumber, -Class))
# vous pouvez faire names(databcall)

# Selectionner les classes seulement:
classesbcall <- subset(bcdata, select = Class)

# prendre une partie de databcall pour l’apprentissage:
databctrain <- databcall[1:400,]
classesbctrain <- classesbcall[1:400,]

# prendre une partie de databcall pour le test:
databctest <- databcall[401:699,]
classesbctest <- classesbcall[401:699,]

##################################################

##################################################

# Faire help(svm) pour bien comprendre la fonction "svm" :
model <- svm(databctrain, classesbctrain)

# Faire str(model)
# Faire help(predict.svm) #Attention, ici "model" est un objet de
# la classe "svm". Ainsi, il faut faire help(predict.svm) et non pas
# help(predict)
# Validation de "model" :
pred <- predict(model, databctest)

# Comparer la prediction et les vraies classes :
table(pred, t(classesbctest))

# Les hyperparametres affecte les performances du noyau. Le
# package e1071 offre une fonction, tune(), qui fait ce qu’on
# appelle "grid search" et donne ainsi une estimation des
# parametres. Pour cela, faire : help(tune)
# Exemple d’application de la fonction "tune" :
a = tune(svm, train.x = databctrain, train.y = classesbctrain, validation.x = databctest, validation.y = classesbctest, ranges = list(gamma = 10^(-1:1), cost = c(1,1.5,2)), control = tune.control(sampling = "fix"))

# Faire str(a)
model <- svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = a$best.parameters$cost)

# Comparaison de la prediction et des vraies classes
pred <- predict(model, databctest)
table(pred, t(classesbctest))
##################################################

# 2.
b = c()
for(i in 1:100) {
	ta <- table(predict(svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = i, kernel = "radial"), databctest), t(classesbctest))
	b <- cbind(b, c(round(i, digits = 3), ((ta[1,2] + ta[2,1]) / sum(ta)) * 100))
}

png("plots/ex4_q2_proba_erreur_gamma.png")
plot(t(b), main = "Variation proba d’erreur en fonction de gamma", xlab = "paramètre de régularisation gamma", ylab = "la probabilité d'erreur", col = "red")
dev.off()

# composantes du vecteur alpha
png("plots/ex4_q2_boxplot_alpha_.png")
par(mfrow = c(2, 5))
for(i in 1:10) {
	boxplot(svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = i^2, kernel = "radial")$coefs, main = round(i^2, digits = 3))
}
dev.off()

b = c()
for(i in 1:100) {
	b <- cbind(b, c(round(i, digits = 3), dim(svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = i, kernel = "radial")$coefs)[1]))
}
png("plots/ex4_q2_alpha_positifs.png")
plot(t(b), main = "Variation des alpha positifs en fonction de gamma", xlab = "paramètre de régularisation gamma", ylab = "le nombre d'alpha non nuls", col = "red")
dev.off()

# 3.
# cost = 100 = infini
b = c()
for(i in 1:100) {
	ta <- table(predict(svm(databctrain, classesbctrain, gamma = i / 100, cost = 100, kernel = "radial"), databctest), t(classesbctest))
	b <- cbind(b, c(i / 100, ((ta[1,2] + ta[2,1]) * 100) / sum(ta)))
}

png("plots/ex4_q3_proba_erreur_gamma1.png")
plot(t(b), xlab = "largeur de bande (intervalle [0;1])", ylab = "la probabilité d'erreur", main = "Variation proba d'erreur en fonction de gamma", col = "red")
dev.off()

# probabilité d'erreur en fonction du paramètre de pénalisation 
ba = c()
for (i in 1:50) {
	ta <- table(predict(svm(databctrain, classesbctrain, gamma = i / 2, cost = 100, kernel = "radial"), databctest), t(classesbctest))
	ba <- cbind(ba, c(i / 2, ((ta[1,2] + ta[2,1]) * 100) / sum(ta)))
}
png("plots/ex4_q3_proba_erreur_gamma2.png")
plot(t(ba), xlab = "largeur de bande (intervalle [0;25])", ylab = "la probabilité d'erreur", main = "Variation proba d'erreur en fonction de gamma", col = "red")
dev.off()

b = c()
for(i in 1:50) {
	b <- cbind(b, c(round(i / 50, digits = 3), dim(svm(databctrain, classesbctrain, gamma = i / 50, cost = 100, kernel = "radial")$coefs)[1]))
}
png("plots/ex4_q3_alpha_positifs.png")
plot(t(b), xlab = "largeur de bande", ylab = "le nombre d'alpha non nuls", main = "Variation des alpha positifs en fonction de gamma", col = "red")
dev.off()

# composantes du vecteur alpha
png("plots/ex4_q3_boxplot_alpha_.png")
par(mfrow = c(1, 5))
for(i in 1:5) {
	boxplot(svm(databctrain, classesbctrain, gamma = i / 50, cost = 100, kernel = "radial")$coefs, main = round(i / 20, digits = 3))
}
dev.off()

# 4.
a = tune(svm, train.x = databctrain, train.y = classesbctrain, validation.x = databctest, validation.y = classesbctest, ranges = list(gamma = 1, cost = c(1:100)), control = tune.control(sampling = "fix"))

b = c()
# gaussien
model <- svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = a$best.parameters$cost, kernel = "radial")
pred <- predict(model, databctest)
ta <- table(pred, t(classesbctest))
#b <- cbind(b, c(1, ((ta[1,2] + ta[2,1]) * 100) / sum(ta)))

# polynomial
for(i in 1:15) {
	model <- svm(databctrain, classesbctrain, gamma = a$best.parameters$gamma, cost = a$best.parameters$cost, coef0 = 1, degree = i, kernel = "polynomial")
	pred <- predict(model, databctest)
	ta <- table(pred, t(classesbctest))
	b <- cbind(b, c(2, ((ta[1,2] + ta[2,1]) * 100) / sum(ta)))
}
png("plots/ex4_q4_polynomial.png")
plot(t(b)[,2], main = "fonction noyau polynomial", xlab = "évolution du degré", ylab = "probabilité d'erreur", col = "red")
dev.off()

