# Matrice des points
obs = matrix(c(2,0,-2,-1,0,2,2,3), ncol=2)

# Vecteur des classes
classes = c(1,-1,-1,-1)
classes = t(classes)

# Methode SVM
model = svm(obs,as.factor(classes), scale = FALSE, type = NULL, kernel = "linear")
