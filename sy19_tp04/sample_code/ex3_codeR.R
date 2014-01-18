# Matrice des points
obs = matrix(c(1,2,6,4,5))

# Classes
classes = c(1,1,1,-1,-1)
classes = t(classes)

# Methode SVM
model = svm(obs, as.factor(classes), degree = 2, scale = FALSE, kernel = "polynomial",
coef0 = 1, cost = 100, gamma = 1)

> model$SV
     [,1]
[1,]    2
[2,]    6
[3,]    5

> model$coefs
          [,1]
[1,]  2.499180
[2,]  4.831745
[3,] -7.330924
