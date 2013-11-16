# UV : SY19 - TP02
# Exercice3
# Fichier : mixtmult.R

gmixtmulti <- function (donnees, pik = NULL, muk = NULL, Sigmak = NULL, K = 2, fCEM = FALSE) {

	# modèle de mélange multidimensionnel,
	# sous l'hypothèse d'indépendance conditionnelle
	# (matrices de covariance diagonales)
	# nombre K de composantes quelconque

	n <- dim(donnees)[1]
	p <- dim(donnees)[2]

	# initialisation arbitraire des paramètres
	if (is.null(pik)) {
		pik <- rep(1,K) / K
	}

	if (is.null(muk)) {
		muk <- matrix(runif(K), nrow = K, ncol = p)
	}

	if (is.null(Sigmak)) {
		Sigmak <- matrix(1,nrow = K, ncol = p)
	}

	t <- matrix(0, ncol = K, nrow = n) # matrice des proba d'appartenance
	z <- matrix(0, ncol = K, nrow = n) # matrice des classes
	denscond <- matrix(0, ncol = K, nrow = n) # densités conditionnelles

	for (k in 1:K) {
		denscond[,k] <- 
	}

	logLold <- -1e250
	logL <- 
	print(logL)

	iter <- 0
	epsi <- 1e-8

	# tant que le point de convergence n'est pas atteint
	while (((logL - logLold) / abs(logLold)) > epsi) {
		iter <- iter + 1

		###### étape E ######
		t <- 

		###### étape C ######
		if (fCEM) {
		}

		###### étape M ######
		pik <- 
		for (k in 1:K) {
			muk[k,] <- 
			Sigmak[k,] <- 
		}

		for (k in 1:K) {
			denscond[,k] <- 
		}

		logLold <- logL
		logL <- 
		print(logL)
	}

	return (list(param = list(pik = pik, muk = muk, Sigmak = Sigmak), post = t, logL = logL))
}

mvdnorm <- function (X, mu, Sigma) {

	# calcul de la densité d'une loi normale multidimensionnelle
	# d'espérance mu et de matrice de covariance Sigma
	# aux points spécifiés dans le vecteur X
	# Benjamin Quost, 2009.09.13

	n <- dim(X)[1]
	p <- dim(X)[2]

	Xc <- X - matrix(rep(mu, n), nrow = n, byrow = T)
	densite <- exp(-1 / 2 * diag(Xc %*% ginv(Sigma) %*%t (Xc))) / ((2 * pi)^(p / 2) * det(Sigma)^(1 / 2))

	return (densite)
}

map <- function (x) {

	# règle d'affectation en fonction
	# du maximum de probabilité

	N <- dim(x)[1]
	K <- dim(x)[2]
	z <- matrix(0, nrow = N, ncol = K)

	for(n in 1:N) {
		m <- max(x[n, ])

		for(k in 1:K) {
			if(x[n, k] == m) break;
		}
		z[n,k] <- 1
	}
	return (z)
}

