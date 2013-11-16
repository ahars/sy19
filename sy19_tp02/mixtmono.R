# UV : SY19 - TP02
# Fichier : mixtmono.R

gmixtmono <- function (donnees, mu = NULL, sigma2 = NULL, fCEM = FALSE) {

	# modele de mélange en 1D,
	# pour K = 2 composantes, proportions identiques

	n <- length(donnees)

	# initialisation arbitraire des paramètres
	if (is.null(mu)) {
		mu <- matrix(runif(2), nrow = 2, ncol = 1)
	}

	if (is.null(sigma2)) {
		sigma2 <- matrix(1, nrow = 2, ncol = 1)
	}

	t <- matrix(0, ncol = 2, nrow = n) # matrice des proba d'appartenance
	z <- matrix(0, ncol = 2, nrow = n) # matrice des classes
	denscond <- matrix(0, ncol = 2, nrow = n) # densites conditionnelles

	denscond[,1] <- dnorm(donnees, mean = mu[1], sd = sigma2[1])
	denscond[,2] <- dnorm(donnees, mean = mu[2], sd = sigma2[2])

	logLold <- -1e250
	logL <- sum(log(apply(pi * denscond, 1, sum))) # log-vraisemblance
	print(logL)

	iter <- 0
	epsi <- 1e-8

    # tant que le point de convergence n'est pas atteint
	while (((logL - logLold) / abs(logLold)) > epsi) {

		iter <- iter + 1

		###### etape E ######
		t[,1] <- (pi * denscond[,1]) / (pi * denscond[,1] + pi * denscond[,2])
		t[,2] <- (pi * denscond[,2]) / (pi * denscond[,1] + pi * denscond[,2])

		###### etape C ######
		if (fCEM) {
			t <- map(t)
		}

		###### etape M ######
		mu[1,] <- (sum(t[,1] * donnees)) / sum(t[,1])
		mu[2,] <- (sum(t[,2] * donnees)) / sum(t[,2])
		sigma2[1,] <- sum(t[,1] * (donnees - mu[1])^2) / sum(t[,1])
		sigma2[2,] <- sum(t[,2] * (donnees - mu[2])^2) / sum(t[,2])

		denscond[,1] <- dnorm(donnees, mean = mu[1], sd = sigma2[1])
		denscond[,2] <- dnorm(donnees, mean = mu[2], sd = sigma2[2])

		logLold <- logL
		logL <- sum(log(apply(pi * denscond, 1, sum)))
		print(logL)
	}

	return (list(param = list(mu = mu, sigma2 = sigma2), post = t, logL = logL))
}

map <- function (x) {

	# règle d'affectation en fonction
	# du maximum de probabilité

    N <- dim(x)[1]
    K <- dim(x)[2]
    z <- matrix(0, nrow = N, ncol = K)

    for(n in 1:N) {

        m <- max(x[n, ])
        for (k in 1:K) {
            if (x[n, k] == m) break;
        }
        z[n, k] <- 1
    }
    return (z)
}

