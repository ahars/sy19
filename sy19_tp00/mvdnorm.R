# UV : SY19 - TP00
# Fonction mvdnorm
# Fichier : mvdnorm.R

mvdnorm <- function(X, mu, Sigma)
{
	# computes the density of a multivariate normal distribution 
	# with mean mu and covariance matrix Sigma 
	# at the points specified in X 
	# Benjamin Quost, 2009.09.13 

	n <- dim(X)[1]
	p <- dim(X)[2]

	Xc <- X - matrix(rep(mu,n),nrow=n,byrow=T)
	density <- exp(-1/2*diag(Xc%*%ginv(Sigma)%*%t(Xc))) / ((2*pi)^(p/2)*det(Sigma)^(1/2))
	return(density)
}

