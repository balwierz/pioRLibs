normalize.to.reference.power.law.distribution <- function(values, lin.reg.coef, alpha = 1.25, T = 10^6)
{
	a <- lin.reg.coef[1]
	b <- lin.reg.coef[2] 
	
	# patch to go from revcum coefficients to density coefficients (derivative of x^a ~ x^(a-1)):
	a <- a - 1
	alpha <- alpha + 1
	
	if(alpha <= 1 || (-a) <= 1)
		stop("alpha needs to be positive")
	# patch: missing zeta(-a)
	lambda <- ( (    T  * VGAM::zeta(-a)   )
				        / 
			    (exp(b) * VGAM::zeta(alpha))
			  )^(1/alpha)
	# exp(b) could be changed to sum(values), but this assumes that ALL values are being normalised
	# and focuses on low counts, not the region fitted.
	#was: lambda <- (T/(VGAM::zeta(alpha) * exp(b)))^(1/alpha)
	beta <- -1 * a/alpha
	values.norm <- lambda * (values)^beta
	return(values.norm)
	
}
