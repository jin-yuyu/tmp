# Theoretical power 
K = function(mu) { 1-pnorm( (62-mu)/2 ) } 

# =====================================
# Generalized version of Example 8.5-2 (sigma: known)
# -------------------------------------
# General Theoretical power 
K2 = function(mu,mu0,n,sigma,alpha) { 
    za = qnorm(1-alpha)
    1-pnorm( (mu0-mu)/(sigma/sqrt(n))+za ) 
}


# Check 
K2 (mu=60, mu0=60, n=25, sigma=10, alpha=0.1587)  # it should give alpha
K2 (mu=65, mu0=60, n=25, sigma=10, alpha=0.1587)  # it should be the same as K(65)


# Empirical power (through simulation)
set.seed(1)
ITER = 1000
mu0=0; n=5; sigma=2; alpha=0.05
MU=seq(0, 5, length=51)
power = numeric(length(MU))

# Assume that sigma known
for ( j in 1:length(MU) ) {
    mu = MU[j]
    za = qnorm(1-alpha)                             # Here, slightly different
    for ( i in 1:ITER ) {
        X = rnorm(n=n, mean=mu, sd=sigma)
        Z = (mean(X)-mu0)/(sigma/sqrt(n))           # Here, slightly different
        if ( Z > za) power[j] = power[j] + 1/ITER   # Here, slightly different
   }
}

curve(K2(mu,mu0=mu0,n=n,sigma=sigma,alpha=alpha), from=0, to=5, ylim=c(0,1), xname="mu" )
lines(MU, power, col="red")





