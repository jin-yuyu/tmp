
# =====================================
# Generalized version of Example 8.5-2 (sigma: unknown)
# -------------------------------------
# General Theoretical power (sigma: unknown)
K3 = function(mu,mu0,n,sigma,alpha) {
    ta = qt(1-alpha, df=n-1)                        # Here, slightly different
    delta = (mu-mu0)/(sigma/sqrt(n))
    1-pt(ta, df=n-1, ncp=delta)
}


# 
# Empirical power (through simulation)
set.seed(1)
ITER = 1000
mu0=0; n=5; sigma=2; alpha=0.05
MU=seq(0, 5, length=51)
power = numeric(length(MU))

# Assume that sigma is unknown.
for ( j in 1:length(MU) ) {
    mu = MU[j]
    ta = qt(1-alpha, df=n-1)                        # Here, slightly different
    for ( i in 1:ITER ) {
        X = rnorm(n=n, mean=mu, sd=sigma)
        T = (mean(X)-mu0)/(sd(X)/sqrt(n))           # Here, slightly different (S is used)
        if ( T > ta) power[j] = power[j] + 1/ITER   # Here, slightly different
   }
}

curve(K3(mu,mu0=mu0,n=n,sigma=sigma,alpha=alpha), from=0, to=5, ylim=c(0,1), xname="mu", lty=2)
lines(MU, power, col="cyan")





