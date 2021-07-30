
# Theoretical power 
K = function(mu) { 1-pnorm( (62-mu)/2 ) } 

# =====================================
# Figure 8.5-2 (sigma: known)
# -------------------------------------
# Empirical power (through simulation)
set.seed(1)
ITER = 1000
n=25; sigma=10; MU=seq(60, 68, length=81)
power = numeric(length(MU)) 
for ( j in 1:length(MU) ) { 
    mu = MU[j] 
    for ( i in 1:ITER ) { 
        X = rnorm(n=n, mean=mu, sd=sigma)
        if ( mean(X) > 62 ) power[j] = power[j] + 1/ITER
   }
}
# Compare the empirical power with the theoretical power 
curve(K, from=58, to=68, ylim=c(0,1), xname="mu" )
lines(MU, power, col="red")


