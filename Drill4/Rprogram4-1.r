
# =====================================
# Example 8.5-2 (sigma: known)
# -------------------------------------
# Theoretical power 
K = function(mu) { 1-pnorm( (62-mu)/2 ) } 

# Empirical power (through simulation)
set.seed(1); 
ITER = 10000
n=25; sigma=10; mu=65
count = 0
for ( i in 1:ITER ) { 
    X = rnorm(n=n, mean=mu, sd=sigma)
    # count if it is in a critical region.
    if (mean(X) > 62) count = count+1  
}
c( count / ITER, K(65) )
