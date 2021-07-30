
#=======================================================================
# PLOT 
#-----------------------------------------------------------------------
pdf(file="Figure-8-5-2.pdf", width=5.0, height=4.0)
par(mfrow=c(1,1), mar=c(5, 5, 1, 1), omi=c(0,0,0,0), cex=0.6, mex=0.6)
#-----------------------------------------------------------------------
#
# Theoretical power 
K = function(mu) { 1-pnorm( (62-mu)/2 ) }
# 
# Empirical power (through simulation)
set.seed(1)
ITER = 500
n=25; sigma=10; MU=seq(60, 68, length=81)
power = numeric(length(MU))
for ( j in 1:length(MU) ) {
    mu = MU[j]
    for ( i in 1:ITER ) {
        X = rnorm(n=n, mean=mu, sd=sigma)
        if ( mean(X) >= 62 ) power[j] = power[j] + 1/ITER
   }
}
# Compare the empirical power with the theoretical power 
plot(MU, K(MU), xlim=c(58,68), ylim=c(0,1),  type="l",
     xlab=expression(mu), ylab="Power Function")
lines(MU, power, lwd=0.5, col="red")
abline(h=1, col="bisque", lty=1)
legend (58, 1, legend=c(expression(K(mu)), "Simulated Power (z-test)"  ),
        horiz=FALSE, bty="n", lty=c(1,1), col=c("black", "red") )
dev.off()


