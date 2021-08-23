
#===================================================
pdf(file="ROC-simulation.pdf", width=7.5, height=7.5)
par(mfrow=c(1,1), mar=c(5, 5, 1, 1), omi=c(0,0,0,0) )
#---------------------------------------------------

# Power function :  Assuming sigma=known
Kz = function(mu, alpha, mu0, sigma, n) {
    z.cut = qnorm(1-alpha/2)
    tmp = (mu-mu0)/(sigma/sqrt(n))
    pnorm(z.cut + tmp, lower.tail=FALSE) + pnorm(-z.cut + tmp)
}

# Power function :  Assuming sigma = unknown
Kt = function(mu, alpha, mu0, sigma, n) {
    t.cut = qt(1-alpha/2,df=n-1)
    ncp = (mu-mu0)/(sigma/sqrt(n))
    pt(t.cut,df=n-1,ncp=ncp, lower.tail=FALSE) + pt(-t.cut,df=n-1,ncp=ncp)
}


#===================================================
# Plot: Comparing ROC of z-test and t-test 
# --------------------------------------------------

mu0 = 0.5; mu1 = 1; sigma=1; n = 5
ALPHA=seq(0.01,0.99, by=0.01)

# Theoretical curves
powerz = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=n)
powert = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=n)
 
 plot(ALPHA, powert, xlim=c(0,1),  ylim=c(0,1), type="l", col="red",
      xlab=expression(alpha), ylab="Power", lty=1)
lines(ALPHA, powerz,  col="black", lty=1)

# Simulation approaches
iter = 2000
nALPHA = length(ALPHA)
sim.powerz <- sim.powert <- numeric(nALPHA)

for ( j in 1:nALPHA ) {
    z.cut = qnorm(1-ALPHA[j]/2)
    t.cut = qt(1-ALPHA[j]/2, df=n-1)
    for ( i in 1:iter ) {
        x = rnorm(n, mean=mu1, sd=sigma)
   
        # sigma is known.
        test.stat =  abs((mean(x)-mu0)/(sigma/sqrt(n)))
        if (test.stat>z.cut) sim.powerz[j] = sim.powerz[j] + 1/iter

        # sigma is UNKNOWN
        test.stat =  abs((mean(x)-mu0)/(sd(x)/sqrt(n)))
        if (test.stat>t.cut) sim.powert[j] = sim.powert[j] + 1/iter
    }
}

lines(ALPHA, sim.powerz, lty=2, lwd=1.0, col="black")
lines(ALPHA, sim.powert, lty=2, lwd=1.0, col="red")


# Legends
legend (0, 1.0, 
        legend=c(expression(K[z](alpha)), expression(K[t](alpha)), "Simulated power of z-test", "Simulated power of t-test" ),
        horiz=FALSE, bty="n", lty=c(1,1,2,2), col=c("black", "red", "black", "red") )
legend (0, 0.8, legend=c("n=5"), bty="n")

