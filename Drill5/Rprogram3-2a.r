
#===================================================
pdf(file="Two-side-tests.pdf", width=5.5, height=4.0)
par(mfrow=c(1,1), mar=c(5, 5, 1, 1), omi=c(0,0,0,0), cex=0.6, mex=0.6)
#---------------------------------------------------
iter = 500
mu0 = 0.5; sigma=1; alpha=0.05 ; n = 5
MU = seq(-1,2, l=81)

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
powerz = Kz(MU, alpha=alpha, mu0=mu0, sigma=sigma, n=n)
powert = Kt(MU, alpha=alpha, mu0=mu0, sigma=sigma, n=n)


#===================================================
# Simulation Approach 
#---------------------------------------------------
nMU = length(MU)

# sigma known
sim.powerz = numeric(nMU)
z.cut = qnorm(1-alpha/2)
for ( j in 1:nMU ) { 
    for ( i in 1:iter ) {
        x = rnorm(n, mean=MU[j], sd=sigma)
        s = sigma     # sigma is known 
        test.stat =  abs((mean(x)-mu0)/(s/sqrt(n)))
        if (test.stat>z.cut) sim.powerz[j] = sim.powerz[j] + 1/iter
    }
}

# sigma unknown
sim.powert = numeric(nMU)
t.cut = qt(1-alpha/2, df=n-1)
for ( j in 1:nMU ) { 
    for ( i in 1:iter ) {
        x = rnorm(n, mean=MU[j], sd=sigma)
        s = sd(x)     # sigma is UNKNOWN
        test.stat =  abs((mean(x)-mu0)/(s/sqrt(n)))
        if (test.stat>t.cut) sim.powert[j] = sim.powert[j] + 1/iter
    }
}

# Plot
 plot(NA,NA, xlim=range(MU), ylim=c(0,1), type="n",
      xlab=expression(mu), ylab="Power Function")
abline(h=alpha, v=mu0, col="bisque")
lines(MU, powert, type="l",  lty=1, lwd=0.5, col="red")
lines(MU, powerz, type="l",  lty=1, lwd=0.5, col="black" )
lines(MU, sim.powerz, lty=2, lwd=1.0, col="black")
lines(MU, sim.powert, lty=2, lwd=1.0, col="red")
legend (0.0,1, legend=c(expression(K[z](mu)), expression(K[t](mu)) ),
        horiz=FALSE, bty="n", lty=c(1,1), col=c("black", "red") )
legend (0.5,1,legend=c("Simulated Power (z-test)","Simulated Power (t-test)"),
        horiz=FALSE, bty="n", lty=c(2,2), lwd=1.0, col=c("black","red") )

