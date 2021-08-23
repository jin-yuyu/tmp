
#===================================================
pdf(file="ROC-curve.pdf", width=7.5, height=2.5)
par(mfrow=c(1,3), mar=c(5, 5, 1, 1), omi=c(0,0,0,0), cex=0.6, mex=0.6)
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
mu0 = 0.5; mu1 = 1; sigma=1; n = 5

ALPHA=seq(0.01,0.99, by=0.01)

powerz = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=n)
powert = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=n)
 
 plot(ALPHA, powert, xlim=c(0,1),  ylim=c(0,1), type="l", col="red",
      xlab=expression(alpha), ylab="Power")
lines(ALPHA, powerz,  col="black")

legend (0,1.0, legend=c(expression(K[z](alpha)), expression(K[t](alpha)) ),
        horiz=FALSE, bty="n", lty=c(1,1), col=c("black", "red") )

legend (0.6, 0.5, legend=c("n=5"), bty="n")

#===================================================
# Plot: Comparing ROC of z-tests with different n
mu0 = 0.5; mu1 = 1; sigma=1;

ALPHA=seq(0.01,0.99, by=0.01)

powerz5  = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powerz10 = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=10)
powerz20 = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=20)
powerz30 = Kz(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=30)

 plot(ALPHA, powerz5, xlim=c(0,1),  ylim=c(0,1), type="l", col="black",
      xlab=expression(alpha), ylab="Power")
lines(ALPHA, powerz10,  lty=2)
lines(ALPHA, powerz20,  lty=3)
lines(ALPHA, powerz30,  lty=4)
legend (0.6, 0.5, legend=c("n=5", "n=10", "n=20", "n=30"), 
        horiz=FALSE, bty="n", lty=1:4, lwd=1.0 )

# Let's add t-tests  with red color
powert5  = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powert10 = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=10)
powert20 = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=20)
powert30 = Kt(mu1, alpha=ALPHA, mu0=mu0, sigma=sigma, n=30)

lines(ALPHA, powert5,   lty=1, col="red")
lines(ALPHA, powert10,  lty=2, col="red")
lines(ALPHA, powert20,  lty=3, col="red")
lines(ALPHA, powert30,  lty=4, col="red")

#===================================================
# Plot: Comparing ROC of z-tests with different mu
mu0 = 0.5; sigma=1; n=5

ALPHA=seq(0.01,0.99, by=0.01)

powerz0.5 = Kz(mu=0.5, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powerz1.0 = Kz(mu=1.0, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powerz1.5 = Kz(mu=1.5, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powerz2.0 = Kz(mu=2.0, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)

 plot(ALPHA, powerz0.5, xlim=c(0,1),  ylim=c(0,1), type="l", col="black",
      xlab=expression(alpha), ylab="Power")
lines(ALPHA, powerz1.0,  lty=2)
lines(ALPHA, powerz1.5,  lty=3)
lines(ALPHA, powerz2.0,  lty=4)
legend (0.6, 0.5, 
        legend=c(expression(mu==0.5), expression(mu==1.0), expression(mu==1.5), expression(mu==2.0)), 
        horiz=FALSE, bty="n", lty=1:4, lwd=1.0 )

# Let's add t-tests with red color
powert0.5 = Kt(mu=0.5, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powert1.0 = Kt(mu=1.0, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powert1.5 = Kt(mu=1.5, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)
powert2.0 = Kt(mu=2.0, alpha=ALPHA, mu0=mu0, sigma=sigma, n=5)

lines(ALPHA, powert0.5,  lty=1, col="red")
lines(ALPHA, powert1.0,  lty=2, col="red")
lines(ALPHA, powert1.5,  lty=3, col="red")
lines(ALPHA, powert2.0,  lty=4, col="red")

# Check with a straignt line with slope one.
abline(a=0, b=1, col="cyan")







