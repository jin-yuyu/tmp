# Z ~ N(0,1) will be enough
# Note: location-scale family. Z = (X-mu)/sigma

ITER = 100000 
n = 5

#--------------------------------------------------------------------
HL <- 
function (x, estimator = c("HL1", "HL2", "HL3"), na.rm = FALSE) 
{
    estimator = match.arg(estimator)
    if (na.rm) 
        x <- x[!is.na(x)]
    xx = outer(x, x, "+")
    HL.estimation <-
        switch(estimator, HL1 = 0.5 * median(xx[lower.tri(xx, diag = FALSE)]), 
                          HL2 = 0.5 * median(xx[lower.tri(xx, diag = TRUE)]), 
                          HL3 = 0.5 * median(xx)
              )
    return(HL.estimation)
}
#--------------------------------------------------------------------


MU1 = MU2 = MU3 = numeric(ITER)


for ( i in seq_len(ITER) ) {
    Z = rnorm(n)
    MU1[i] = median(Z)
    MU2[i] = mean(Z)
    MU3[i] = HL(Z)
}

var(MU1)
var(MU2)
var(MU3)



# NOTE: one can use rQCC package
RE(5, "median")
RE(5, "mean")
RE(5, "HL1")



