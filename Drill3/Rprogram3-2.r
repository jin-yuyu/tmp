# How to program HL estimator
#===================================================================
HL <- function (x, estimator=c("HL1", "HL2", "HL3"), na.rm=FALSE) {
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
#===================================================================
HL1 <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:(n-1) ) {         ### <----------
        for ( j in (i+1):n ) {     ### <----------
            yij = c(yij, (x[i]+x[j])/2)
        }
    }
    return( median(yij) )
}
#-----------------------------
HL2 <- function (x) {
    n = length(x)
    yij = NULL 
    for ( i in 1:n)  {         ### <----------
        for ( j in i:n ) {     ### <----------
            yij = c(yij, (x[i]+x[j])/2)
        }
    }
    return( median(yij) )
}
#-----------------------------
HL3 <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:n)  {         ### <----------
        for ( j in 1:n ) {     ### <----------
            yij = c(yij, (x[i]+x[j])/2)
        }
    }
    return( median(yij) )
}
#-----------------------------


#===================================================================
# Testing the above functions
set.seed(1)
x = runif(99) 
 HL(x, "HL1")
 HL1(x)
 
 HL(x, "HL2")
 HL2(x)

 HL(x, "HL3")
 HL3(x)

#===================================================================
# Their performance
n = 10
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3(x) )


#===================================================================
# Their performance
n = 50
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3(x) )

#===================================================================
# Their performance
n = 100
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3(x) )

#===================================================================
# Their performance
n = 500
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3(x) )




