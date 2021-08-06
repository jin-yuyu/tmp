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
HL1a <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:(n-1) ) {         
        for ( j in (i+1):n ) {            # <------
            yij = c(yij, (x[i]+x[j])/2)   # <------
        }                                 # <------
    }
    return( median(yij) )
}
#
HL1b <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:(n-1) ) {         
        yij = c(yij, (x[i]+x[(i+1):n]))   ### <-------
    }
    return( median(yij)/2 )               # <-----
}
#-----------------------------
HL2a <- function (x) {
    n = length(x)
    yij = NULL 
    for ( i in 1:n)  {                    # <------
        for ( j in i:n ) {                # <------ 
            yij = c(yij, (x[i]+x[j])/2)   # <------
        }
    }
    return( median(yij) )
}
#
HL2b <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:n)  {         
            yij = c(yij, (x[i]+x[i:n]))   ### <-------
    }
    return( median(yij)/2 )               # <-----
}
#-----------------------------
HL3a <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:n)  {         
        for ( j in 1:n ) {                 # <------      
            yij = c(yij, (x[i]+x[j])/2)    # <------
        }                                  # <------
    }
    return( median(yij) )
}
HL3b <- function (x) {
    n = length(x)
    yij = NULL
    for ( i in 1:n)  {         
            yij = c(yij, (x[i]+x[1:n]))   ### <-------
    }
    return( median(yij)/2 )               # <-----
}
#-----------------------------


#===================================================================
# Testing the above functions
set.seed(1)
x = runif(99) 
 HL(x, "HL1")
 HL1a(x)
 HL1b(x)
 
 HL(x, "HL2")
 HL2a(x)
 HL2b(x)

 HL(x, "HL3")
 HL3a(x)
 HL3b(x)


#===================================================================
# Their performance
n = 200
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1a(x) )
system.time( HL1b(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2a(x) )
system.time( HL2b(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3a(x) )
system.time( HL3b(x) )

#===================================================================
# Their performance
n = 500
x = rnorm(n)
# HL1
system.time( HL(x, "HL1") )
system.time( HL1a(x) )
system.time( HL1b(x) )

# HL2
system.time( HL(x, "HL2") )
system.time( HL2a(x) )
system.time( HL2b(x) )

# HL3
system.time( HL(x, "HL3") )
system.time( HL3a(x) )
system.time( HL3b(x) )


