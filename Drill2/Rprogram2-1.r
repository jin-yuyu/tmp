
# ---------------------------------
# Equal Weights
# ---------------------------------

# 1 (a): Odd number of observations 
fn1 = function(x) 0.2*abs(x-1) + 0.2*abs(x-2) + 0.2*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
curve(fn1, 0,6) 

# 1 (b): Even number of observations 
fn2 = function(x) 0.25*abs(x-1) + 0.25*abs(x-2) + 0.25*abs(x-3) + 0.25*abs(x-4) 
curve(fn2, 0,6) 


# ---------------------------------
# Unequal Weights
# ---------------------------------

# 2 (a): Odd number of observations 
fn3 = function(x) 0.3*abs(x-1) + 0.2*abs(x-2) + 0.1*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
curve(fn3, 0,6) 

# 2 (b) Even number of observations 
fn4 = function(x) 0.25*abs(x-1) + 0.30*abs(x-2) + 0.20*abs(x-3) + 0.25*abs(x-4) 
curve(fn4, 0,6) 



# =============================================
# Using median and weighted.median
# =============================================

# ---------------------------------
# Equal Weights
# ---------------------------------

# 1 (a): Odd number of observations 
median( c(1,2,3,4,5) )

# 1 (b): Even number of observations 
median( c(1,2,3,4) )

# ---------------------------------
# Unequal Weights
# ---------------------------------

# Refer to https://github.com/AppliedStat/SCD/blob/master/SCD.R
source("https://raw.githubusercontent.com/AppliedStat/SCD/master/SCD.R")

# 1 (a): Odd number of observations 
weighted.median( c(1,2,3,4,5), c(0.3, 0.2, 0.1, 0.2, 0.2) )

# 1 (b): Even number of observations 
weighted.median( c(1,2,3,4),   c(0.25, 0.3, 0.2, 0.24) )




