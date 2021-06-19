
# Equal Weights
# Odd number of observations 
fn1 = function(x) 0.2*abs(x-1) + 0.2*abs(x-2) + 0.2*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
curve(fn1, 0,6) 

# Odd number of observations 
fn2 = function(x) 0.25*abs(x-1) + 0.25*abs(x-2) + 0.25*abs(x-3) + 0.25*abs(x-4) 
curve(fn2, 0,6) 


# Unequal Weights
# Even number of observations 
fn3 = function(x) 0.3*abs(x-1) + 0.2*abs(x-2) + 0.1*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
curve(fn3, 0,6) 

# Even number of observations 
fn4 = function(x) 0.25*abs(x-1) + 0.30*abs(x-2) + 0.20*abs(x-3) + 0.25*abs(x-4) 
curve(fn4, 0,6) 


