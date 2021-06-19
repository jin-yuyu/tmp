# ========================
# 1. Variance
# ========================

# 2 (a) 
  ITER = 100000
  n = 5
  mu = 10; sigma=10

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rnorm(n, mu, sigma)
      SUM[i] = sum((Xi-mu)^2) 
  }
  mean(SUM) / sigma^2


# 2 (b) 
  ITER = 100000
  n = 5
  mu = 10; sigma=10

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rnorm(n, mu, sigma)
      Xbar = mean(Xi)               #<- different 
      SUM[i] =  sum((Xi-Xbar)^2)    #<- different
  }
  mean(SUM) / sigma^2


# 3 (a) 
  ITER = 100000
  n = 5
  mu = 1; sigma=mu     # It works even with different mu

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rexp(n, rate=1/mu)       #<- different
      SUM[i] = sum((Xi-mu)^2) 
  }
  mean(SUM) / sigma^2


# 3 (b) 
  ITER = 100000
  n = 5
  mu = 1; sigma=mu     # It works even with different mu

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rexp(n, rate=1/mu)       #<- different
      Xbar = mean(Xi)               #<- different 
      SUM[i] =  sum((Xi-Xbar)^2)    #<- different
  }
  mean(SUM) / sigma^2






