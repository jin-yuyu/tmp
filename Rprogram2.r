# ========================
# 2. Standard Deviation
# ========================

# 3 
  ITER = 100000
  n = 5
  mu =0; sigma=1   ## It works even with different mu and sigma

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rnorm(n, mu, sigma)
      SUM[i] = sd(Xi)
  }
  mean(SUM) / sigma

# ========================
# Using an original gamma
# ------------------------
c4A = function(n) sqrt(2/(n-1))*gamma(n/2)/gamma(n/2-1/2)

# ========================
# Using a log-gamma 
# ------------------------
c4B = function(n) {
     tmp = lgamma(n/2) - lgamma(n/2-1/2)
     sqrt(2/(n-1)) * exp(tmp)
}

# ========================
# Using a simulation 
# ------------------------
c4C = function(n, iter=1000) {
      m1 = 0
      for ( i in 1:iter ) {
          m1 = m1 + sd(rnorm(n)) / iter
      }
      return(m1)
}
# ------------------------

# double-check
c4A(n)
c4B(n)
c4C(n)


# 4 
  ITER = 100000
  n = 5
  mu = 1; sigma=mu

  # NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

  SUM = numeric(ITER)
  for ( i in seq_len(ITER) ) {
      Xi = rexp(n, rate=1/mu)
      SUM[i] = sd(Xi)
  }
  mean(SUM) / sigma

