# Author: Daniel A.M. Villela
# Implements the Moschopoulos method for computing the distribution
# of a random variable given by the sum of independent variables
# described by gamma distributions
# Moschopoulos, P. G. (1985). The distribution of the sum of independent gamma random variables. Annals of the Institute of Statistical Mathematics, 37(1), 541-544.


# Computes gamma_k value used in the formula
gammak <- function (a,b,k) {
      b1 <- min(b)
      lga <- log(a)+ k*log(1-b1/b) - log(k)
      return (sum(exp(lga)))
}

# implements the Moschopolous method for finding the probability density
# y: the input value
# a: shape
# b: scale
sum_gamma_dist <- function (y, a, b, K=100){
      b1 <- min(b)
      rho <- sum(a)
      C <- exp(sum(a*log(b1/b)))
      
      delta <- gammav <- c(1,rep(0,K))
      
      gammav <- sapply(1:K, function(x) {gammak(a,b,x)})
      #for (k in 1:K) {
      #  gammav[k] <- gammak(a,b,k)
      #}
      
      delta[1] <- 1
      
      for (k in 0:(K-1)) {
            ss2 <- (1:(k+1))*gammav[1:(k+1)]*delta[1+k+1 - (1:(k+1))]
            delta[(k+1)+1] <- sum(ss2)/(k+1)
      }
      
      xx <- log(delta) + 
            (rho-1 + 1:(K+1))*log(y) -(y/b1) - lgamma(rho+1:(K+1)) - 
            (rho + 1:(K+1))*log(b1)
      gy <- C*sum(exp(xx)) 
      gy
}

# Function to compute the probability distribution, 
# given by \int_0^{y} f(x) dx
# y: the input value
# a: shape
# b: scale
int_sum_gamma <- function(y, a, b, K=200, step=.1, max=50) {
      summ <- 0
      #step <- 0.1
      res <- rep(0, max/step)
      for (i in (1:(max/step))*step) {
            xx <- sum_gamma_dist(i,a,b, K)*step
            if (is.na(xx)) {
                  cat("NA i a b K", i,a,b,K)
            }
            if (is.infinite(xx)) {
                  cat("infinite i a b K", i,a,b,K)
            }
            if (is.nan(xx)) {
                  cat("nan i a b K", i,a,b,K)
            }
            #print(xx)
            summ <- summ + xx
            res[round(i/step)] <- summ
      }
      list(yres=res[round(y/step)], dist=res, i=(1:(max/step))*step)
}


# inverse function given a probability distribution, i.e.,
# given the probability, returns the random variable value
# OBS: it performs a binary search
# p: the probability value
# a: shape
# b: scale
t_sum_gamma_v3 <- function(p, a, b, K=200, step=.1, max=100, res=.1) {  
      xa <- 0
      xb <- max
      while ((xb-xa)>res) {
            y <- (xa+xb)/2.0
            px <- int_sum_gamma(y, a, b, max=max)    
            
            if (px$yres>p) {xb <- y}
            else {xa <- y}
      }
      y
}

# It is better to rename the function
t_sum_gamma <- function(p, a, b, K=200, step=.1, max=100, res=.1) {  
      t_sum_gamma_v3(p, a, b, K, step, max, res)
}

# inverse function given a probability distribution, i.e.,
# given the probability, returns the random variable value
# OBS: not computationally efficient
# p: the probability value
# a: shape
# b: scale
t_sum_gamma_v2 <- function(p, a, b, K=100, step=.1, max=40) {  
      inc <- .1
      y <- 0
      px <- 0
      while (px<p) {
            y <- y+inc
            px <- int_sum_gamma(y, a, b)    
      }
      (y-inc)
}

# Again not computationally efficient
t_sum_gamma_max <- function(p, a, b, K=100, step=.1, max=40) {  
      rx <- (0:(max*10))/10.0
      ggy2 <- sapply(rx, 
                     function(x) { int_sum_gamma(x, a, b, K=K, step=step)})
      rt <- which(ggy2>p)
      tequiv <- rx[rt[1]]
      tequiv
}


# Function to compute the probability distribution, 
# given by \int_0^{y} f(x) dx
# and using time series of temperature values
# y: the input value
# a: shape
# b: scale
# Temp: time series of temperature values
# t: time when Temp[t+1] start
# Temp has to have length = length(t) + 1
# Temp: 27   21   23
# t:         4    5  
# The idea is to have
# P( X <= t) = P(X > t_i) * P(X > t | X> t_i),
# where t_i is the instant in which temperature has changed previous to 
# time t
# Then, P(X>t | X > t_i) has a time rescaling
# P(X>t | X > t_i) = 1 - \int_{t_{i}}^{t} f(\tau + t_{equiv} - t_{i}, \mathbf{\theta}, \mathbf{\beta(Temp_i)})/P(X > t_i)
# The procedure can continue iteratively
int_sum_gamma_T <- function(y, a, b, Temp, t, K=200, step=.1, max=50, withbreak=TRUE, unitscale=1) {
      summ <- 0
      c <- 1
      b[2] <- 1/lambdaEIP(T=Temp[c])
      tdiff <- 0
      res <- rep(0, max/step)
      pdf <- rep(0, max+1)
      for (i in (1:(max/step))*step) {
            # cat("i ", i, "summ", summ, "\n")
            if ((withbreak) && (summ>0.999)) {
                  j <- ceiling(i)
                  pdf[j+1] <- pdf[j+1] + 0.0
                  res[round(i/step)] <- summ        
            }
            else {
                  b[2] <- 1/lambdaEIP(T=Temp[c])
                  if (c<=length(t)) {
                        if (i>t[c]) {
                              c <- c+1
                              b[2] <- 1/lambdaEIP(T=Temp[c])  
                              tsum <- t_sum_gamma_v3(summ, a, b/unitscale, max=max)
                              tdiff <- tsum - i
                        }
                  }    
                  #    xx <- sum_gamma_dist(max(i+tdiff, 0),a,b, K)*(step*unitscale)
                  xx <- sum_gamma_dist(max(i+tdiff, 0),a,b/unitscale, K)*(step)
                  
                  summ <- summ + xx
                  #    j <- ceiling(i/unitscale)
                  j <- ceiling(i)
                  pdf[j+1] <- pdf[j+1] + xx
                  res[round(i/step)] <- summ  
            }
      }
      list(yres=res[y/step], dist=res, i=(1:(max/step))*step, pdf = pdf)
      #summ
}


# Example
# using three gamma distributions
#a <- c(1.4, 2.3, 1.4)
#b <- c(.5, .3, 2)
#rr <- (0:10000)/100
#step <- 0.01
#ggy2 <- sapply(rr, function(x) { sum_gamma_dist(x, a, b)*step})
#plot(rr, ggy2)
