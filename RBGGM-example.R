## --------------------------------------------- ##
##           One-shot example of RBGGM           ##
## --------------------------------------------- ##

# load the function
source("RBGGM-function.R")
Matmean <- function(x){
  p <- dim(x)[1]
  mc <- dim(x)[3]
  out <- x[,,1]
  for (i in 2:mc) {
    out <- out + x[,,i]
  }
  out <- out/mc
  return(out)
}



# set dimension and sample size
p <- 12;  n <- 200

Omega <- matrix(0, p,p)
diag(Omega) <- rep(1, p)
Omega[1,2] <- Omega[2,1] <- 0.5
for(i in 3:p){
  Omega[i,i-1] <- Omega[i-1,i] <- 0.5
  Omega[i,i-2] <- Omega[i-2,i] <- 0.25
}

set.seed(1)
outvec <- zerovec <- rep(0,p)
th1 <- list(zerovec, solve(Omega))
th2 <- list(outvec, diag(30,p))  ## outlier
Pr <- (runif(n)<0.1)
y <- matrix(0, n, p)
for (i in 1:n) {
  if(Pr[i]){ ## outlier
    y[i,] <- rmvnorm(1, th2[[1]], th2[[2]])
  }else{ ## non-outlier
    y[i,] <- rmvnorm(1, th1[[1]], th1[[2]])
  }
}



## Setting for WBB
mc <- 10000
bn <- 4000
gam <- 0.1
lam <- 0.02



## Fitting of WBB
fit <- RBGGM.function(y, gam=gam, mc=mc-bn, lambda=lam, core=1)

## posterior mean
Post.mean <- Matmean(fit)
