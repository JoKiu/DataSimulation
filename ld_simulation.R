#Data simulation
#
rm(list=ls())
seed <- 1

########################################
## Parameter
########################################
n <- 3000
n_train <- n / 2
n_calib <- n / 2
n_test <- 3000
beta <- 20 / sqrt(n)

xmin <- 0; xmax <- 4
exp_rate <- 0.4
alpha <- 0.05

########################################
## Data generating models
########################################
gen_t <- function(x) exp(2 + beta * sqrt(abs(x)) +  1.5 * rnorm(length(x))) 
gen_c <- function(x) rexp(rate = exp_rate, n = length(x)) 

########################################
## Generate training data
########################################
set.seed(24601)
X <- runif(n_train, xmin, xmax)
T <- gen_t(X)
C <- gen_c(X)
event <- (T < C)
censored_T <- pmin(T, C)
data_fit <- data.frame(X1 = X, C = C, censored_T = censored_T, event = event)

########################################
## Generate the calibration data and the test data
########################################
set.seed(seed)
X <- runif(n_calib + n_test, xmin, xmax)     
T <- gen_t(X) 
C <- gen_c(X)
event <- (T < C)
censored_T <- pmin(T, C)
data <- data.frame(X1 = X, C = C, event = event, censored_T = censored_T)
data_calib <- data[1 : n_calib, ]
data_test <- data[(n_calib + 1) : (n_calib + n_test), ]
data <- rbind(data_fit, data_calib)

data <- rbind(data_fit,data_calib)
########################################
## determine alpha
########################################
gamma=table(data_fit$event)[1]/nrow(data_fit)
alpha = alpha/gamma

########################################
## set up x and y
########################################
x <- data_fit$X1[which(data_fit$event==F)]
y <- data_fit$C[which(data_fit$event==F)]
########################################
## implement distribution tree: full conformal inference
########################################
out = train.fun(x,y) 
fit = matrix(predict.fun(out,x),nrow=n)
pred = matrix(predict.fun(out,x0),nrow=n0)
m = ncol(pred)

# Trial values for y, empty lo, up matrices to fill
ymax = max(abs(y))
yvals = seq(-grid.factor*ymax, grid.factor*ymax,length=num.grid.pts)
lo = up = matrix(0,n0,m)
qvals = rvals = matrix(0,num.grid.pts,m)
xx = rbind(x,rep(0,p))

for (i in 1:n0) {
  if (verbose) {
    cat(sprintf("\r%sProcessing prediction point %i (of %i) ...",txt,i,n0))
    flush.console()
  }
  
  xx[n+1,] = x0[i,]
  ww = c(w[1:n],w[n+i])
  
  # Refit for each point in yvals, compute conformal p-value
  for (j in 1:num.grid.pts) {
    yy = c(y,yvals[j])
    if (j==1) out = train.fun(xx,yy)
    else out = train.fun(xx,yy,out)
    r = abs(yy - matrix(predict.fun(out,xx),nrow=n+1))
    
    # Local scoring?
    if (!is.null(mad.train.fun) && !is.null(mad.predict.fun)) {
      for (l in 1:m) {
        if (j==1 && l==1) out.mad = mad.train.fun(xx,r[,l])
        else out.mad = mad.train.fun(xx,r[,l],out.mad)
        r[,l] = r[,l] / mad.predict.fun(out.mad,xx)
      }
    }
    
    qvals[j,] = apply(r,2,weighted.quantile,prob=1-alpha,w=ww)
    rvals[j,] = r[n+1,]
  }
  
  for (l in 1:m) {
    int = grid.interval(yvals,rvals[,l],qvals[,l])
    lo[i,l] = int$lo
    up[i,l] = int$up
  }
}
if (verbose) cat("\n")

return(list(pred=pred,lo=lo,up=up,fit=fit))
