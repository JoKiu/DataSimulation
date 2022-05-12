#Data simulation for sources, need parameters exp_rate
#from here
seed <- 1
########################################
## Parameter
########################################
n <- 3000
n_train <- n / 2
n_calib <- n / 2
n_test <- 3000
p <- 100
beta <- 30 / sqrt(n)
xnames <- paste0("X",1:p) 
c_ref <-1 : 6 / 2
alpha <- 0.1

########################################
## Data generating models
########################################
mu_x <- function(x) beta * x[,1]^2 - beta * x[,3] * x[,5] + 1 
gen_t <- function(x) 2 * exp(mu_x(x) + rnorm(dim(x)[1]))
gen_c <- function(x) rexp(rate = exp_rate, n = dim(x)[1])

########################################
## Generate training data
########################################
set.seed(24601)
X <- matrix(runif(n_train * p, min = -1, max = 1), n_train)
T <- gen_t(X)
C <- gen_c(X) 
event <- (T<C)
censored_T <- pmin(T,C)
data_fit <- data.frame(X, T = T, censored_T = censored_T, event = event)
colnames(data_fit) <- c(xnames, "T", "censored_T", "event")

########################################
## Generate the calibration data and the test data
########################################
set.seed(seed)
X <- matrix(runif((n_calib + n_test) * p, min = -1, max = 1), n_calib + n_test)
T <- gen_t(X)
C <- gen_c(X)
event <- (T<C)
censored_T <- pmin(T,C)
data <- data.frame(X, T = T, censored_T = censored_T,  event = event)
colnames(data) <- c(xnames, "T", "censored_T", "event")
data_calib <- data[1:n_calib,]
data_test <- data[(n_calib+1) : (n_calib+n_test),]
data <- rbind(data_fit,data_calib)

########################################
## determine alpha
########################################

gamma=sum(data$event)/nrow(data)
alpha = (alpha)/gamma
#to here



########################################
## preparing parameters for distribution free conformal methods
########################################
x <- data[which(data$event),xnames]
y <- data$censored_T[which(data$event)]
x0<- as.matrix(data_test[,xnames])
y0<- data_test$T
lambda<-0#ridge regression

########################################
## set up training and prediction functions
########################################
library(conformalInference)
my.lm.funs = lm.funs(lambda=lambda)
my.conf.fun = function(x, y, x0) {
  conformal.pred(x,y,x0,alpha=alpha,verb="\t\t",
                 train.fun=my.lm.funs$train,
                 predict.fun=my.lm.funs$predict)
}
########################################
## train, predict and coverage
########################################
pred_out<-my.conf.fun(x,y,x0)
mean(y0>pred_out$lo)#lower bound only