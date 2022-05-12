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
beta <- 20 / sqrt(n)

c_ref <- 1 : 6 / 2
xmin <- 0; xmax <- 4
sigma_x <- function(x) (5 + x)/5
pr_all_list <- matrix(0, n + n_test, length(c_ref))
alpha<- 0.1

########################################
## Data generating models
########################################
gen_t <- function(x) exp(2 + beta * sqrt(abs(x)) +  sigma_x(x) * rnorm(length(x))) 
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
data_fit <- data.frame(X1 = X, T = T, censored_T = censored_T, event = event)

########################################
## Generate the calibration data and the test data
########################################
set.seed(seed)
X <- runif(n_calib + n_test, xmin, xmax)     
T <- gen_t(X) 
C <- gen_c(X)
event <- (T < C)
censored_T <- pmin(T, C)
data <- data.frame(X1 = X, T = T, event = event, censored_T = censored_T)
data_calib <- data[1 : n_calib, ]
data_test <- data[(n_calib + 1) : (n_calib + n_test),]
data <- rbind(data_fit, data_calib)

########################################
## determine alpha
########################################

gamma=sum(data$event)/nrow(data)
alpha = (alpha)/gamma

#to here


########################################
## preparing parameters for distribution free conformal methods
########################################
x <- data$X1[which(data$event)]
y <- data$censored_T[which(data$event)]
x0<- data_test$X1
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
