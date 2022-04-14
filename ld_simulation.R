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

########################################
## determine alpha
########################################

gamma=table(data$event)[2]/nrow(data)
alpha = (2*alpha)/gamma




########################################
## preparing parameters for distribution free conformal methods
########################################
x <- data$X1[which(data$event)]
y <- data$C[which(data$event)]
x0<- data_test$X1[which(data_test$event)]
y0<- data_test$C[which(data_test$event)]
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
mean(y0>pred_out$lo&y0<pred_out$up)
