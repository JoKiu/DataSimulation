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
#exp_rate <- 0.4
exp_rate<-0.04#also try 0.025 and 0.01
alpha <- 0.1
tau = 5
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
data_test <- data[(n_calib + 1) : (n_calib + n_test), ]
data <- rbind(data_fit, data_calib)

########################################
## determine alpha
########################################

# gamma=sum(data$event)/nrow(data)
# alpha = (2*alpha)/gamma
# 
# alpha


########################################
## getting poi and yi
########################################
source('PO_function.R')
x <- data$X1
po <- get.po(censored_T = data$censored_T,event = data$event,tau)
y<- data$censored_T

########################################
## plot poi and yi with only uncensored
########################################
plot(y[which(data$event)],po[which(data$event)])

