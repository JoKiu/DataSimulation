library(survival)

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
exp_rate <- 0.04
alpha <- 0.1

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

#########################################
## constructing pseudo observation survival time function
########################################
get.po<-function(censored_T,event,tau=15){
  stopifnot(length(censored_T)==length(event))#check if length match
  nn<-length(censored_T)
  fit1<-survfit(Surv(censored_T,event)~1)#fit the whole dataset survival fit
  tau = 15 #set maximum followup time
  time_0 <- c(0,fit1$time[which(fit1$time<=tau)])#subset the time below tau
  delta_t <- diff(time_0) #calculating delta t
  n_t <- length(time_0)-1
  delta_surv <- rowMeans(cbind(c(1,fit1$surv[1:(n_t-1)]),fit1$surv[1:n_t]))#calculate delta S(u)
  output<-rep(NA,nn)
  for(i in 1:nn){
    fit2<-survfit(Surv(censored_T[-i],event[-i])~1) #removing one observation i
    time_1 <- c(0,fit2$time[which(fit2$time<=tau)]) #refit
    delta_ti <- diff(time_1) #calculate time gap
    n_ti <- length(time_1)-1 
    delta_surv_i <- rowMeans(cbind(c(1,fit2$surv[1:(n_ti-1)]),fit2$surv[1:n_ti]))#calculate delta Si(u)
    output[i]<-n*sum(delta_surv*delta_t)-(n-1)*sum(delta_surv_i*delta_ti)
  }
  return(output)
}


test.po<-get.po(data$censored_T,data$event)
test.po
hist(test.po,xlim = c(0,30),breaks = 1000)
test.po.df<-data.frame(po=test.po,ct<- data$censored_T)
