###############
##library
###############
library(survival)
library(conformalInference)


###############
##set up tau, exp_rate, alpha
###############
exp_rate=0.04
alpha=0.2
tau_list <- c(2)

###############
##creating output table(unfinished)
###############
namelist<-c('ld_homo','ld_hetero','hd_homo','hd_hetero')
output<-matrix(NA,length(tau_list),length(namelist),dimnames = list(tau_list,namelist))

###############
##creating funcitons
###############
  #get.po is to get pseudo observation with given cenceored time, event and tau
  get.po<-function(censored_T,event,tau){
    cat('check in function:',tau)
    stopifnot(length(censored_T)==length(event))#check if length match
    nn<-length(censored_T)
    fit1<-survfit(Surv(censored_T,event)~1)#fit the whole dataset survival fit
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
      output[i]<-nn*sum(delta_surv*delta_t)-(nn-1)*sum(delta_surv_i*delta_ti)
    }
    return(output)
  }
  #get.lb is to get P(T>L')  with given tau,
  #this function contains the whole low dimension, homo, simulation with lei method,
  #but only return P(data_test$T>L')
  get.lb.pr <- function(tau){
    cat(tau,'\n')
    # source(dir, local = TRUE)
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
    ## preparing parameters for distribution free conformal methods
    ########################################
    x <- data$X1
    y <- get.po(censored_T = data$censored_T,event = data$event,tau)
    x0<- data_test$X1
    y0<- data_test$T
    lambda<-0#ridge regression
    
    ########################################
    ## set up training and prediction functions
    ########################################
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
    cat(tau,'\n')
    cat(mean(y0>pred_out$lo),'\n')
    return(mean(y0>pred_out$lo))
  }

###############
##a sapply() that return a list of P(T>L') with a list of tau value, 
##only for low dimension, homo scenario with lei function
###############
ld.results<-sapply(tau_list, function(x)get.lb.pr(tau=x))
ld.results