# This is the function for generating pseudo observation for survival time,
# expected parameter are censored survival time and censored status.
library(survival)
get.po<-function(censored_T,event,tau){
  cat('check tau in function:',tau,'\n')
  stopifnot(length(censored_T)==length(event))#check if length match
  nn<-length(censored_T)
  fit1<-survfit(Surv(censored_T,event)~1)#fit the whole dataset survival fit
  time_0 <- c(0,fit1$time[which(fit1$time<=tau)])#subset the time below tau
  delta_t <- diff(time_0) #calculating delta t
  n_t <- length(time_0)-1
  delta_surv <- rowMeans(cbind(c(1,fit1$surv[1:(n_t-1)]),fit1$surv[1:n_t]))#calculate delta S(u)
  output<-rep(NA,nn)
  cat(sum(delta_surv*delta_t))
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
