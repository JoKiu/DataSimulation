library(survival)
library(survminer)

surv.obj<- Surv(time = data$censored_T,event = data$event,type = 'left')
plot(surv.obj)
summary(surv.obj)
fit1<-survfit(Surv(data$censored_T,data$event)~1)
summary(fit1,time=10)
af<-approxfun(fit1$time,fit1$surv)
# integrate(af,0,1)
plot(af)
fc<-function(x){2*x^2}
integrate(fc,0,1)


po<-pseudo(fit1,times = data$censored_T)
