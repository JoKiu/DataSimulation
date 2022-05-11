exp_rate=0.1
alpha=0.1
tau_list <- c(2,5,10,15,20,22,24,26)
namelist<-c('ld_homo','ld_hetero','hd_homo','hd_hetero')
source('PO_function.R')
library(conformalInference)
output<-matrix(NA,length(tau_list),length(namelist),dimnames = list(tau_list,namelist))

source_lb <- function(dir,tau){
  cat(tau,'\n')
  source(dir, local = TRUE)
  cat(tau,'\n')
  cat(mean(y0>pred_out$lo),'\n')
  return(mean(y0>pred_out$lo))
}

source_get_barS <- function(dir,tau=5,startTag='#from here',endTag='#to here') {
  lines <- scan(dir, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc,local = T)
  get.barSurv(censored_T,event,tau)
}
get.barSurv<- function(censored_T,event,tau){
  cat('check tau in function:',tau,'\n')
  stopifnot(length(censored_T)==length(event))#check if length match
  nn<-length(censored_T)
  fit1<-survfit(Surv(censored_T,event)~1)#fit the whole dataset survival fit
  time_0 <- c(0,fit1$time[which(fit1$time<=tau)])#subset the time below tau
  n_t <- length(time_0)-1
  mean(c(1,fit1$surv[1:(n_t-1)]))
}

#source_get_barS('sources_scripts/ld_homo_po_sour.R')


ld.homo.results<-sapply(tau_list, function(x)source_lb('sources_scripts/ld_homo_po_sour.R',tau=x))
ld.hetero.results<-sapply(tau_list, function(x)source_lb('sources_scripts/ld_hetero_po_sour.R',tau=x))
hd.homo.results<-sapply(tau_list, function(x)source_lb('sources_scripts/hd_homo_po_sour.R',tau=x))
hd.hetero.results<-sapply(tau_list, function(x)source_lb('sources_scripts/hd_hetero_po_sour.R',tau=x))

ld.homo.Sbar<-sapply(tau_list, function(x)source_get_barS('sources_scripts/ld_homo_po_sour.R',tau=x))
ld.hetero.Sbar<-sapply(tau_list, function(x)source_get_barS('sources_scripts/ld_hetero_po_sour.R',tau=x))




