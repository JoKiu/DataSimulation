exp_rate=0.04
alpha=0.1
tau_list <- c(2,5,10,15,22,24,26)
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

ld.homo.results<-sapply(tau_list, function(x)source_lb('sources_scripts/ld_homo_po_sour.R',tau=x))
ld.hetero.results<-sapply(tau_list, function(x)source_lb('sources_scripts/ld_hetero_po_sour.R',tau=x))
hd.homo.results<-sapply(tau_list, function(x)source_lb('sources_scripts/hd_homo_po_sour.R',tau=x))
hd.hetero.results<-sapply(tau_list, function(x)source_lb('sources_scripts/hd_hetero_po_sour.R',tau=x))

