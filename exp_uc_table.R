explist=c(0.04, 0.06, 0.08, 0.1)
#alpha=0.1 by this version, alpha is fixed to 0.1 in source files
namelist<-c('ld_homo','ld_hetero','hd_homo','hd_hetero')
library(conformalInference)
output<-matrix(NA,length(explist),length(namelist),dimnames = list(explist,namelist))

source_lb <- function(dir,exp_rate){
  cat(exp_rate,'\n')
  source(dir, local = TRUE)
  cat(exp_rate,'\n')
  cat(mean(y0>pred_out$lo),'\n')
  return(mean(y0>pred_out$lo))
}

#this is a function to return gamma value
source_get_gamma <- function(dir,exp_rate,startTag='#from here',endTag='#to here') {
  lines <- scan(dir, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc,local = T)
  return(gamma)
}
#this is a function to return alpha value
source_get_alpha <- function(dir,exp_rate,startTag='#from here',endTag='#to here') {
  lines <- scan(dir, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc,local = T)
  return(alpha)
}

#source_get_barS('sources_scripts/ld_homo_po_sour.R')


ld.homo.results<-sapply(explist, function(x)source_lb('sources_scripts/ld_homo_uc_sour.R',exp_rate=x))
ld.hetero.results<-sapply(explist, function(x)source_lb('sources_scripts/ld_hetero_uc_sour.R',exp_rate=x))
hd.homo.results<-sapply(explist, function(x)source_lb('sources_scripts/hd_homo_uc_sour.R',exp_rate =x))
hd.hetero.results<-sapply(explist, function(x)source_lb('sources_scripts/hd_hetero_uc_sour.R',exp_rate =x))

# ld.homo.Sbar<-sapply(tau_list, function(x)source_get_barS('sources_scripts/ld_homo_po_sour.R',tau=x))
# ld.hetero.Sbar<-sapply(tau_list, function(x)source_get_barS('sources_scripts/ld_hetero_po_sour.R',tau=x))

ld.homo.gamma<-sapply(explist, function(x)source_get_gamma('sources_scripts/ld_homo_uc_sour.R',exp_rate =x))

ld.hetero.gamma<-sapply(explist, function(x)source_get_gamma('sources_scripts/ld_hetero_uc_sour.R',exp_rate =x))

hd.homo.gamma<-sapply(explist, function(x)source_get_gamma('sources_scripts/hd_homo_uc_sour.R',exp_rate =x))

hd.hetero.gamma<-sapply(explist, function(x)source_get_gamma('sources_scripts/hd_hetero_uc_sour.R',exp_rate =x))
