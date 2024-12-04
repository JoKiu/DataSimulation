# This is the script that contains function that source the script
# and repeat the sample/validation to get a series of coverage rate.
# the N is the repeat time, default N = 10.
# probs in get.lowBounds.po is to set tau value, when it is NULL, tau
# is the max number in T in the training set. 

#########################
#Load the real data
#########################
df<-read.csv('naccci.csv')
rownames(df)<-df$X
df<-df[-1]
dim(df)
#########################

#dir is the source file to repeat the simulation N times with given seed, the output is a table of
#coverage in every single run.
get.lowBounds<-function(dir,seed=1,N=10){
  output<-rep(NA,N)
  set.seed(seed)
  for(i in 1:N){
  source(dir,local = T)
  output[i]<-mean(y0>pred_out$lo)
  }
  return(output)
}

#this function is modified to adapt changing tau in po methods, if probs is null, the tau is the maximum
#time in training dataset; if probs is given and should be with [0,1], the tau is 
#probs*max(T in the whole data), the output table contain an extra column telling the tau value.
get.lowBounds.po<-function(dir,seed=1,N=10,probs=NULL){
  output<-data.frame(tau=rep(NA,N),lbs=rep(NA,N))
  probs=probs
  set.seed(seed)
  for(i in 1:N){
    source(dir,local = T)
    output[i,2]<-mean(y0>pred_out$lo)
    output[i,1]<-tau
  }
  return(output)
}
uc_lbs<-get.lowBounds('sources_scripts/uc_realData_sour.R')
po_lbs<-get.lowBounds.po('sources_scripts/po_realData_sour.R')
po_lbs0.9<-get.lowBounds.po('sources_scripts/po_realData_sour.R',probs = 0.9)
po_lbs0.8<-get.lowBounds.po('sources_scripts/po_realData_sour.R',probs = 0.8)
