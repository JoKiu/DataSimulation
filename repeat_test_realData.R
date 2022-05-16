df<-read.csv('naccci.csv')
rownames(df)<-df$X
df<-df[-1]
dim(df)
#########################

get.lowBounds<-function(dir,seed=1,N=10){
  output<-rep(NA,N)
  set.seed(seed)
  for(i in 1:N){
  source(dir,local = T)
  output[i]<-mean(y0>pred_out$lo)
  }
  return(output)
}
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
