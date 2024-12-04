
#########################
#sample training
#########################
train.i<-sample(rownames(df),4000)
test.i<-dplyr::setdiff(rownames(df),train.i)
(length(train.i)+length(test.i))==nrow(df)
train.df <- df[train.i,]
test.df <- df[test.i,]

##########################
#set parameters
##########################
alpha = 0.1
lambda = 0
cat('check if probs exists: ',exists('probs'),'\n')
if(exists('probs')&&!is.null(probs)){
  tau = quantile(df$time,probs = probs)
}else{
  tau = max(train.df$time)  
}


#########################
#get po
##########################

x <- train.df[,c(2,3,4)]
source('PO_function.R')
y <- get.po(train.df$time,train.df$cens,tau)
x0 <- test.df[with(test.df,cens==1),c(2,3,4)]
y0 <- test.df[with(test.df,cens==1),5]

x0 <- as.matrix(x0)
########################################
## set up training and prediction functions
########################################
library(conformalInference)
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
cat(mean(y0>pred_out$lo),'\n')#lower bound only
