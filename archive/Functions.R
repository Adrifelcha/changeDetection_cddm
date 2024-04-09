msqe_learning<-function(y,y_hat){
  error<- y-y_hat
  msqe<-1/length(error)*sqrt((t(error)%*%error))
  return(as.numeric(msqe))
}
