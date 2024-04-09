rm(list=ls())
library(R.matlab)
n_sub<-24
trials<-300
vars<-6
conditions<-4
datos<-{}
# ag array holds the data, the first dimention counts trials, the second variables 
# (1: responses, 2: observations, 3: mean, 4: velocity, 5: change-points and 
# 6: reaction times) the third is condition
# and the fourth one is participants.
ag<-array(NA,dim=c(trials,vars,conditions,n_sub))
for(i in 1:n_sub){
  datos<-readMat(paste(c("data/T_",i,".mat"),collapse = ""))
  datos$block.responses<-rbind(c(NA,NA,NA,NA),datos$block.responses)
  for(k in 1:conditions){
    ag[,1,k,i]<-datos$block.responses[,which(datos$con==k)]
    ag[,2,k,i]<-datos$block.observations[,which(datos$con==k)]
    ag[,3,k,i]<-datos$block.position[,which(datos$con==k)]
    ag[,4,k,i]<-datos$block.velocity[,which(datos$con==k)]
    ag[,5,k,i]<-datos$block.change[,which(datos$con==k)]
    ag[,6,k,i]<-datos$block.react[,which(datos$con==k)]
  }
}
save(ag,file="data/abrupt_gradual.Rdata")
