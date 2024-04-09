rm(list=ls())
load("data/abrupt_gradual.Rdata")
library(viridis)
trials<-300
n_par<-24
conditions<-4
#### Plot (data): histograms of errors by condition ####
delta<-matrix(NA,nrow=299*n_par,ncol=conditions)
x<-c()
for(k in 1:conditions){
  x2<-c()
  for(p in 1:n_par){
    for(t in 2:trials){
      x[t-1]<-(ag[t,1,k,p]-ag[t,3,k,p])
    }
    x2<-append(x2,x)
  }
  delta[,k]<-x2
}

col.violin<-viridis_pal(option = "D")(15)[c(3,5,11,1)]

pdf(file="results/error.pdf",width=12)
par(oma=c(2,3,.1,.1))
layout(matrix(seq(1,8),byrow=T,ncol=4))
par(mai=c(.4,.3,.1,.1))
ran<-sample(seq(1,n_par),1)
for(k in 1:conditions){
  plot(seq(1,trials),ag[,2,k,ran],pch=16,col=col.violin[1],axes=F,ann=F,type="n")
  lines(seq(2,trials),ag[2:trials,1,k,ran],col=col.violin[3],lwd=2)
  lines(seq(1,trials),ag[,3,k,ran],col=col.violin[2],lwd=2)
  points(seq(1,trials),ag[,2,k,ran],pch=16,
         col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[1],"aa"),
                  collapse = ""))
  axis(1)
  axis(2,las=2)
  box()
}
par(mai=c(.4,.3,.4,.1))
for(k in 1:conditions){
  par(lwd=1.5) 
  dx<-density(delta[,k])
  sigma<-sqrt(var(delta[,k]))
  mu<-mean(delta[,k])
  plot(dx$x,dx$y/max(dx$y),axes=F,ann=F,lwd=1.3,type="l",col=col.violin[4],
       xlim=c(min(delta,na.rm = T),
              max(delta,na.rm = T)),ylim=c(0,1))
  polygon(c(dx$x,rev(dx$x)),c(dx$y/max(dx$y),rep(0,length(dx$y))),
          col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[4],"bb"),collapse = ""))
  points(delta[,k],rep(-0.018,length(delta[,k])),pch="|",col=col.violin[4],
         cex=0.8)
  text(-2,.8,labels=bquote(paste(mu," = ",.(round(mu,3)))),cex=1.3)
  text(-2,.7,labels=bquote(paste(sigma," = ",.(round(sigma,3)))),cex=1.3)
  axis(1)
  box(lwd=1)
}
mtext("Trials",side=1,line=-24.4,cex=1.8,outer=T)
mtext("Error",side=1,line=0.6,cex=1.8,outer=T)
mtext("Position",side=2,line=0.6,cex=1.8,outer=T,at=0.78)
dev.off()

#### Plot (data): violin errors after change-point ####
changes_a<-sum(ag[,5,2,]==1)
changes_m<-sum(ag[,5,4,]==1)
delta_a<-matrix(NA,nrow=changes_a,ncol=8)
delta_m<-matrix(NA,nrow=changes_m,ncol=8)
t_after<-7
count_ch<-0
error<-c()
for(n in 1:n_par){
  for(ch in which(ag[,5,2,n]!=0)){
    count_ch<-count_ch+1
    count=0
    for(t in ch:(ch+t_after)){
      count=count+1
      error[count]<-sqrt((ag[t,3,2,n]-ag[t,1,2,n])^2)
    }
    delta_a[count_ch,]<-error
  }
}
delta_a[which(delta_a>pi)]<-pi
count_ch<-0
error<-c()
for(n in 1:n_par){
  for(ch in which(ag[,5,4,n]!=0)){
    count_ch<-count_ch+1
    count=0
    for(t in ch:(ch+t_after)){
      count=count+1
      error[count]<-sqrt((ag[t,3,4,n]-ag[t,1,4,n])^2)
    }
    delta_m[count_ch,]<-error
  }
}

col.violin<-viridis_pal(option = "D")(8)
pdf(file="results/error_afterch.pdf",width=12)
par(oma=c(3,3,4,.1))
layout(matrix(seq(1,2),byrow=T,ncol=2))
par(mai=c(.4,.3,.1,.1))
plot(0,0,ann=F,axes=F,type="n",xlim=c(0,8.5),
     ylim=c(min(c(delta_a,delta_m),na.rm=T),pi))
abline(h=0,col="#00000077",lty=2)
for(i in 1:8){
  dx<-density(delta_a[,i])
  lines(i-(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  lines(i+(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  polygon(c(i-(dx$y/max(dx$y)*0.42),rev(i+(dx$y/max(dx$y)*0.42))),c(dx$x,rev(dx$x)),
          col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"33"),collapse=""),
          border=F)
  bx<-boxplot(delta_a[,i],plot=F)
  rect(xleft=i-0.1,xright=i+0.1,ybottom=bx$stats[2,1],ytop=bx$stats[4,1],
       col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"66"),collapse=""),lwd=1)
  lines(c(i-0.1,i+0.1),rep(bx$stats[3,1],2),lwd=1.2)
  lines(c(i,i),c(bx$stats[4,1],bx$stats[5,1]),lwd=1.2)
  lines(c(i,i),c(bx$stats[2,1],bx$stats[1,1]),lwd=1.2)
  axis(1,at=seq(1,15))
  axis(2,las=2,at=c(seq(0,2.5,.5),pi),labels=c(seq(0,2.5,.5),expression(pi)),pos=c(0,0))
}
plot(0,0,ann=F,axes=F,type="n",xlim=c(0,8.5),
     ylim=c(min(c(delta_a,delta_m),na.rm=T),pi))
abline(h=0,col="#00000077",lty=2)
for(i in 1:8){
  dx<-density(delta_m[,i])
  lines(i-(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  lines(i+(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  polygon(c(i-(dx$y/max(dx$y)*0.42),rev(i+(dx$y/max(dx$y)*0.42))),c(dx$x,rev(dx$x)),
          col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"33"),collapse=""),
          border=F)
  bx<-boxplot(delta_m[,i],plot=F)
  rect(xleft=i-0.1,xright=i+0.1,ybottom=bx$stats[2,1],ytop=bx$stats[4,1],
       col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"66"),collapse=""),
       lwd=1)
  lines(c(i-0.1,i+0.1),rep(bx$stats[3,1],2),lwd=1.2)
  lines(c(i,i),c(bx$stats[4,1],bx$stats[5,1]),lwd=1.2)
  lines(c(i,i),c(bx$stats[2,1],bx$stats[1,1]),lwd=1.2)
  axis(1,at=seq(1,8))
  axis(2,las=2,at=c(seq(0,2.5,.5),pi),labels=c(seq(0,2.5,.5),expression(pi)),pos=c(0,0))
}
mtext('Abrupt-Gradual',side=3,outer=T,line=1,cex=1.8,at=.75,col="#000000bb")
mtext('Abrupt',side=3,outer=T,line=1,cex=1.8,at=.25,col="#000000bb")
mtext('Trials after change-point',side=1,outer=T,line=1,cex=1.8)
mtext('Error',side=2,outer=T,line=0.4,cex=1.8)
dev.off()








#### Plot (data): violin reaction time by condition ####
col.violin<-viridis_pal(option = "D")(4)
pdf(file="results/Reactiontimes.pdf",width=12)
par(oma=c(3,3,4,.1),
    mai=c(.4,.3,.1,.1))
plot(0,0,ann=F,axes=F,type="n",xlim=c(0.4,4.4),
     ylim=c(min(ag[,6,,]),max(ag[,6,,])))
abline(h=0,col="#00000077",lty=2)
for(i in 1:length(ag[1,1,,1])){
  dx<-density(ag[,6,i,])
  lines(i-(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  lines(i+(dx$y/max(dx$y)*0.42),dx$x,col=col.violin[i],lwd=1.3)
  polygon(c(i-(dx$y/max(dx$y)*0.42),rev(i+(dx$y/max(dx$y)*0.42))),c(dx$x,rev(dx$x)),
          col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"33"),collapse=""),
          border=F)
  bx<-boxplot(ag[,6,i,],plot=F)
  rect(xleft=i-0.02,xright=i+0.02,ybottom=bx$stats[2,1],ytop=bx$stats[4,1],
       col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[i],"66"),collapse=""),lwd=1)
  lines(c(i-0.02,i+0.02),rep(bx$stats[3,1],2),lwd=1.2)
  lines(c(i,i),c(bx$stats[4,1],bx$stats[5,1]),lwd=1.2)
  lines(c(i,i),c(bx$stats[2,1],bx$stats[1,1]),lwd=1.2)
  axis(1,at=c(1,2,3,4),labels=c("stable",'abrupt','gradual','mixed'),cex.axis=1.3)
  axis(2,las=2)
}
mtext('Condition',side=1,outer=T,line=1,cex=1.8)
mtext('Reaction times',side=2,outer=T,line=0.8,cex=1.8)
dev.off()
#### Plot (models) : abrupt changes and error after change ####
pdf('results/abrupt_models.pdf',width=12)
source('src/Functions.R')
models<-c("data/discrete_learning.Rdata","data/velocity.Rdata","data/PID.Rdata")
name_m<-c('Latent Mixture',"Velocity","PID Controller")
changes_a<-sum(ag[,5,2,]==1)
t_after<-4
delta_a<-matrix(NA,nrow=changes_a,ncol=t_after+1)
count_ch<-0
error<-c()
for(n in 1:n_par){
  for(ch in which(ag[,5,2,n]!=0)){
    count_ch<-count_ch+1
    count=0
    for(t in ch:(ch+t_after)){
      count=count+1
      error[count]<-sqrt((ag[t,3,2,n]-ag[t,1,2,n])^2)
    }
    delta_a[count_ch,]<-error
  }
}
delta_a[which(delta_a>pi)]<-pi
k<-2
col.violin<-viridis_pal(option = "D")(15)[c(8,11,1)]
sim<-100
r_s<-17
delta_am<-array(NA,dim=c(changes_a,t_after+1,sim,3))
sim_sample<-sample(1:2250,sim)
par(oma=c(2,3,.1,.1))
layout(matrix(seq(1,4),byrow=T,ncol=2))
par(mai=c(.4,.3,.1,.1))
for(m in models){
  mm<-which(models==m)
  load(paste(m))
  for(s in 1:sim){
    count_ch<-0
    for(n in 1:length(ag[1,1,1,])){
      for(ch in which(ag[,5,2,n]!=0)){
        count_ch<-count_ch+1
        count=0
        for(t in ch:(ch+t_after)){
          count=count+1
          delta_am[count_ch,count,s,mm]<-sqrt((ag[t,3,2,n]-samples$BUGSoutput$sims.list$pred.y[s,(t-1),2,n])^2)
        }
      }
    }
  }
  mean.95<-matrix(NA,nrow=300,ncol=2)
  for(t in 1:299){
    mean.95[t+1,1:2]<-HPDinterval(as.mcmc(samples$BUGSoutput$sims.list$pred.y[,t,k,r_s]))[1:2]
  }
  plot(ag[,1,k,r_s],type="n",ann=F,axes=F)
  polygon(c(1:300,rev(1:300)),c(mean.95[,1],rev(mean.95[,2])),
          col=col.violin[2],border=F)
  lines(ag[,1,k,r_s],col=col.violin[3],lwd=2.3)
  lines(c(NA,samples$BUGSoutput$mean$pred.y[,2,r_s]),col=col.violin[1],lwd=2.3)
  text(35,5,labels = paste(name_m[mm]),cex=1.6,col="#000000bb")
  box()
  axis(1,cex.axis=1.4)
  if(mm==2){
    axis(2,las=2,labels=F)
  }
  else{
    axis(2,las=2,cex.axis=1.4)
  }
}
col.violin<-viridis_pal(option = "D")(3)
plot(0,0,xlim=c(1,5),ylim=c(min(delta_am),4),ann=F,axes=F,type="n")
axis(2,las=2,pos=c(0.98,0))
abline(v=c(2,3,4),col="#55555588",lty=2)
axis(1)
for(i in 2:5){
  bx<-boxplot(delta_a[,i],plot = F)
  rect(xleft = (i-1),xright = (i-1)+3/13,
       ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
       border="#000000",col="#00000077")
  segments(x0=(i-1),x1=(i-1)+3/13,y0=bx$stats[3,1],y1=bx$stats[3,1])
  if(min(delta_a[,i])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[2,1],col="#000000")
    points(rep((i-1)+.25/2,sum(delta_a[,i]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           delta_a[which(delta_a[,i]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))),i],
           pch=16,col="#000000")
  }
  else{
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[1,1],
             y1=bx$stats[2,1])
  }
  if(max(delta_a[,i])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[4,1],col="#000000")
    points(rep((i-1)+.25/2,sum(delta_a[,i]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           as.vector(delta_a[,i])[which(delta_a[,i]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
           pch="-",col="#000000")
  }
  else{
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[5,1],
             y1=bx$stats[4,1])
  }
  for(m in 1:3){
    bx<-boxplot(delta_am[,i,,m],plot = F)
    rect(xleft = (i-1+.25*m),xright = (i-1+.25*m)+3/13,
         ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
         border=col.violin[m],
         col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[m],"33"),collapse=""))
    segments(x0=(i-1+.25*m),x1=(i-1+.25*m)+3/13,
             y0=bx$stats[3,1],y1=bx$stats[3,1],
             col=col.violin[m])
    if(min(delta_am[,i,,m])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[2,1],col=col.violin[m])
      points(rep((i-1+.25*m)+.25/2,sum(delta_am[,i,,m]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_am[,i,,m])[which(delta_am[,i,,m]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch=16,col=col.violin[m])
    }
    else{
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[1,1],
               y1=bx$stats[2,1],
               col = col.violin[m])
    }
    if(max(delta_am[,i,,m])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[4,1],col=col.violin[m])
      points(rep((i-1+.25*m)+.25/2,sum(delta_am[,i,,m]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_am[,i,,m])[which(delta_am[,i,,m]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch="-",col=col.violin[m])
    }
    else{
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[5,1],
               y1=bx$stats[4,1],
               col=col.violin[m])
    }
  }
}
mtext("Position",side=2,line=1,outer=T,cex=1.7)
mtext("Trials",side=1,line=1,outer=T,cex=1.7,at=.25)
mtext("Trials after changepoint",side=1,line=1,outer=T,cex=1.7,at=.75)
mtext("Error",side=2,line=-35.8,outer=T,cex=1.7,at=.27)
dev.off()
#### Plot (models) : mixed changes and error after change ####
pdf('results/mixed_models.pdf',width=12)
source('src/Functions.R')
models<-c("data/discrete_learning.Rdata","data/velocity.Rdata","data/PID.Rdata")
name_m<-c('Latent Mixture',"Velocity","PID Controller")
changes_a<-sum(ag[,5,4,]==1)
t_after<-4
delta_a<-matrix(NA,nrow=changes_a,ncol=t_after+1)
count_ch<-0
error<-c()
for(n in 1:n_par){
  for(ch in which(ag[,5,4,n]!=0)){
    count_ch<-count_ch+1
    count=0
    for(t in ch:(ch+t_after)){
      count=count+1
      error[count]<-sqrt((ag[t,3,4,n]-ag[t,1,4,n])^2)
    }
    delta_a[count_ch,]<-error
  }
}
delta_a[which(delta_a>pi)]<-pi
k<-4
col.violin<-viridis_pal(option = "D")(15)[c(8,11,1)]
sim<-100
r_s<-17
delta_am<-array(NA,dim=c(changes_a,t_after+1,sim,3))
sim_sample<-sample(1:2250,sim)
par(oma=c(2,3,.1,.1))
layout(matrix(seq(1,4),byrow=T,ncol=2))
par(mai=c(.4,.3,.1,.1))
for(m in models){
  mm<-which(models==m)
  load(paste(m))
  for(s in 1:sim){
    count_ch<-0
    for(n in 1:length(ag[1,1,1,])){
      for(ch in which(ag[,5,4,n]!=0)){
        count_ch<-count_ch+1
        count=0
        for(t in ch:(ch+t_after)){
          count=count+1
          delta_am[count_ch,count,s,mm]<-sqrt((ag[t,3,4,n]-samples$BUGSoutput$sims.list$pred.y[s,(t-1),4,n])^2)
        }
      }
    }
  }
  mean.95<-matrix(NA,nrow=300,ncol=2)
  for(t in 1:299){
    mean.95[t+1,1:2]<-HPDinterval(as.mcmc(samples$BUGSoutput$sims.list$pred.y[,t,k,r_s]))[1:2]
  }
  plot(ag[,1,k,r_s],type="n",ann=F,axes=F)
  polygon(c(1:300,rev(1:300)),c(mean.95[,1],rev(mean.95[,2])),
          col=col.violin[2],border=F)
  lines(ag[,1,k,r_s],col=col.violin[3],lwd=2.3)
  lines(c(NA,samples$BUGSoutput$mean$pred.y[,4,r_s]),col=col.violin[1],lwd=2.3)
  text(35,30,labels = paste(name_m[mm]),cex=1.6,col="#000000bb")
  box()
  axis(1,cex.axis=1.4)
  if(mm==2){
    axis(2,las=2,labels=F)
  }
  else{
    axis(2,las=2,cex.axis=1.4)
  }
}
col.violin<-viridis_pal(option = "D")(3)
plot(0,0,xlim=c(1,5),ylim=c(min(delta_am),4.5),ann=F,axes=F,type="n")
axis(2,las=2,pos=c(0.98,0))
abline(v=c(2,3,4),col="#55555588",lty=2)
axis(1)
for(i in 2:5){
  bx<-boxplot(delta_a[,i],plot = F)
  rect(xleft = (i-1),xright = (i-1)+3/13,
       ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
       border="#000000",col="#00000077")
  segments(x0=(i-1),x1=(i-1)+3/13,y0=bx$stats[3,1],y1=bx$stats[3,1])
  if(min(delta_a[,i])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[2,1],col="#000000")
    points(rep((i-1)+.25/2,sum(delta_a[,i]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           delta_a[which(delta_a[,i]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))),i],
           pch=16,col="#000000")
  }
  else{
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[1,1],
             y1=bx$stats[2,1])
  }
  if(max(delta_a[,i])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[4,1],col="#000000")
    points(rep((i-1)+.25/2,sum(delta_a[,i]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           as.vector(delta_a[,i])[which(delta_a[,i]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
           pch="-",col="#000000")
  }
  else{
    segments(x0=(i-1)+.25/2,
             x1=(i-1)+.25/2,
             y0=bx$stats[5,1],
             y1=bx$stats[4,1])
  }
  for(m in 1:3){
    bx<-boxplot(delta_am[,i,,m],plot = F)
    rect(xleft = (i-1+.25*m),xright = (i-1+.25*m)+3/13,
         ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
         border=col.violin[m],
         col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[m],"33"),collapse=""))
    segments(x0=(i-1+.25*m),x1=(i-1+.25*m)+3/13,
             y0=bx$stats[3,1],y1=bx$stats[3,1],
             col=col.violin[m])
    if(min(delta_am[,i,,m])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[2,1],col=col.violin[m])
      points(rep((i-1+.25*m)+.25/2,sum(delta_am[,i,,m]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_am[,i,,m])[which(delta_am[,i,,m]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch=16,col=col.violin[m])
    }
    else{
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[1,1],
               y1=bx$stats[2,1],
               col = col.violin[m])
    }
    if(max(delta_am[,i,,m])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[4,1],col=col.violin[m])
      points(rep((i-1+.25*m)+.25/2,sum(delta_am[,i,,m]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_am[,i,,m])[which(delta_am[,i,,m]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch="-",col=col.violin[m])
    }
    else{
      segments(x0=(i-1+.25*m)+.25/2,
               x1=(i-1+.25*m)+.25/2,
               y0=bx$stats[5,1],
               y1=bx$stats[4,1],
               col=col.violin[m])
    }
  }
}
mtext("Position",side=2,line=1,outer=T,cex=1.7)
mtext("Trials",side=1,line=1,outer=T,cex=1.7,at=.25)
mtext("Trials after changepoint",side=1,line=1,outer=T,cex=1.7,at=.75)
mtext("Error",side=2,line=-35.8,outer=T,cex=1.7,at=.27)
dev.off()
#### Plot (error) : models vs observations and predictions ####
source('src/Functions.R')
models<-c("data/discrete_learning.Rdata","data/velocity.Rdata","data/PID.Rdata")
name_m<-c('Latent Mixture',"Velocity","PID Controller")
sim<-100
sim_s<-sample(1:2250,sim)
delta_m<-array(NA,dim=c(conditions,n_par,length(models),sim,2))
delta_a<-array(NA,dim=c(conditions,n_par))
for(k in 1:conditions){
  for(n in 1:n_par){
    delta_a[k,n]<-msqe_learning(ag[2:trials,1,k,n],ag[2:trials,3,k,n])
  }
}
for(m in models){
  mm<-which(m==models)
  load(paste(m))
  ss<-0
  for(s in sim_s){
    ss<-ss+1
    for(k in 1:conditions){
      for(n in 1:n_par){
        if(k==2|k==4){
          y<-ag[-c(which(ag[,5,k,n]==1)),3,k,n]
          y<-y[-1]
          x<-ag[-c(which(ag[,5,k,n]==1)),1,k,n]
          x<-x[-1]
          yhat<-samples$BUGSoutput$sims.list$pred.y[s,-c(which(ag[,5,k,n]==1)-1),k,n]
          delta_m[k,n,mm,ss,1]<-msqe_learning(y,yhat)
          delta_m[k,n,mm,ss,2]<-msqe_learning(x,yhat)
          
        }
        else{
          delta_m[k,n,mm,ss,1]<-msqe_learning(ag[2:trials,3,k,n],
                                              samples$BUGSoutput$sims.list$pred.y[s,,k,n])
          delta_m[k,n,mm,ss,2]<-msqe_learning(ag[2:trials,1,k,n],
                                              samples$BUGSoutput$sims.list$pred.y[s,,k,n])
          
        }
      }
    }
  }
}

#measure .22
pdf(file='results/descriptive_ad.pdf')
par(oma=c(1,1.5,.1,.1))
plot(0,0,type="n",xlim=c(1,5),axes=F,ann=F,ylim=c(min(delta_m[,,,,1]),max(delta_m[,,,,1])))
abline(v=c(2,3,4),lty=2,col="#55555588")
r<-0.015
k<-0.2
for(i in 1:conditions){
  bx<-boxplot(delta_a[i,],plot = F)
  rect(xleft =(i)+r,xright = ((i)+r)+k,
       ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
       border="#000000",col="#00000077")
  segments(x0=(i)+r,x1=((i)+r)+k,
           y0=bx$stats[3,1],y1=bx$stats[3,1])
  if(min(delta_a[i,])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i)+r+k/2,
             x1=(i)+r+k/2,
             y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[2,1],col="#000000")
    points(rep((i)+r+k/2,sum(delta_a[i,]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           as.vector(delta_a)[which(delta_a[i,]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))],
           pch=16,col="#000000")
  }
  else{
    segments(x0=(i)+r+k/2,
             x1=(i)+r+k/2,
             y0=bx$stats[1,1],
             y1=bx$stats[2,1])
  }
  if(max(delta_a[i,])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
    segments(x0=(i)+r+k/2,
             x1=(i)+r+k/2,
             y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
             y1=bx$stats[4,1],col="#000000")
    points(rep((i)+r+k/2,sum(delta_a[i,]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
           as.vector(delta_a[i,])[which(delta_a[i,]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
           pch="-",col="#000000")
  }
  else{
    segments(x0=(i)+r+k/2,
             x1=(i)+r+k/2,
             y0=bx$stats[5,1],
             y1=bx$stats[4,1])
  }
  for(m in 1:3){
    bx<-boxplot(delta_m[i,,m,,1],plot = F)
    rect(xleft = ((i)+r+.22*m),xright = ((i)+r+.22*m)+k,
         ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
         border=col.violin[m],
         col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[m],"33"),collapse=""))
    segments(x0=((i)+r+.22*m),x1=((i)+r+.22*m)+k,
             y0=bx$stats[3,1],y1=bx$stats[3,1],
             col=col.violin[m])
    if(min(delta_m[i,,m,,1])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=((i)+r+.22*m)+k/2,
               x1=((i)+r+.22*m)+k/2,
               y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[2,1],col=col.violin[m])
      points(rep(((i)+r+.22*m)+k/2,sum(delta_m[i,,m,,1]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_m[i,,m,,1])[which(delta_m[i,,m,,1]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch='-',col=col.violin[m])
    }
    else{
      segments(x0=((i)+r+.22*m)+k/2,
               x1=((i)+r+.22*m)+k/2,
               y0=bx$stats[1,1],
               y1=bx$stats[2,1],
               col = col.violin[m])
    }
    if(max(delta_m[i,,m,,1])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=((i)+r+.22*m)+k/2,
               x1=((i)+r+.22*m)+k/2,
               y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[4,1],col=col.violin[m])
      points(rep(((i)+r+.22*m)+k/2,sum(delta_m[i,,m,,1]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_m[i,,m,,1])[which(delta_m[i,,m,,1]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch="-",col=col.violin[m])
    }
    else{
      segments(x0=((i)+r+.22*m)+k/2,
               x1=((i)+r+.22*m)+k/2,
               y0=bx$stats[5,1],
               y1=bx$stats[4,1],
               col=col.violin[m])
    }
  }
}
axis(1,at=c(seq(1.5,4.5)),labels=c("stationary",'abrupt',"gradual","mixed"),cex.axis=1.3)
axis(2,las=2)
box()
mtext('RMSE (obs-pred)',side=2,cex=1.8,line=-0.5,outer=T)
mtext('Condition',side=1,cex=1.8,line=-1,outer=T)
dev.off()

pdf(file='results/pred_ad.pdf')
par(oma=c(1,1.5,.1,.1))
plot(0,0,type="n",xlim=c(1,5),axes=F,ann=F,ylim=c(min(delta_m[,,,,2]),max(delta_m[,,,,2])))
r<-0.015
k<-0.2
for(i in 1:conditions){
  for(m in 1:3){
    bx<-boxplot(delta_m[i,,m,,2],plot = F)
    rect(xleft = ((i)+r+.22*(m-1)),xright = ((i)+r+.22*(m-1))+k,
         ybottom = bx$stats[2,1],ytop = bx$stats[4,1],
         border=col.violin[m],
         col=paste(c(substr(col.violin,1,nchar(col.violin)-2)[m],"33"),collapse=""))
    segments(x0=((i)+r+.22*(m-1)),x1=((i)+r+.22*(m-1))+k,
             y0=bx$stats[3,1],y1=bx$stats[3,1],
             col=col.violin[m])
    if(min(delta_m[i,,m,,2])<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=((i)+r+.22*(m-1))+k/2,
               x1=((i)+r+.22*(m-1))+k/2,
               y0=bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[2,1],col=col.violin[m])
      points(rep(((i)+r+.22*(m-1))+k/2,sum(delta_m[i,,m,,2]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_m[i,,m,,2])[which(delta_m[i,,m,,2]<(bx$stats[2,1]-1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch='-',col=col.violin[m])
    }
    else{
      segments(x0=((i)+r+.22*(m-1))+k/2,
               x1=((i)+r+.22*(m-1))+k/2,
               y0=bx$stats[1,1],
               y1=bx$stats[2,1],
               col = col.violin[m])
    }
    if(max(delta_m[i,,m,,2])>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]))){
      segments(x0=((i)+r+.22*(m-1))+k/2,
               x1=((i)+r+.22*(m-1))+k/2,
               y0=bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1]),
               y1=bx$stats[4,1],col=col.violin[m])
      points(rep(((i)+r+.22*(m-1))+k/2,sum(delta_m[i,,m,,2]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))),
             as.vector(delta_m[i,,m,,2])[which(delta_m[i,,m,,2]>(bx$stats[4,1]+1.5*(bx$stats[4,1]-bx$stats[2,1])))],
             pch="-",col=col.violin[m])
    }
    else{
      segments(x0=((i)+r+.22*(m-1))+k/2,
               x1=((i)+r+.22*(m-1))+k/2,
               y0=bx$stats[5,1],
               y1=bx$stats[4,1],
               col=col.violin[m])
    }
  }
}
axis(1,at=c(seq(1.5,4.5)),labels=c("stationary",'abrupt',"gradual","mixed"),cex.axis=1.3)
axis(2,las=2)
abline(v=c(2,3,4),lty=2,col="#55555588")
box()
mtext('RMSE (pred-resp)',side=2,cex=1.8,line=-0.5,outer=T)
mtext('Condition',side=1,cex=1.8,line=-1,outer=T)
dev.off()