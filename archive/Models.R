rm(list=ls())
load("data/abrupt_gradual.Rdata")
library(R2jags)
datos_jags<-list("ag")
#### Discrete Learning ####
write('model{
      sigma[1]~dgamma(0.1,0.1)
      sigma[2]~dgamma(0.1,0.1)
      sigma[3]~dgamma(0.1,0.1)
      sigma[4]~dgamma(0.1,0.1)
      
      for(n in 1:length(ag[1,1,1,])){
      alpha[1,n]~dunif(0,1)
      alpha[2,n]~dunif(0,1)
      alphas[1:2,n]=sort(alpha[,n])
      
      beta[1,n]~dunif(0,1)
      beta[2,n]~dunif(0,1)
      betas[1:2,n]=sort(beta[,n])

        for(k in 1:length(ag[1,1,,1])){
          phi[k,n]~dunif(0,1)
      
          psi[k,n]~dunif(0,1)

          M[1,k,n]<-ag[1,2,k,n]
          V[1,k,n]<-0

          for(t in 2:length(ag[,1,1,1])){
            z[t,k,n]~dbern(phi[k,n])
            z1[t,k,n]=z[t,k,n]+1
            u[t,k,n]~dbern(psi[k,n])
            u1[t,k,n]=u[t,k,n]+1

            V[t,k,n]<-V[t-1,k,n]+beta[u1[t,k,n],n]*(ag[t-1,2,k,n]-M[t-1,k,n])
            M[t,k,n]<-M[t-1,k,n]+V[t,k,n]+alpha[z1[t,k,n],n]*(ag[t-1,2,k,n]-M[t-1,k,n])

            ag[t,1,k,n]~dnorm(M[t,k,n],1/sigma[k])
            pred.y[t,k,n]~dnorm(M[t,k,n],1/sigma[k])
          }
        }
      }  
      }', 'velocity_jumps.txt')

# Initial values for the chains
initial_values <-list(
  list(
    alpha=matrix(runif(length(ag[1,1,1,])*2,0,1),nrow=2,ncol=length(ag[1,1,1,])),
    beta=matrix(runif(length(ag[1,1,1,])*2,0,1),nrow=2,ncol=length(ag[1,1,1,])),
    phi=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    psi=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    sigma=rgamma(4,1,1)
  )
)

#Parameters to be Monitored

parameters <- c('z','u','phi','psi','alphas','betas','sigma','pred.y')

# The following command calls JAGS with specific options

samples_dis <- jags.parallel(datos_jags, inits = initial_values, 
                parameters.to.save = parameters, model.file = 'velocity_jumps.txt',
                n.chains = 3, n.iter = 10000, n.burnin = 5000, n.thin = 2,
                DIC = T)
unlink('velocity_jumps.txt')
save(samples_dis,file="data/discrete_learning.Rdata")

#### Velocity ####
write('model{

      sigma[1]~dgamma(0.1,0.1)
      sigma[2]~dgamma(0.1,0.1)
      sigma[3]~dgamma(0.1,0.1)
      sigma[4]~dgamma(0.1,0.1)

      for(n in 1:length(ag[1,1,1,])){
      for(k in 1:length(ag[1,1,,1])){
      
      alpha[k,n]~dunif(0,1)
      beta[k,n]~dunif(0,1)
      M[1,k,n]<-ag[1,2,k,n]
      V[1,k,n]<-0
      
      for(t in 2:length(ag[,1,1,1])){
      
      V[t,k,n]<-V[t-1,k,n]+beta[k,n]*(ag[t-1,2,k,n]-M[t-1,k,n])
      M[t,k,n]<-M[t-1,k,n]+V[t,k,n]+alpha[k,n]*(ag[t-1,2,k,n]-M[t-1,k,n])
      
      ag[t,1,k,n]~dnorm(M[t,k,n],1/sigma[k])
      pred.y[t,k,n]~dnorm(M[t,k,n],1/sigma[k])
      }
      }
      }  
      }', 'Delta_vel.txt')

# Initial values for the chains
initial_values <-list(
  list(
    alpha=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow = 4,ncol=length(ag[1,1,1,])),
    beta=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow = 4,ncol=length(ag[1,1,1,])),
    sigma=rgamma(4,1,1)
  )
)

#Parameters to be Monitored

parameters <- c('alpha','beta','sigma','pred.y')

# The following command calls JAGS with specific options

samples <- jags.parallel(datos_jags, inits = initial_values, 
                         parameters.to.save = parameters, model.file = 'Delta_vel.txt',
                         n.chains = 3, n.iter = 10000, n.burnin = 5000, n.thin = 2,
                         DIC = T)
unlink('Delta_vel.txt')
save(samples,file="data/velocity.Rdata")

#### P. I. D. ####
write(
  'model{
  # Prior Distribution Variance
  sigma[1]~dgamma(0.1,0.1)
  sigma[2]~dgamma(0.1,0.1)
  sigma[3]~dgamma(0.1,0.1)
  sigma[4]~dgamma(0.1,0.1)

  for (n in 1:length(ag[1,1,1,])){
    for(k in 1:length(ag[1,1,,1])){

  # Proportional gain
      Kp[k,n]~dunif(-1,1)
  
  # Integral gain
     KI[k,n]~dunif(-1,1)    
  
  # Derivative gain
      KD[k,n]~dunif(-1,1)
  
  # Memory retention
      lambda[k,n]~dunif(0,1)

      M[1,k,n]=ag[1,2,k,n]
      e[1,k,n]=0
      I[1,k,n]=0
  
      for(t in 2:length(ag[,1,1,1])){
        e[t,k,n]=ag[t-1,2,k,n]-M[t-1,k,n]
        I[t,k,n]=lambda[k,n]*I[t-1,k,n]+KI[k,n]*e[t,k,n]
        u[t,k,n]=Kp[k,n]*e[t,k,n]+I[t,k,n]+KD[k,n]*(e[t,k,n]-e[t-1,k,n])
        M[t,k,n]=M[t-1,k,n]+u[t,k,n]
        ag[t,1,k,n]~dnorm(M[t,k,n],1/sigma[k])
        pred.y[t,k,n]~dnorm(M[t,k,n],1/sigma[k])
      }
    }
  }
  
  }','PID.bug')

# Initial values for the chains
initial_values <-list(
  list(
    KI = array(runif(length(ag[1,1,,1])*length(ag[1,1,1,]),0,1),
               dim=c(length(ag[1,1,,1]),length(ag[1,1,1,]))),
    KD = array(runif(length(ag[1,1,,1])*length(ag[1,1,1,]),0,1),
               dim=c(length(ag[1,1,,1]),length(ag[1,1,1,]))),
    Kp = array(runif(length(ag[1,1,,1])*length(ag[1,1,1,]),0,1),
               dim=c(length(ag[1,1,,1]),length(ag[1,1,1,]))),
    lambda = array(runif(length(ag[1,1,,1])*length(ag[1,1,1,]),0,1),
                   dim=c(length(ag[1,1,,1]),length(ag[1,1,1,]))),
    sigma_1=rgamma(2,1,1),sigma_2=rgamma(2,1,1),sigma_3=rgamma(2,1,1),
    sigma_4=rgamma(2,1,1))
)

# Parameters to be monitored
parameters <- c('Kp','KI','KD','sigma','lambda','pred.y')

# The following command calls JAGS with specific options
samples <- jags.parallel(datos_jags, inits = initial_values, 
                         parameters.to.save = parameters, model.file = 'PID.bug',
                         n.chains = 3, n.iter = 10000, n.burnin = 5000, n.thin = 2,
                         DIC = T)
unlink('PID.bug')
save(samples,file="data/PID.Rdata")

#### simple delta ####
write('model{

      sigma[1]~dgamma(0.1,0.1)
      sigma[2]~dgamma(0.1,0.1)
      sigma[3]~dgamma(0.1,0.1)
      sigma[4]~dgamma(0.1,0.1)
      
      for(n in 1:length(ag[1,1,1,])){
      alpha[1,n]~dunif(0,1)
      alpha[2,n]~dunif(0,1)
      alphas[1:2,n]=sort(alpha[,n])
      phi[n]~dnorm(0,0.1)T(0,)

      for(k in 1:length(ag[1,1,,1])){
      M[1,k,n]<-ag[1,2,k,n]
      
      for(t in 2:length(ag[,1,1,1])){
      z[t,k,n]<-step(phi[n]-abs(ag[t-1,2,k,n]-M[t-1,k,n]))
      M[t,k,n]<-M[t-1,k,n]+z[t,k,n]*alpha[1,n]*(ag[t-1,2,k,n]-M[t-1,k,n])+(1-z[t,k,n])*alpha[2,n]*(ag[t-1,2,k,n]-M[t-1,k,n])
      ag[t,1,k,n]~dnorm(M[t,k,n],1/sigma[k])
      pred.y[t,k,n]~dnorm(M[t,k,n],1/sigma[k])
      }
      }
      }  
      }', 'Delta.txt')

initial_values <-list(
  list(
    alpha=matrix(runif(length(ag[1,1,1,])*2,0,1),nrow=2,ncol=length(ag[1,1,1,])),
    phi=rgamma(24,1,1),
    sigma=rgamma(4,1,1)
  )
)

#Parameters to be Monitored

parameters <- c('alpha','z','phi','sigma','pred.y')

# The following command calls JAGS with specific options

samples <- jags.parallel(datos_jags, inits = initial_values, 
                         parameters.to.save = parameters, model.file = 'Delta.txt',
                         n.chains = 3, n.iter = 10000, n.burnin = 5000, n.thin = 2,
                         DIC = T)
unlink('Delta.txt')
save(samples,file="data/delta.Rdata")

#### Model_Selection ####
write('model{
      # Variance for velcity
      sigma[1,1]~dgamma(0.1,0.1)
      sigma[2,1]~dgamma(0.1,0.1)
      sigma[3,1]~dgamma(0.1,0.1)
      sigma[4,1]~dgamma(0.1,0.1)
      
      # Variance for PID
      sigma[1,2]~dgamma(0.1,0.1)
      sigma[2,2]~dgamma(0.1,0.1)
      sigma[3,2]~dgamma(0.1,0.1)
      sigma[4,2]~dgamma(0.1,0.1)

      # Variance for simple delta
      sigma[1,3]~dgamma(0.1,0.1)
      sigma[2,3]~dgamma(0.1,0.1)
      sigma[3,3]~dgamma(0.1,0.1)
      sigma[4,3]~dgamma(0.1,0.1)

      # Prior prob models
      pi[1:3] ~ ddirch(c(1,1,1))

      for(n in 1:length(ag[1,1,1,])){
    # Model Selection Variable 
        m[n]~dcat(pi[])

    # Parameters Simple delta
        alpha_d[1,n]~dunif(0,1)
        alpha_d[2,n]~dunif(0,1)
        alphas_d[1:2,n]=sort(alpha_d[,n])
        phi_d[n]~dnorm(0,0.1)T(0,)

        for(k in 1:length(ag[1,1,,1])){
    # Parameters Velocity      
          alpha_v[k,n]~dunif(0,1)
          beta_v[k,n]~dunif(0,1)

    # Parameters PID
          kp[k,n]~dunif(-1,1)
          ki[k,n]~dunif(-1,1)    
          kd[k,n]~dunif(-1,1)
          lambda[k,n]~dunif(0,1)

      # Initial values for models
      #velocity
          V[1,k,n]<-0
          M[1,k,n,1]<-ag[1,2,k,n]
      #pid
          e[1,k,n]=0
          I[1,k,n]=0
          M[1,k,n,2]<-ag[1,2,k,n]
      #simple delta
          M[1,k,n,3]<-ag[1,2,k,n]

      for(t in 2:length(ag[,1,1,1])){
      # Model Updates
      #velocity
            V[t,k,n]<-V[t-1,k,n]+beta_v[k,n]*(ag[t-1,2,k,n]-M[t-1,k,n,2])
            M[t,k,n,1]<-M[t-1,k,n,1]+V[t,k,n]+alpha_v[k,n]*(ag[t-1,2,k,n]-M[t-1,k,n,1])
      #pid
            e[t,k,n]=ag[t-1,2,k,n]-M[t-1,k,n,2]
            I[t,k,n]=lambda[k,n]*I[t-1,k,n]+ki[k,n]*e[t,k,n]
            u[t,k,n]=kp[k,n]*e[t,k,n]+I[t,k,n]+kd[k,n]*(e[t,k,n]-e[t-1,k,n])
            M[t,k,n,2]=M[t-1,k,n,2]+u[t,k,n]
      #simple delta
            z[t,k,n]<-step(phi_d[n]-abs(ag[t-1,2,k,n]-M[t-1,k,n,3]))
            M[t,k,n,3]<-M[t-1,k,n,3]+z[t,k,n]*alpha_d[1,n]*(ag[t-1,2,k,n]-M[t-1,k,n,3])+(1-z[t,k,n])*alpha_d[2,n]*(ag[t-1,2,k,n]-M[t-1,k,n,3])
      # Likelihood Function
            ag[t,1,k,n]~dnorm(M[t,k,n,m[n]],1/sigma[k,m[n]])
          }
        }
      }  
      }', 'models.txt')

parameters <- c('m','pi')

initial_values <-list(
  list(
    alpha_d=matrix(runif(length(ag[1,1,1,])*2,0,1),nrow=2,ncol=length(ag[1,1,1,])),
    alpha_v=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    beta_v=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    kd=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    ki=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    kp=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    lambda=matrix(runif(length(ag[1,1,1,])*4,0,1),nrow=4,ncol=length(ag[1,1,1,])),
    phi_d=rgamma(24,1,1),
    pi=c(1/3,1/3,1/3),
    sigma=matrix(rgamma(12,1,1),nrow=4,ncol=3)
  )
)
# The following command calls JAGS with specific options

samples <- jags.parallel(datos_jags, inits = initial_values, 
                         parameters.to.save = parameters, model.file = 'models.txt',
                         n.chains = 3, n.iter = 10000, n.burnin = 6000, n.thin = 2,
                         DIC = T)
unlink('models.txt')
save(samples,file="data/models.Rdata")
