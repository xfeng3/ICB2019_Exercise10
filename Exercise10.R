#Exercise 10

library(ggplot2)

#parameters; basic model when the cancer drug is absent
N0=99
M0=1
rN=0.1
rM=0.1
k=1000000
timesteps=500

#create vector to store N's M's and set initial N M 
Nt=numeric(length=timesteps)
Mt=numeric(length=timesteps)
Nt[1]=N0
Mt[1]=M0

# simulate
for (t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+rN*Nt[t]*(1-(Nt[t]+Mt[t])/k)
  Mt[t+1] <- Mt[t]+rM*Mt[t]*(1-(Nt[t]+Mt[t])/k)
}

# plot results
simdata <- data.frame(time=1:timesteps,N=Nt,M=Mt)


ggplot(data=simdata)+
  geom_line(aes(x=time,y=N),col="blue")+
  geom_line(aes(x=time,y=M),col="red")+
  theme_classic()



# event: drug treatment 
max(Nt)
max(Mt)
sum(Nt<989999)
sum(Mt<9999)
# after ~ 228 days, the growth reaches its equilibrium stage, Nt ~ 990000, Mt ~ 10000
#lets say drug present at t=250

# parameters
N0=99
M0=1
rN=0.1
rM=0.1
rNdrug=-0.1
rMdrug=0.1*0.5
k=1000000
timesteps=1000

#create vectors and set initial
Ntdrug=numeric(length=timesteps)
Mtdrug=numeric(length=timesteps)
Ntdrug[1]=N0
Mtdrug[1]=M0

# simulate
for (t in 1:(timesteps-1)){
  if (t<=250){
    Ntdrug[t+1] <- Ntdrug[t]+rN*Ntdrug[t]*(1-(Ntdrug[t]+Mtdrug[t])/k)
    Mtdrug[t+1] <- Mtdrug[t]+rM*Mtdrug[t]*(1-(Ntdrug[t]+Mtdrug[t])/k)
  }else if (t>250){
    Ntdrug[t+1] <- Ntdrug[t]+rNdrug*Ntdrug[t]*(1-(Ntdrug[t]+Mtdrug[t])/k)
    Mtdrug[t+1] <- Mtdrug[t]+rMdrug*Mtdrug[t]*(1-(Ntdrug[t]+Mtdrug[t])/k)
  }
}

#plot results
simdrug <- data.frame(time=1:timesteps,Ndrug=Ntdrug,Mdrug=Mtdrug)

ggplot(data=simdrug)+
  geom_line(aes(x=time,y=Ntdrug),col="blue")+
  geom_line(aes(x=time,y=Mtdrug),col="red")+
  theme_classic()


