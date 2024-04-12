rm(list=ls())

library(DAAG)
library(goftest)

tdengue <- dengue$temp
tdengue <- na.omit(tdengue)
#===========================================================#
#===Run the code lines for each distribution independently==#
#===========================================================#

#==========================#
#========WSLOG=============#
#==========================#

#I_r function

Ir_fun <- function(r,b){ 
  n = length(b)
  f = numeric(n)
  for(i in 1:n){
    f[i] = integrate(function(x,r,b)x^r*dlogis(x)*plogis(b*x), r=r, b=b, lower=-Inf, upper=Inf)$value
  }
  return(f)
}

#mu_1 prime

m1_fun <- function(m,s,b){   
  2*s*Ir_fun(1,b)+m
}

#sigma_1 prime

s1_fun <- function(s,b){    
  sqrt(2*s^2*(Ir_fun(2,b)-2*Ir_fun(1,b)^2))
}

#k_lambda

b2_fun <- function(b){ 
  (Ir_fun(4,b)-8*Ir_fun(1,b)*Ir_fun(3,b)+24*Ir_fun(1,b)^2*Ir_fun(2,b)-24*Ir_fun(1,b)^4)/(2*(Ir_fun(2,b)-2*Ir_fun(1,b)^2)^2)
}

#pdf

dWSLog <- function(y,m,s,a,b){
  2/(s*(1+a*b2_fun(b)))*(1+a*((y-m1_fun(m,s,b))/s1_fun(s,b))^4)*dlogis((y-m)/s)*plogis(b*((y-m)/s))
}

#cdf

pWSLog <- function(x,m,s,a,b){
  n=length(x)
  f=rep(0,n)
  for(i in 1:n){
    f[i]=integrate(function(u,m,s,a,b){dWSLog(u,m,s,a,b)},
                   lower=-Inf,upper=x[i],m=m,s=s,b=b,a=a)$value
  }
  return(f)
}

#log-likelihood

loglikWSLog <- function(x,p){
  -sum(log(dWSLog(x,p[1],p[2],p[3],p[4])))
}

#==========================#
#========FIT===============#
#==========================#
fit1 <- optim(par=c(27.604265,exp(1.010003),exp(-2.130810),-5.947935),fn=loglikWSLog,method=c("L-BFGS-B"),lower=c(-Inf,0,0,-Inf),upper = c(Inf,Inf,Inf,Inf),hessian=TRUE,x=tdengue)
fit1$converge                                                 # 0 Converged
round(fit1$par,3)                                             # Estimates
round(sqrt(diag(solve(fit1$hessian))),3)                      # Standard Errors
aic1  <- 2*fit1$value+2*length(fit1$par)                      # AIC
gaic1 <- 2*fit1$value+length(fit1$par)*(log(length(tdengue))+1) # GAIC
bic1  <- 2*fit1$value+length(fit1$par)*(log(length(tdengue)))   # BIC

#================================#
#=AD & CVM TEST==================#
#================================#
seed <- 8 
set.seed(seed)

ad.test(tdengue, "pWSLog", m=27.6042651, s=2.7456093, a=0.1187425, b=-5.9479350, estimated=TRUE)
#Anmax = 2.87, p-value = 0.7697

set.seed(seed)
cvm.test(tdengue, "pWSLog", m=27.6042651, s=2.7456093, a=0.1187425, b=-5.9479350, estimated=TRUE)
#omega2max = 0.53538, p-value = 0.768

#==========================#
#========MLOG=============#
#==========================#

#pdf

dMLog <- function(x,m,s,m2,s2,a){
  a/s*exp((x-m)/s)/(1+exp((x-m)/s))^2+(1-a)/s2*exp((x-m2)/s2)/(1+exp((x-m2)/s2))^2
}

#log-likelihood

loglikMLog <- function(x,p){
  -sum(log(dMLog(x,p[1],p[2],p[3],p[4],p[5])))
}

#==========================#
#========FIT===============#
#==========================#
fit2 <- optim(par=c(12.5352251,1.3463266,25.2309998,0.2215619,0.5306689),fn=loglikMLog,method=c("L-BFGS-B"),lower=c(-Inf,0,-Inf,0,0),upper = c(Inf,Inf,Inf,Inf,1),hessian=TRUE,x=tdengue)
fit2$converge                                                 # 0 Converged
round(fit2$par,3)                                             # Estimates
round(sqrt(diag(solve(fit2$hessian))),3)                      # Standard error
aic2  <- 2*fit2$value+2*length(fit2$par)                      # AIC
gaic2 <- 2*fit2$value+length(fit2$par)*(log(length(tdengue))+1) # GAIC
bic2  <- 2*fit2$value+length(fit2$par)*(log(length(tdengue)))   # BIC


#==========================#
#========SBLOG=============#
#==========================#

#pdf
dSBLog <- function(x,m,s,a,lambda){
  6/s^3*(s^2+a*(x-m)^2)/(3+pi^2*a)*exp((x-m)/s)/(1+exp((x-m)/s))^2*exp(lambda*(x-m)/s)/(1+exp(lambda*(x-m)/s))
}

#log-likelihood
loglikSBLog <- function(x,p){
  -sum(log(dSBLog(x,p[1],p[2],p[3],p[4])))
}

#==========================#
#========FIT===============#
#==========================#
fit3 <- optim(par=c(15.7350161,0.9374269,0.5129876,0.2022054),fn=loglikSBLog,method=c("L-BFGS-B"),lower=c(-Inf,0,0,-Inf),upper = c(Inf,Inf,Inf,Inf),hessian=TRUE,x=tdengue)
fit3$converge                                                 # 0 Converged
round(fit3$par,3)                                              # Estimates
round(sqrt(diag(solve(fit3$hessian))),3)                       # Standard Errors
aic3  <- 2*fit3$value+2*length(fit3$par)                       # AIC
gaic3 <- 2*fit3$value+length(fit3$par)*(log(length(tdengue))+1)  # GAIC
bic3  <- 2*fit3$value+length(fit3$par)*(log(length(tdengue)))    # BIC

#==========================#
#========SFN===============#
#==========================#  

#pdf  
dSFN <- function(x,m,s,a,lambda){
  1/s*(1-pnorm(a))^{-1}*dnorm(abs((x-m)/s)+a)*pnorm(lambda*(x-m)/s)
}

#log-likelihood
loglikSFN <- function(x,p){
  -sum(log(dSFN(x,p[1],p[2],p[3],p[4])))
}

#==========================#
#========FIT===============#
#==========================#
fit4 <- optim(par=c(28.1771656,exp(2.7850971),0.6440979,-22.9419955),fn=loglikSFN,method=c("L-BFGS-B"),lower=c(-Inf,0,-Inf,-Inf),upper = c(Inf,Inf,Inf,Inf),hessian=TRUE,x=tdengue)
fit4$converge                                                 # 0   Converged
round(fit4$par,3)                                             # Estimates
round(sqrt(diag(solve(fit4$hessian))),3)                      # Standard Errors
aic4  <- 2*fit4$value+2*length(fit4$par)                      # AIC
gaic4 <- 2*fit4$value+length(fit4$par)*(log(length(tdengue))+1) # GAIC
bic4  <- 2*fit4$value+length(fit4$par)*(log(length(tdengue)))   # BIC


#==========================#
#=======ABPN===============#
#==========================# 

dABPN <- function(x,m,s,a,lambda){
  2*a/s*2^{a-1}/(2^a-1)*dnorm((x-m)/s)*(pnorm(abs((x-m)/s)))^{a-1}*pnorm(lambda*(x-m)/s)
}

loglikABPN <- function(x,p){
  -sum(log(dABPN(x,p[1],p[2],p[3],p[4])))
}

#==========================#
#========FIT===============#
#==========================#
fit5 <- optim(par=c(28.121304,exp(2.639883),exp(-10.552431),-19.284149),fn=loglikABPN,method=c("L-BFGS-B"),lower=c(-Inf,0,exp(-12),-Inf),upper = c(Inf,Inf,Inf,Inf),hessian=TRUE,x=tdengue)
fit5$converge                                                    # 0 Converged
round(fit5$par,3)                                                # Estimates
round(sqrt(diag(solve(fit5$hessian))),3)                         # Standard Error 
aic5  <- 2*fit5$value+2*length(fit5$par)                         # AIC
gaic5 <- 2*fit5$value+length(fit5$par)*(log(length(tdengue))+1)    # GAIC
bic5  <- 2*fit5$value+length(fit5$par)*(log(length(tdengue)))      # BIC

#==============================#
#======HISTOGRAMS==============#
#==============================#
hist(tdengue,freq=F,nclass=21,xlab="Temp variable",ylab="Density function",cex.lab=1.3,col="white",main="",ylim=c(0,0.10))
m      <- 27.604
s      <- 2.746
lambda <- -5.948
a      <- 0.119
curve(dWSLog(x,m,s,a,lambda),add=TRUE,lwd=2)

m  <- 12.529
s  <- 3.842
m2 <- 25.230
s2 <- 1.249
a  <- 0.530
curve(dMLog(x,m,s,m2,s2,a),add=TRUE,lwd=2,col="grey")


m      <- 15.736
s      <- 2.553
lambda <- 0.202
a      <- 1.671
curve(dSBLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=2,col="grey")



m      <- 28.177  
s      <- 16.201 
lambda <- -22.942   
a      <- 0.644
curve(dSFN(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3,col="grey")

m      <- 2.812145e+01  
s      <- 1.401152e+01 
lambda <- -1.928394e+01  
a      <- 2.585478e-05
curve(dABPN(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3,col="grey")
box()
legend("topleft",c("WSLOG","MLOG","SBLOG","ABPN","SFN"),lty=c(1,1:4),col=c("black","grey","grey","grey","grey"),lwd=2,bty="n")





