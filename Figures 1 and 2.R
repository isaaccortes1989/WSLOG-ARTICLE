# b is the lambda parameter 
# m is the mu parameter
# s is the sigma parameter 
# a is the alpha parameter

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

#Probability density function

dWSLog <- function(y,m,s,a,b){
  2/(s*(1+a*b2_fun(b)))*(1+a*((y-m1_fun(m,s,b))/s1_fun(s,b))^4)*dlogis((y-m)/s)*plogis(b*((y-m)/s))
}

#Cumulative distribution function

pWSLog <- function(x,m,s,a,b){
  n=length(x)
  f=rep(0,n)
  for(i in 1:n){
    f[i]=integrate(function(u,m,s,a,b){dWSLog(u,m,s,a,b)},
                   lower=-600,upper=x[i],m=m,s=s,b=b,a=a)$value
  }
  return(f)
}

# Hazard rate function

hWSLog <- function(x,m,s,a,b){
  dWSLog(x,m,s,a,b)/(1-pWSLog(x,m,s,a,b))
}



#===============#
#Figure 1=======#
#===============#
m      <- 10
s      <- 2
lambda <- 0.5
a      <- 0.5
curve(dWSLog(x,m,s,a,lambda),-10,37,ylim=c(0,0.11),lwd=2,xlab="y",ylab="Density function",cex.lab=1.3)
lambda <- 4
a      <- 0.5
curve(dWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=2)
lambda <- -1
a      <- 0.1
curve(dWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3)

#===============#
#Figure 2=======#
#===============#

#Figure 2 (left)

m      <- 0
s      <- 1
lambda <- -0.2
a      <- 5
curve(hWSLog(x,m,s,a,lambda),-10,20,lwd=2,xlab="y",ylab="Hazard rate function",cex.lab=1.3)
lambda <- 0.5
curve(hWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=2)
lambda <- 2
curve(hWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3)
legend("topleft",c("-0.2","0.5","2"),lty=1:3,lwd=2,title=expression(lambda),bty="n")

#Figure 2 (right)

m      <- 0
s      <- 1
lambda <- -0.2
a      <- 0.1
curve(hWSLog(x,m,s,a,lambda),-10,20,lwd=2,xlab="y",ylab="Hazard rate function",cex.lab=1.3)
lambda <- 0.5
curve(hWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=2)
lambda <- 2
curve(hWSLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3)
legend("topleft",c("-0.2","0.5","2"),lty=1:3,lwd=2,title=expression(lambda),bty="n")



