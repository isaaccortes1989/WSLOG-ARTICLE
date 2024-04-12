# Geometric feature of pollen grains (ridge variable)

rm(list=ls())
library(goftest)


ridge <- scan()
 -2.3482  -1.1520  -2.5245   5.7523   8.7494  10.4303  -3.6049  -5.6383
 9.5434  -9.0292   3.1676  -4.6072  -9.0119  20.6333   3.8890   0.5247
 2.0099  -4.8432  -0.4911   6.8007  -2.4424  14.4619   8.7413   4.7543
 1.2480   8.7653 -11.8489  -7.2062   6.2312   9.1580   2.6893  10.0658
 -1.7084  -1.0802  -1.3009  -5.3000  -5.2823   9.7174   2.8455   4.9671
 5.5170  -9.0296   4.1365   1.0009  -8.7272  21.4066  -5.6215  -3.4745
 -0.7744  -0.1216 -16.0161  10.5389   0.2195  12.7217   1.4815  -0.0682
 9.0331   5.3055  -9.5009   2.6741 -10.0123   4.3691 -10.1716  -3.2580
 4.2181  -4.0808  -8.6400  10.5636  -6.6808   0.5323  -0.0347   5.8613
 4.6748 -10.7319  11.2446  -3.6473  -0.1305   1.9694   5.5513   9.6421
 -7.3720  -3.2901   4.4276  -2.1656  -8.4973   5.4033   7.3309   1.0244
 -0.7617  -2.5069  -2.3577  -5.9772   0.2097  -5.7256   0.8712   3.7330
 8.3684  12.5461  -0.3302   5.6671  -1.3391   0.9769   1.0954  -9.8401
 6.2773  -1.2831   9.3625  -5.3853 -19.4687  -2.1201  -3.3624   6.9285
 -0.9824   1.3317   8.6132   0.1884  11.1434   3.0768  12.5006  -7.2107
 -4.1780  -1.2636 -15.7861   9.0190  -0.0765  -0.4494  -1.5315  -7.5128
 1.3885  -1.6751  -8.8635   8.6672  -9.3815  -6.9950  10.5963   3.6756
 -2.1803  11.5577  -1.0654   3.9705  11.2838  -2.6200  -2.7857   0.1748
 -3.2512   7.2893 -10.0208   5.0440  10.8437  -2.3471 -10.3495   4.0742
 1.0975   8.5759  -2.1309   5.1094  -9.1212   3.2973 -11.8830   8.6839
 1.3887   7.7918  -2.2878  -4.8403  -2.3645   5.3825   5.6654  -6.4549
 4.3399   0.7436   8.4039  -1.2118  -5.5593   4.7584   3.2325   4.8088
 5.5635   0.6031   1.6820   2.1964  -0.6508  12.4694   9.0270   8.3147
 -9.6008   9.3146   8.5171  -2.3400   1.1597   6.0931  -2.6372   4.1168
 -10.8543   2.1395   0.1053   2.7796   5.4484  -3.5852   8.6165  -5.6919
 -11.7412   0.0035  -7.6364  -7.9206   2.1429  -8.8182  -0.4512   3.0627
 -0.0484   5.8438  -8.9771   7.4250  -8.7096  12.0701  -2.2209  -0.7701
 -2.5697  -1.9026   4.1961  -4.7872  -1.4042   4.5487   3.1834  -3.4219
 -6.4657   0.0031  -2.3624  -9.1154   1.3963  -0.7247  -7.8587   2.5399
 15.2161  -9.2055  -1.0014   5.2710   6.2357  -9.7965  -3.7986   0.1032
 -4.5187   7.0820  -3.3389  16.4471  -8.2948  -5.7590   0.8738   6.9772
 -1.2710  -1.7422  -1.9812   4.8942   1.4845   4.3295   9.0255   0.6149
 -6.0667  -3.1802   4.5414   0.0049   1.1099   5.9499   6.9976   5.3570
 -5.4861   5.5131  -5.9075   4.2265   0.5415  -0.4104   2.0282  -1.2938
 4.3264   0.5400   3.0757   0.0472  -2.4591   2.9562  -0.8694  -3.3632
 -5.0439  -1.7464 -13.9144  -3.0695   0.4000  -2.8783   4.0782  -5.2393
 -8.3139  -8.3610   7.5810  -5.2911   7.1971   3.1784   4.3904  -3.9683
 9.8678   5.1990   2.3792  -1.6898   8.9318   0.3717   4.2381   2.8426
 1.2520  10.1548  -8.1083  -3.3096  -1.3680   1.5916   2.9996  -1.2653
 8.9275  -2.4772  -1.4878   8.7311  -1.2549  -9.7530  11.8201  -1.7798
 9.5773  -6.4465  -3.0670   5.9473  -2.2350   3.2189  -3.9335   0.6818
 7.1360   1.7804 -11.5737  -3.3548   4.7858   3.5527  -3.2145   4.1208
 -1.1699   5.6923  -1.3262 -17.2352   1.4513   8.3954   4.4174   7.3212
 -0.8326  -0.2579   6.0260  -2.1991  -7.2088  -2.5852 -14.6721  -2.2030
 -0.5855  -2.7191 -12.0812   4.2242   8.5269  -1.0164  -1.8736  -9.9934
 -1.5573  14.6113   5.7668  -0.4957  -5.6666  -5.8793   5.3955   3.4726
 5.3337   1.7459   9.4113   4.0403  -1.0421  -0.7476  -7.1085   1.5419
 8.6023   3.1284  -1.2153   7.5255  -9.2442  -2.9533  -3.1696  -1.8022
 -1.0677 -12.5226  10.0139   0.0155   4.0185   6.1244 -13.8260   2.7295
 1.3964  -7.8086  -1.5308  -2.2180  -1.5633  -5.3944  -4.4752   3.8472
 -6.0208  -5.6561  -1.2650 -10.3435  -1.8583  -3.9252  -7.5017  -1.6760
 4.8097  -1.5094  -1.0788   1.4048  -3.8970  -1.0757 -15.0442  -2.6213
 2.7782   5.5336  -2.4413   1.1539  -9.7900  -0.1612  -2.5036   3.4944
 2.2096  -4.4115  -2.9091  -5.4192   5.5983   1.8223   9.7720   3.8949
 -4.2672   3.4021  -9.9554   3.2419  -4.9058  15.9321  -1.0804  -5.9879
 4.5704  -8.5582 -11.0258   0.8178   8.0552  -1.6054  -1.3994  -1.1389
 2.5807  -7.9111  -4.3778  -3.1818  -3.5117  -4.2322  -7.5471  -1.2559
 -8.6210  -7.4153   6.4224  -1.1279   5.5018   4.5227  10.4951   0.8234
 0.1635  -3.2862  -8.2171   1.9511   6.2352  -3.4346   9.1900   5.4091
 -0.7965  -5.6503  -1.2429  13.6923  -1.7086   9.1614  -4.1689 -13.1823
 2.1475
 
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
 
 fit1 <- optim(par=c(-1.4577208,1.5584372,0.2553727,0.1817768),fn=loglikWSLog,method=c("BFGS"),hessian=TRUE,x=ridge)
 fit1$converge                                                 # 0 Converged
 round(fit1$par,3)                                             # Estimates
 round(sqrt(diag(solve(fit1$hessian))),3)                      # Standard Errors
 aic1  <- 2*fit1$value+2*length(fit1$par)                      # AIC
 gaic1 <- 2*fit1$value+length(fit1$par)*(log(length(ridge))+1) # GAIC
 bic1  <- 2*fit1$value+length(fit1$par)*(log(length(ridge)))   # BIC
 
 #================================#
 #=AD & CVM TEST==================#
 #================================#
 seed <- 1
 set.seed(seed)

 ad.test(ridge, "pWSLog", m=-1.4577208, s=1.5584372, a=0.2553727, b=0.1817768, estimated=TRUE)
 #Anmax = 3.684, p-value = 0.2453
 
 set.seed(seed)
 cvm.test(ridge, "pWSLog", m=-1.4577208, s=1.5584372, a=0.2553727, b=0.1817768, estimated=TRUE)
 #omega2max = 0.70206, p-value = 0.2301
 
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
 
 fit2 <- optim(par=c(-1.2073,3.2311,7.2809,1.9855,0.8319),fn=loglikMLog,method=c("BFGS"),hessian=TRUE,x=ridge)
 fit2$converge                                                 # 0 Converged
 round(fit2$par,3)                                             # Estimates
 round(sqrt(diag(solve(fit2$hessian))),3)                      # Standard error
 aic2  <- 2*fit2$value+2*length(fit2$par)                      # AIC
 gaic2 <- 2*fit2$value+length(fit2$par)*(log(length(ridge))+1) # GAIC
 bic2  <- 2*fit2$value+length(fit2$par)*(log(length(ridge)))   # BIC
 
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
 
  fit3 <- optim(par=c(2.6,2.2,0.3,-0.2),fn=loglikSBLog,method=c("BFGS"),hessian=TRUE,x=ridge)
  fit3$converge                                                 # 0 Converged
  round(fit3$par,3)                                              # Estimates
  round(sqrt(diag(solve(fit3$hessian))),3)                       # Standard Errors
  aic3  <- 2*fit3$value+2*length(fit3$par)                       # AIC
  gaic3 <- 2*fit3$value+length(fit3$par)*(log(length(ridge))+1)  # GAIC
  bic3  <- 2*fit3$value+length(fit3$par)*(log(length(ridge)))    # BIC
  
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
 
 fit4 <- optim(par=c(2.4,6.1,-0.2,-0.4),fn=loglikSFN,method=c("BFGS"),hessian=TRUE,x=ridge)
 fit4$converge                                                 # 0   Converged
 round(fit4$par,3)                                             # Estimates
 round(sqrt(diag(solve(fit4$hessian))),3)                      # Standard Errors
 aic4  <- 2*fit4$value+2*length(fit4$par)                      # AIC
 gaic4 <- 2*fit4$value+length(fit4$par)*(log(length(ridge))+1) # GAIC
 bic4  <- 2*fit4$value+length(fit4$par)*(log(length(ridge)))   # BIC
 
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
 
 fit5 <- optim(par=c(-1.29,7.00,0.39,0.32),fn=loglikABPN,method=c("BFGS"),hessian=TRUE,x=ridge)
 fit5$converge                                                    # 0 Converged
 round(fit5$par,3)                                                # Estimates
 round(sqrt(diag(solve(fit5$hessian))),3)                         # Standard Error 
 aic5  <- 2*fit5$value+2*length(fit5$par)                         # AIC
 gaic5 <- 2*fit5$value+length(fit5$par)*(log(length(ridge))+1)    # GAIC
 bic5  <- 2*fit5$value+length(fit5$par)*(log(length(ridge)))      # BIC

 #==============================#
 #======HISTOGRAMS==============#
 #==============================#
 hist(ridge,freq=F,nclass=21,xlab="Ridge variable",ylab="Density function",cex.lab=1.3,col="white",main="")
 m      <- -1.458
 s      <- 1.558
 lambda <- 0.182
 a      <- 0.255
 curve(dWSLog(x,m,s,a,lambda),add=TRUE,lwd=2)
 
 m      <- -1.207
 s      <- 3.231
 m2     <- 7.281
 s2     <- 1.986
 a      <- 0.832
 curve(dMLog(x,m,s,m2,s2,a),add=TRUE,lwd=2,col="grey")
 
 
 m      <- 2.612
 s      <- 2.241
 lambda <- -0.264
 a      <- 0.376
 curve(dSBLog(x,m,s,a,lambda),add=TRUE,lwd=2,lty=2,col="grey")
 
 
 m      <- 2.464  
 s      <- 6.176 
 lambda <- -0.417 
 a      <- -0.214
 curve(dSFN(x,m,s,a,lambda),add=TRUE,lwd=2,lty=3,col="grey")
 
 
 m      <- -1.294
 s      <- 6.967
 a      <- 0.392
 lambda <- 0.327 
 curve(dABPN(x,m,s,a,lambda),add=TRUE,lwd=2,lty=4,col="grey")
 box()
 legend("topleft",c("WSLOG","MLOG","SBLOG","ABPN","SFN"),lty=c(1,1:4),col=c("black","grey","grey","grey","grey"),lwd=2,bty="n")
 
