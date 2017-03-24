

med.3jobcost.2 <- function(para1,para2,udat)
{
  A2 <- para1[1]
  A3 <- para1[2]
  A4 <- para1[3]
  alp1 <- para2[1]
  alp2 <- para2[2]
  alp3 <- para2[3]
  if (A2 <=0 || A2 >= A3 || A3 >= A4) return (1.e10)
  u1 <- udat[,1]
  u2 <- udat[,2]
  u3 <- udat[,3]
  C1 <- u1
  term1 <- alp1*abs(C1-A2)
  C2 <- pmax(C1,A2)+u2
  term2 <- alp2*abs(C2-A3)
  C3 <- pmax(C2,A3)+u3
  term3 <- alp3*abs(C3-A4)
  cost <- term1+term2+term3
  out <- median(cost)
  out
}






q.muljobcost <- function(A,alp,udat,probs)
{
  
  if (A[1] <= 0 || any(diff(A) <= 0)) return (1.e10)
  d <- ncol(udat)
  cost <- alp[1]*abs(udat[,1]-A[1])
  C.temp <- udat[,1]
  for(j in 2:d)
  {
    C <- pmax(C.temp, A[j-1]) + udat[,j]
    cost <- cost + alp[j]*abs(C-A[j])
    C.temp <- C
  }
  as.numeric(quantile(cost,probs))
}

setwd("U:/ASP/r file")
#source("med.3jobcost.2")
library(rgenoud)

set.seed(123456)
n <- 1000000
mu <- 0
sig <- 1
dat <- matrix(exp(rnorm(3*n,mean=mu,sd=sig)), ncol=3)

#=======================
#test function
med.3jobcost.2(para1=c(1,1.5,2),para2=c(1,1,1),udat=udat)
q.muljobcost(A=c(1,1.5,2),alp=c(1,1,1),udat=udat,probs=.5)


#=============
# optimization
# alp1 = alp2 = alp3 =1
result.optim <- optim(c(1,1.5,2), med.3jobcost.2, para2=c(1,1,1),udat=dat,
                      method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2))
# c(1, 1.5,2)
# $par
# [1] 0.8793758 2.2239332 3.6021041
# 
# $value
# [1] 2.446872

result.GA <- genoud(fn=q.muljobcost, nvars=3, default.domains=5,
                    pop.size=500, alp=c(1,1,1), probs=0.5, udat=dat)
result.GA$par
#0.8859606 2.2446426 3.5996335
result.GA$value
#2.446725
#generate soltions acceptable.



# relative stable, maybe there are local minimum

#===================
#try more jobs (4, 5, 6, 7)
library(rgenoud)
set.seed(123456)
n <- 1000000
mu <- 0
sig <- 1
dat <- matrix(exp(rnorm(7*n,mean=mu,sd=sig)), ncol=7)

prob = c(.05, .25, .5, .75, .95)
est.4 <- matrix(0, nrow=5, ncol=5)
for(i in 1:5)
{
  result.GA <- genoud(fn=q.muljobcost, nvars=4, pop.size=500, alp=c(1,1,1,1), probs=prob[i], udat=dat[,1:4])
  est.4[i,1:4] <- result.GA$par
  est.4[i,5] <- result.GA$value
}

result.GA <- genoud(fn=q.muljobcost, nvars=4, pop.size=1000, Domains=cbind(rep(0,4),rep(20,4)),
                     alp=c(1,1,1,1), probs=prob[5], udat=dat[,1:4])
# est.4[4,1:4] <- result.GA$par
# est.4[4,5] <- result.GA$value

est.5 <- matrix(0, nrow=5, ncol=6)
for(i in 1:5)
{
  result.GA <- genoud(fn=q.muljobcost, nvars=5, pop.size=500, Domains=cbind(rep(0,5),rep(20,5)),
                      alp=c(1,1,1,1,1), probs=prob[i], udat=dat[,1:5])
  est.5[i,1:5] <- result.GA$par
  est.5[i,6] <- result.GA$value
}

est.5


est.6 <- matrix(0, nrow=5, ncol=7)
for(i in 1:5)
{
  result.GA <- genoud(fn=q.muljobcost, nvars=6, pop.size=500, Domains=cbind(rep(0,6),rep(25,6)),
                      alp=c(1,1,1,1,1,1), probs=prob[i], udat=dat[,1:6])
  est.6[i,1:6] <- result.GA$par
  est.6[i,7] <- result.GA$value
}

est.6

# result.GA <- genoud(fn=q.muljobcost, nvars=6, pop.size=1000, Domains=cbind(rep(0,6),rep(30,6)),
#                     alp=c(1,1,1,1,1,1), probs=prob[5], udat=dat[,1:6])
# est.6[5,1:6] <- result.GA$par
# est.6[5,7] <- result.GA$value
# 
# load("optpercentile.RData",verbose=T)
# est.4
# est.5
# est.6

#est.7

est.7 <- matrix(0, nrow=5, ncol=8)
for(i in 1:5)
{
  result.GA <- genoud(fn=q.muljobcost, nvars=7, pop.size=1000, Domains=cbind(rep(0,7),rep(30,7)),
                      alp=rep(1,7), probs=prob[i], udat=dat[,1:7])
  est.7[i,1:7] <- result.GA$par
  est.7[i,8] <- result.GA$value
}

# result.GA <- genoud(fn=q.muljobcost, nvars=7, pop.size=2000, Domains=cbind(rep(0,7),rep(30,7)),
#                     alp=rep(1,7), probs=prob[5], udat=dat[,1:7],max=F)
# 
# est.7[5,1:7] <- result.GA$par
# est.7[5,8] <- result.GA$value
# est.4
# est.5
# est.6
# est.7
# save(est.4,est.5,est.6,est.7, file="optpercentile.RData")

# load("optpercentile.RData", verbose=T)
# est.4
