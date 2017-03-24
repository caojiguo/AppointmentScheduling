setwd("C:/Users/perry/Dropbox/ASP")
dyn.load("median.dll")

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
  out <- .C("binapprox",n=as.integer(length(cost)),x= as.double(cost),
              result = as.double(0.0))
  out[[3]] 
}


set.seed(123456)
n <- 1000000
mu <- 0
sig <- 1
udat <- matrix(exp(rnorm(3*n,mean=mu,sd=sig)), ncol=3)


#=============
# optimization
# alp1 = alp2 = alp3 =1
opt3.1 <- optim(c(1,1.5,2), med.3jobcost.2, para2=c(1,1,1),udat=udat,
         method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2))
# c(1, 1.5,2)
$par
[1] 0.9133914 2.2395425 3.6571192

$value
[1] 2.442403

system.time(opt3.1<-optim(c(1,2.5,5), med.3jobcost.2, para2=c(1,1,1),udat=udat,
        method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
user  system elapsed 
 24.39   6.11   30.50 

$par
[1] 1.051179 2.517027 3.897970

$value
[1] 2.484789


system.time(opt3.1<-optim(c(2,5,10), med.3jobcost.2, para2=c(1,1,1),udat=udat,
        method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
 user  system elapsed 
 25.95    6.75   32.71 

$par
[1] 1.034218 2.253736 3.696562

$value
[1] 2.451741

# relative stable, maybe there are local minimum

#===================
cat("unequal cost for each job\n")
# (alp1,alp2,alp3) = (1,2,3)
system.time(opt3.1 <- optim(c(1,1.5,2), med.3jobcost.2, para2=c(1,2,3),udat=udat,
         method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

 user  system elapsed 
 24.70    6.63   31.73 

$par
[1] 1.237333 2.599328 3.808457

$value
[1] 5.169722

system.time(opt3.1 <- optim(c(1,2.5,5), med.3jobcost.2, para2=c(1,2,3),udat=udat,
         method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

 user  system elapsed 
 22.23    5.85   28.14 

$par
[1] 1.283048 2.640703 3.819251

$value
[1] 5.166386


system.time(opt3.1 <- optim(c(1,5,10), med.3jobcost.2, para2=c(1,2,3),udat=udat,
         method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

 user  system elapsed 
 24.08    6.33   30.79 

$par
[1] 1.320580 2.649823 3.834464

$value
[1] 5.166024







