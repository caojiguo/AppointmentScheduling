setwd("C:/Users/perry/Dropbox/ASP")


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
# $par
# [1] 0.8793758 2.2239332 3.6021041
# 
# $value
# [1] 2.446872

system.time(opt3.1<-optim(c(1,2.5,5), med.3jobcost.2, para2=c(1,1,1),udat=udat,
            method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
# user  system elapsed 
# 25.18   5.59 30.76 

$par
[1] 0.8990952 2.2620256 3.6077687

$value
[1] 2.447102



system.time(opt3.1<-optim(c(2,5,10), med.3jobcost.2, para2=c(1,1,1),udat=udat,
              method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
user  system elapsed 
33.08  7.26   40.39 

$par
[1] 0.8903403 2.2231586 3.5986968

$value
[1] 2.446836

# relative stable, maybe there are local minimum

#===================
cat("unequal cost for each job\n")
# (alp1,alp2,alp3) = (1,2,3)
system.time(opt3.1 <- optim(c(1,1.5,2), med.3jobcost.2, para2=c(1,2,3),udat=udat,
              method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

user  system elapsed 
30.25  6.79   37.17

opt3.1
$par
[1] 1.327808 2.635621 3.811327

$value
[1] 5.180722

system.time(opt3.1 <- optim(c(1,2.5,5), med.3jobcost.2, para2=c(1,2,3),udat=udat,
              method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

user  system elapsed 
26.78  5.91   32.81  

$par
[1] 1.342228 2.652612 3.821804

$value
[1] 5.180727


system.time(opt3.1 <- optim(c(1,5,10), med.3jobcost.2, para2=c(1,2,3),udat=udat,
                method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))

user  system elapsed 
32.25  7.19   39.53

$par
[1] 1.332104 2.662158 3.851997

$value
[1] 5.182172

#=========================================
#check that (1.328,2.636,3.811) is indeed the optimal appointment time
#compare with 5.1807
set.seed(123)
test <- numeric(1000)
for(i in 1:1000)
{
  A2 <- runif(1,min=0,max=2)
  A3 <- runif(1,min=A2,max=3)
  A4 <- runif(1,min=A3,max=4.5)
  test[i] <- med.3jobcost.2(c(A2,A3,A4),para2=c(1,2,3),udat=udat)
}
min(test)
#5.1989 greater than 5.1807.







