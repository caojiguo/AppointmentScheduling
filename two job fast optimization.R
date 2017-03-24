
setwd("C:/Users/perry/Dropbox/ASP")
dyn.load("median.dll")

med.2jobcost.2 <- function(para1,para2,udat)
{
  A2 <- para1[1]
  A3 <- para1[2]
  alp1 <- para2[1]
  alp2 <- para2[2]
  if (A2 < 0 || A2 ==0 || A2 > A3 || A2 == A3) return (1.e10)
  u1 <- udat[,1]
  u2 <- udat[,2]
  term1 <- alp1*abs(u1-A2)
  term2 <- alp2*abs(pmax(u1,A2)+u2-A3)
  out <- .C("binapprox",n=as.integer(length(term1+term2)),x= as.double
              (term1+term2), result = as.double(0.0))
  out[[3]] 
}


set.seed(123456)
n <- 1000000
mu <- 0
sig <- 1
u1 <- exp(rnorm(n,mean=mu,sd=sig))
u2 <- exp(rnorm(n,mean=mu,sd=sig))
udat <- cbind(u1,u2)

#===============
# 3d plot 
data = data.frame(
  x = seq(0.1,0.9,length=40),
  y = seq(1,1.8,length=40)
)


# 3d-plot of the function obj2
# 2-dimensional function of A2 and A3
surface1 <- matrix(NA, nrow=length(data$x),ncol=length(data$y))
for(i in 1:length(data$x))
{
  for(j in 1:length(data$y))
  {
    surface1[i,j] <- med.2jobcost.2(c(data$x[i],data$y[j]),c(1,1),udat)   
    }
}
  


library(rgl)  
open3d()
persp3d(x = data$x, y = data$y, 
        z = surface1 ,col = "orange", xlab="A2", ylab="A3", 
        zlab="median")


#==================
# optimization
opt3 <- optim(c(1,1.5), med.2jobcost.2, para2=c(1,1),udat=udat,
          method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2))
# c(1, 1.5)
$par
[1] 0.7798812 1.9594949

$value
[1] 1.344748

system.time(opt3<-optim(c(1,2.5), med.2jobcost.2, para2=c(1,1),udat=udat,
          method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))

 user  system elapsed 
 6.84    1.07    7.92 
# c(1, 2.5)
$par
[1] 0.7442728 1.9519698

$value
[1] 1.344249

system.time(opt3<-optim(c(3,10), med.2jobcost.2, para2=c(1,1),udat=udat,
        method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))
 user  system elapsed 
 6.95    1.12    8.08

$par
[1] 0.7588428 1.9836185

$value
[1] 1.344503




