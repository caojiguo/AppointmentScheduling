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
  median(term1+term2)
}


#generating data
set.seed(123456)
n <- 1000000
mu <- 0
sig <- 1
u1 <- exp(rnorm(n,mean=mu,sd=sig))
u2 <- exp(rnorm(n,mean=mu,sd=sig))
udat <- cbind(u1,u2)

data = data.frame(
  x = seq(0.1,0.9,length=20),
  y = seq(1,1.8,length=20)
)


# 3d-plot of the function obj2
# 2-dimensional function of A2 and A3
surface1 <- matrix(NA, nrow=length(data$x),ncol=length(data$y))
for(i in 1:length(data$x))
{
  for(j in 1:length(data$y))
  {
  surface1[i,j] <- med.2jobcost.2(c(data$x[i],data$y[j]), para2=c(1,1),udat=udat)
  }
}



library(rgl)  
open3d()
persp3d(x = data$x, y = data$y, 
        z = surface1 ,col = "orange", xlab="A2", ylab="A3", 
        zlab="median")

#=======================
#optimization
#equal cost
system.time(opt3<-optim(c(1,1.5), med.2jobcost.2, para2=c(1,1),udat=udat,
        method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
# user  system elapsed 
# 8.69    1.25   9.95
# c(1,1.5)
# $par
# [1] 0.7484627 1.9580642
# 
# $value
# [1] 1.34833

system.time(opt3<-optim(c(1,2.5), med.2jobcost.2, para2=c(1,1),udat=udat,
        method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=0)))
# user  system elapsed 
# 8.67    1.41   10.08 

# c(1,2.5)
# $par
# [1] 0.745637 1.957721
# 
# $value
# [1] 1.348361

system.time(opt3<-optim(c(3,10), med.2jobcost.2, para2=c(1,1),udat=udat,
            method = "Nelder-Mead",hessian=F,control=list(maxit=500,trace=2)))
# c(3,10)
# user  system elapsed 
# 8.28    1.36    9.65  
# $par
# [1] 0.7442621 1.9620857
# 
# $value
# [1] 1.348345


#=============================
#check that (0.748, 1.958) is indeed the optimal appointment time
#compare with 1.348
set.seed(123)
test <- numeric(1000)
for(i in 1:1000)
{
  A2 <- runif(1,min=0,max=3)
  A3 <- runif(1,min=A2,max=4)
  test[i] <-  med.2jobcost.2(c(A2,A3), para2=c(1,1),udat=udat)
  
}
min(test)
#1.348689 
#greater than 1.34833






