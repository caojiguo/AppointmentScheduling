#equal case
#cdf function
p.equcost= function (q, A2,para)
{
  alp <- para[1]
  mu <- para[2]
  sig <- para[3]
  
  term1 <- pnorm(log(q/alp+A2),mean=mu,sd=sig)
  term2 <- pnorm(log(pmax(A2-q/alp,0)),mean=mu,sd=sig)
  pmax(term1 - term2,0)
}


#some checks for the correctness of the last few functions
# p.equcost(0,A2=1,para=c(1,0,1))
# # 0
# p.equcost(Inf,A=1,para=c(1,0,1))
# # 1
# # checks
# p.equcost(c(0.1,0.5,1,3,5,9),A2=1,para=c(1,0,1))
# 0.0799209 0.4133236 0.7558914 0.9171715 0.9634142 0.9893489


#density function of total costs with equal coefficients
den.equcost <- function(x,A2,para)
{
  alp <- para[1]
  mu <- para[2]
  sig <- para[3]
  den <- numeric(length(x))
  for(i in 1:length(x))
  {
    if(A2 > x[i]/alp)
    { term1 <- dnorm(log(A2+x[i]/alp),mean=mu,sd=sig)/(alp*A2+x[i])
      term2 <- dnorm(log(A2-x[i]/alp),mean=mu,sd=sig)/(alp*A2-x[i])
      den[i] <- term1+term2
    } else {
      term1 <- dnorm(log(A2+x[i]/alp),mean=mu,sd=sig)/(alp*A2+x[i])
      den[i] <- term1
    }
  }
  den
}




#====================================
#cat("unequal cost coefficients\n")
#cdf (cumulative distribution function)
p.unequcost= function (q, A2,para)
{
  alp <- para[1]
  beta <- para[2]
  mu <- para[3]
  sig <- para[4]
  term1 <- plnorm(q/alp+A2,meanlog=mu,sdlog=sig)-plnorm(A2,meanlog=mu,sdlog=sig)
  term2 <- plnorm(A2,meanlog=mu,sdlog=sig)-plnorm((pmax(A2-q/beta,0)),meanlog=mu,sdlog=sig)
  term1+term2
 
}
#check the correctness
# p.unequcost(0,A2=1,para=c(1,1,0,1))
# #0
# p.unequcost(Inf,A2=1,para=c(1,1,0,1))
# #1
# p.unequcost(c(0.1,0.5,1,3,5,9),A2=1,para=c(1,1,0,1))
#0.0799209 0.4133236 0.7558914 0.9171715 0.9634142 0.9893489


den.unequcost <- function(x,A2,para)
{
  alp <- para[1]
  beta <- para[2]
  mu <- para[3]
  sig <- para[4]
  den <- numeric(length(x))
  for(i in 1:length(x))
  {
    if(A2 > x[i]/beta)
    { term1 <- dlnorm(A2+x[i]/alp,meanlog=mu,sdlog=sig)/alp
      term2 <- dlnorm(A2-x[i]/beta,meanlog=mu,sdlog=sig)/beta
      den[i] <- term1+term2
    } else {
      term1 <- dlnorm(A2+x[i]/alp,meanlog=mu,sdlog=sig)/alp
      den[i] <- term1
    }
  }
  den
}

# integrate(den.unequcost,0,1,A2=1,para=c(1,1,0,1))$value
# #0.7558914
# integrate(den.unequcost,0,0.1,A2=1,para=c(1,1,0,1))$value
# #0.0799209
# integrate(den.unequcost,0,9,A2=1,para=c(1,1,0,1))$value
# 0.9893489
dif <- function(x,p,A2,para)
{
  p.unequcost(x,A2,para)-p
}

q.unequcost <- function(A2,p,para)
{
  if(A2<0 | A2==0) return(1e10)
  else{
       alp <- para[1]
       beta <- para[2]
       mu <- para[3]
      sig <- para[4]
       q <- rep(NA,length(p))
      for(i in 1:length(p))
   {
    if(p[i] > p.unequcost(A2*beta,A2,para)|p[i]==p.unequcost(A2*beta,A2,para)) 
          q[i] <- alp*(qlnorm(p[i],meanlog=mu,sdlog=sig)-A2)
    else q[i] <- uniroot(dif,c(0,A2*beta),p=p[i],A2=A2,para=para)$root
   }
   q
  }
}
# q.unequcost(0.7558914,A2=1,para=c(1,1,0,1))
# #1
# q.unequcost(0.0799209,A2=1,para=c(1,1,0,1))
# #0.09998056
# q.unequcost(0.9893489,A2=1,para=c(1,1,0,1))
# #9
# q.unequcost(c(0.0799209,0.4133236,0.7558914,0.9171715,0.9634142,0.9893489),
#               A2=1,para=c(1,1,0,1))


#=============================
#calculate mean of total cost
integrand <- function(x,A2,para)
{
  x*den.unequcost(x,A2,para)
}


mean.unequcost <- function(A2,para)
{
  alp <- para[1]
  beta <- para[2]
  mu <- para[3]
  sig <- para[4]
  term <- integrate(integrand,lower=0,upper=Inf,A2=A2,para=para)$value
  term
}

#mean.unequcost(A2=1,para=c(1,1,0,1))
# #1.125565
# 
# X <- rlnorm(10^4,meanlog=0,sdlog=1)
# mean(abs(X-1))
# #1.103895
# exp(1/2)*(pnorm(-1,lower.tail=F)-pnorm(-1))
#1.125565