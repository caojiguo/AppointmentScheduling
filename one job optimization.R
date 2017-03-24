# generate random samples of the total costs of one job
# with equal cost coefficents, duration time ~ logNormal
# arguments: alp is the cost coefficient, mu and sigma are 
# parameters in logNormal distribution
r.equcost <- function(n,A2,para)
{
  alp <- para[1]
  mu <- para[2]
  sig <- para[3]
  Z <- rnorm(n, mean=mu, sd=sig)
  Y <- alp*abs(exp(Z)-A2)
  Y
  
}


# density function of total costs with equal coefficients
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

source("one-job density.r")
#=============================================
#Figure 1
cat("distribution of the total costs\n")
windows()
pdf("distribution of overall costs.pdf")
par(mfrow=c(2,2),cex.lab=1.3,font.lab=2,font.main=2)
hist(r.equcost(100000,A2=0.1,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,0.1))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=0.1,para=c(1,0,1)),from=0.001,to=15,col="black",add=T)
# hist(r.equcost(100000,A2=0.5,para=c(1,0,1)),main=expression(paste("A2 = 0.5, ", alpha, "= 1")),
#      freq=F,breaks=1000,xlab="x",xlim=c(0,15))
# curve(den.equcost(x,A2=0.5,para=c(1,0,1)),from=0.001,to=15,col="brown",add=T)
hist(r.equcost(100000,A2=1,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,1))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=1,para=c(1,0,1)),from=0.001,to=15,col="violet",add=T)
hist(r.equcost(100000,A2=3,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,3))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=3,para=c(1,0,1)),from=0.001,to=15,col="blue",add=T)
# hist(r.equcost(100000,A2=5,para=c(1,0,1)),main=expression(paste("A2 = 5, ", alpha, "= 1")),
#      freq=F,breaks=1000,xlab='x',xlim=c(0,15))
# curve(den.equcost(x,A2=5,para=c(1,0,1)),from=0.001,to=15,col="green",add=T)
hist(r.equcost(100000,A2=10,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,10))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=10,para=c(1,0,1)),from=0.001,to=15,col="orange",add=T)
dev.off()


#tiff format
tiff(file = "1.tiff", width = 480, height = 480,
       units = "px")
par(mfrow=c(2,2),cex.lab=1.3,font.lab=2,font.main=2)
hist(r.equcost(100000,A2=0.1,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,0.1))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=0.1,para=c(1,0,1)),from=0.001,to=15,col="black",add=T)
# hist(r.equcost(100000,A2=0.5,para=c(1,0,1)),main=expression(paste("A2 = 0.5, ", alpha, "= 1")),
#      freq=F,breaks=1000,xlab="x",xlim=c(0,15))
# curve(den.equcost(x,A2=0.5,para=c(1,0,1)),from=0.001,to=15,col="brown",add=T)
hist(r.equcost(100000,A2=1,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,1))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=1,para=c(1,0,1)),from=0.001,to=15,col="violet",add=T)
hist(r.equcost(100000,A2=3,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,3))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=3,para=c(1,0,1)),from=0.001,to=15,col="blue",add=T)
# hist(r.equcost(100000,A2=5,para=c(1,0,1)),main=expression(paste("A2 = 5, ", alpha, "= 1")),
#      freq=F,breaks=1000,xlab='x',xlim=c(0,15))
# curve(den.equcost(x,A2=5,para=c(1,0,1)),from=0.001,to=15,col="green",add=T)
hist(r.equcost(100000,A2=10,para=c(1,0,1)),
     main=expression(bold(paste(A[2], "= " ,10))),
     freq=F,breaks=1000,xlab=expression(bold(Y[1])),xlim=c(0,15),
     ylab="")
mtext(side=2,text="probability density function",line=2.5,font=2,cex=1.1)
curve(den.equcost(x,A2=10,para=c(1,0,1)),from=0.001,to=15,col="orange",add=T)

dev.off()
#=====================
#mean of total cost of one job with equal cost
f <- function(x,A2,para) x*den.equcost(x,A2,para)
mean.equcost = function(A2,para)
{
  integrate(f,lower=0,upper=Inf,A2=A2,para=para)$value
}

mean.equcost(0.5,para=c(2,0,1))
#===========================================
# cdf: cumulative distribution function of total costs with equal coefficients
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
p.equcost(0,A2=1,para=c(1,0,1))
# 0
p.equcost(Inf,A=1,para=c(1,0,1))
# 1
# checks
p.equcost(c(0.1,0.5,1,3,5,9),A2=1,para=c(1,0,1))
# 0.0799209 0.4133236 0.7558914 0.9171715 0.9634142 0.9893489
integrate(den.equcost,0,1,A2=1,para=c(1,0,1))$value
# 0.7558914
integrate(den.equcost,0,0.1,A2=1,para=c(1,0,1))$value
# 0.0799209
integrate(den.equcost,0,9,A2=1,para=c(1,0,1))$value
# 0.9893489

p.equcost(c(0.1,0.5,1,3,5,9),A2=3,para=c(1,0,1))
# 0.01456048 0.07461122 0.16128008 0.96341425 0.98121161 0.99352073

integrate(den.equcost,0,1,A2=3,para=c(1,0,1))$value
# 0.1612801
integrate(den.equcost,0,0.1,A2=3,para=c(1,0,1))$value
# 0.01456048
integrate(den.equcost,0,9,A2=3,para=c(1,0,1))$value
# 0.9935206


#====================================
cat("objective funtion")

# objective function: the quantile function of the overall cost
# with equal idling and waiting cost
# argument: q denotes qth quantile, x is used to express the
# difference of two quantiles, para include coefficent, mu and sigma
obj.quantile.equcost <- function(q,x,para)
{
  alp <- para[1]
  mu <- para[2]
  sig <- para[3]
  if (x < 0 || x > (1-q)) return (1.e10)
  upp <- exp(qnorm(x+q)*sig + mu)
  low <- exp(qnorm(x)*sig + mu)
  upp - low
}



#approximated version using sample
# median rather than theoretical quantile

obj.quantile.equcost.app <- function(q,x,dat)
{
  
  if (x < 0 || x > (1-q)) return (1.e10)
  upp <- quantile(dat,q+x)
  low <- quantile(dat,x)
  upp - low
}

cat("optimize objective function")
# optimize the objetive function and then find out the optimal
# appointment time: A2, which is the middle point of upp and low
# nlm is to find out the point which can minimize the objective function
# here is obj.quantile.equcost. 
opt.quantile.equcost <- function(start, q, par)
{
  
  alp <- par[1]
  mu <- par[2]
  sig <- par[3]
  fit <- nlm(obj.quantile.equcost, start, q=q, para=par)
  x.opt <- fit$est
  upp <- exp(qnorm(x.opt+q)*sig + mu)
  low <- exp(qnorm(x.opt)*sig + mu)
  (upp + low)/2
}

#approximated version
opt.quantile.equcost.app <- function(start,q,data)
{
  # start=0.1; q=0.5; par=c(2,0,1); n=10000000
  fit <- nlm(obj.quantile.equcost.app, start, q=q,dat=data)
  x.opt <- fit$est
  upp <- quantile(data,q+x.opt)
  low <- quantile(data,x.opt)
  (upp + low)/2
}
#=============================================
#calculate the optimal appointment time 
# here we specify mu=0, sigma=1, actually, more
# general case could be considered here.
# (we just make use of this function to check whether the function for one job optimization 
# with equal cost will converge or not).
appoint <- function(x,q)
{
  sig <- 1; mu <- 0;
  upp <- exp(qnorm(x+q)*sig + mu)
  low <- exp(qnorm(x)*sig + mu)
  (upp+low)/2
}


fit1 <- nlm(obj.quantile.equcost, 0.1, q=0.5, para=c(2,0,1),print.level=2)
iter <- 0:fit1$iterations
med.5 <- 2*c(1.010724,0.9820691,0.9660502,0.9539989,0.9451941, 0.938923,
           0.9345161,0.9240467,0.9236983,0.9231975,0.923179,0.9231786,
           0.9231786)
appo.5 <- appoint(c(0.1,0.08247126,0.07131196,0.06191002,0.05419567, 0.04801224,
                0.04311729,0.02569862, 0.01632267,0.02093285, 0.02026741,
                0.02015198,0.02015519),q=0.5)

fit2 <- nlm(obj.quantile.equcost, 0.1, q=0.75, para=c(2,0,1),print.level=2)
iter2 <- 0:fit2$iterations
med.75 <- 2*c(2.541538,2.436785,2.352052,2.27207,2.198351,2.131676,2.073143,2.024077,1.985746,1.958651,
              1.941632,1.931919,1.926542,1.923461,1.921587,1.917872,1.917704,1.91744, 1.917429,1.917429,1.917429)
appo.75 <- appoint(c(0.1,0.08949067, 0.08010438,0.07038066,0.06052801, 0.05071299,0.04119394,0.03233588, 0.02459635,0.01841224,
                    0.01396805,0.01104158, 0.009170015,0.007937957,0.007082941, 0.004615729,0.002920348,0.0037699, 0.003640147,
                    0.00361526,0.003616081),q=0.75)

#======================
#Figure 2
windows()
pdf("convergence check.pdf",width=9)
par(mfrow=c(2,2),cex.lab=1.3,font.lab=2,lwd=3,cex=1.2,mar=c(4,4,3,2)+0.1)
plot(iter,med.5,type="l",ylab="",xlab="Iterations",ylim=c(1.82,2.05))
mtext(side=2,text="median",line=2.5,cex=1.2,font=2)
points(iter,med.5,pch=4)
plot(iter,appo.5,type="l",ylab="",xlab="Iterations",
     ylim=c(0.56,0.81))
mtext(side=2,text="appointment time",line=2.5,cex=1.2,font=2)
points(iter,appo.5,pch=4)
plot(iter2,med.75,type="l",ylab="",xlab="Iterations",
     ylim=c(3.72,5.11))
mtext(side=2,text="75% percentile",line=2.5,cex=1.2,font=2)
points(iter2, med.75,pch=4)
plot(iter2,appo.75,type="l",ylab="",xlab="Iterations",
     ylim=c(0.95,1.56))
mtext(side=2,text="appointment time",line=2.5,cex=1.2,font=2)
points(iter2,appo.75,pch=4)
dev.off()

#tiff format
tiff(file = "temp.tiff", width = 480, height = 480, 
     units = "px"); 
par(mfrow=c(2,2),cex.lab=1.3,font.lab=2,lwd=3,cex=1.2,mar=c(4,4,3,2)+0.1)
plot(iter,med.5,type="l",ylab="",xlab="Iterations",ylim=c(1.82,2.05))
mtext(side=2,text="median",line=2.5,cex=1.2,font=2)
points(iter,med.5,pch=4)
plot(iter,appo.5,type="l",ylab="",xlab="Iterations",
     ylim=c(0.56,0.81))
mtext(side=2,text="appointment time",line=2.5,cex=1.2,font=2)
points(iter,appo.5,pch=4)
plot(iter2,med.75,type="l",ylab="",xlab="Iterations",
     ylim=c(3.72,5.11))
mtext(side=2,text="75% percentile",line=2.5,cex=1.2,font=2)
points(iter2, med.75,pch=4)
plot(iter2,appo.75,type="l",ylab="",xlab="Iterations",
     ylim=c(0.95,1.56))
mtext(side=2,text="appointment time",line=2.5,cex=1.2,font=2)
points(iter2,appo.75,pch=4)
dev.off()
#=========================
cat("plot of difference of quantile of duration time\n")

# plot for which probability (x) that miminize the difference of 
# the quantile function Q(x+q) - Q(x)
#=================================================
#Figure 3
windows()
pdf("optimal quantile plot.pdf")
par(mfrow=c(2,3),cex.lab=1.5,lwd=3,font.lab=2)
# curve(obj.quantile.equcost(q=.1, x, para=c(2,4,1)), from = 0.01, to = 0.89, ylab="quantile")
curve(obj.quantile.equcost(q=.25, x, para=c(2,4,1)), from = 0.01, to = 0.2, 
    ylab="",xlab="x",main = "q = 0.25")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.3, x, para=c(2,4,1)), from = 0.01, to = 0.69, ylab="quantile")
curve(obj.quantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.01, to = 0.04, 
      ylab="", xlab="x",main = "q = 0.5")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.01, to = 0.49, ylab="quantile")
curve(obj.quantile.equcost(q=.75, x, para=c(2,4,1)), from = 0.001, to = 0.01, 
    ylab="", xlab="x", main = "q = 0.75")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.7, x, para=c(2,4,1)), from = 0.001, to = 0.29, ylab="quantile")
curve(obj.quantile.equcost(q=.9, x, para=c(2,4,1)), from = 3e-4, to = 0.0008, 
      ylab="", xlab="x",main = "q = 0.9")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)

# curve(obj.quantile.equcost(q=.9, x, para=c(2,4,1)), from = 0.001, to = 0.09, ylab="quantile")
curve(obj.quantile.equcost(q=.95, x, para=c(2,4,1)), from = 0.0001, to = 2e-4, 
      ylab="",xlab="x",main = "q = 0.95")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)

curve(obj.quantile.equcost(q=.99,x,para=c(2,4,1)),from=4e-6,to=1e-5,
    ylab="",xlab="x",main="q=0.99")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
dev.off()

#tiff format
tiff(file = "3.tiff", width = 480, height = 480, 
     units = "px"); 
par(mfrow=c(2,3),cex.lab=1.5,lwd=3,font.lab=2)
# curve(obj.quantile.equcost(q=.1, x, para=c(2,4,1)), from = 0.01, to = 0.89, ylab="quantile")
curve(obj.quantile.equcost(q=.25, x, para=c(2,4,1)), from = 0.01, to = 0.2, 
      ylab="",xlab="x",main = "q = 0.25")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.3, x, para=c(2,4,1)), from = 0.01, to = 0.69, ylab="quantile")
curve(obj.quantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.01, to = 0.04, 
      ylab="", xlab="x",main = "q = 0.5")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.01, to = 0.49, ylab="quantile")
curve(obj.quantile.equcost(q=.75, x, para=c(2,4,1)), from = 0.001, to = 0.01, 
      ylab="", xlab="x", main = "q = 0.75")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
# curve(obj.quantile.equcost(q=.7, x, para=c(2,4,1)), from = 0.001, to = 0.29, ylab="quantile")
curve(obj.quantile.equcost(q=.9, x, para=c(2,4,1)), from = 3e-4, to = 0.0008, 
      ylab="", xlab="x",main = "q = 0.9")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)

# curve(obj.quantile.equcost(q=.9, x, para=c(2,4,1)), from = 0.001, to = 0.09, ylab="quantile")
curve(obj.quantile.equcost(q=.95, x, para=c(2,4,1)), from = 0.0001, to = 2e-4, 
      ylab="",xlab="x",main = "q = 0.95")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)

curve(obj.quantile.equcost(q=.99,x,para=c(2,4,1)),from=4e-6,to=1e-5,
      ylab="",xlab="x",main="q=0.99")
mtext(side=2,text="Q(x+q) - Q(x)",font=2,line=2.3,cex=1.1)
dev.off()

#let's compare it with the case when the underlying distribution is symmetric
# take normal distribution as an example
obj.normquantile.equcost <- function(q,x,para)
{
  alp <- para[1]
  mu <- para[2]
  sig <- para[3]
  if (x < 0 || x > (1-q)) return (1.e10)
  upp <- qnorm(x+q)*sig + mu
  low <- qnorm(x)*sig + mu
  upp - low
}




windows()
par(mfrow=c(2,3))
# curve(obj.quantile.equcost(q=.1, x, para=c(2,4,1)), from = 0.01, to = 0.89, ylab="quantile")
curve(obj.normquantile.equcost(q=.1, x, para=c(2,4,1)), from = 0.3, to = 0.5, ylab="difference",
      xlab="probability",main = "q = 0.1")
# curve(obj.quantile.equcost(q=.3, x, para=c(2,4,1)), from = 0.01, to = 0.69, ylab="quantile")
curve(obj.normquantile.equcost(q=.3, x, para=c(2,4,1)), from = 0.1, to = 0.6, ylab="difference",
      xlab="probability",main = "q = 0.3")
# curve(obj.quantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.01, to = 0.49, ylab="quantile")
curve(obj.normquantile.equcost(q=.5, x, para=c(2,4,1)), from = 0.1, to = 0.4, ylab="difference",
      xlab="probability", main = "q = 0.5")
# curve(obj.quantile.equcost(q=.7, x, para=c(2,4,1)), from = 0.001, to = 0.29, ylab="quantile")
curve(obj.normquantile.equcost(q=.7, x, para=c(2,4,1)), from = 0.1, to = 0.2, ylab="difference",
      xlab="probability",main = "q = 0.7")

# curve(obj.quantile.equcost(q=.9, x, para=c(2,4,1)), from = 0.001, to = 0.09, ylab="quantile")
curve(obj.normquantile.equcost(q=.9, x, para=c(2,4,1)), from = 0.001, to = 0.099, ylab="difference",
      xlab="probability",main = "q = 0.9")







#===============================
prob <- seq(0.1, 0.8, by=0.01)

#check the change of optimal time against prob with different \sigma, different \mu,
#and different cost
app2 <- app3<- app4 <- app5 <- app6 <- app7 <-numeric(length(prob))
for(i in 1:length(app2))
{
app2[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0,1))
app3[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0,0.7))
app4[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0,0.5))
app5[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0,1))
app6[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0.3,1))
app7[i] <- opt.quantile.equcost(start=0.1, prob[i], par=c(2,0.5,1))
}

#===============
cat("median against mu\n")
mu0 <- seq(-1,1,by=.01)
app.mu.5 <- app.mu.75 <- app.mu.95 <- numeric(length(mu0))
for(i in 1:length(mu0))
{
  app.mu.5[i] <- opt.quantile.equcost(start=0.1, 0.5, par=c(2,mu0[i],1))
  app.mu.75[i] <- opt.quantile.equcost(start=0.1, 0.75, par=c(2,mu0[i],1))
  app.mu.95[i] <- opt.quantile.equcost(start=0.001, 0.95, par=c(2,mu0[i],1))
  
}

#====================================
#\alpha=2, \sigma = 1
#Figure 4
windows()
pdf("optimal quantile appointment time over different mu.pdf",width=12)
par(mfrow=c(1,2),font.lab=2,cex.lab=1.4)
plot(x=prob,y=app5,type="l", xlab = expression(q), 
     ylab="",
     col="red",lty=1,lwd=3)
mtext(text="optimal appointment time",side=2,line=2.3,font=2,cex=1.3)

points(x=prob,y=app6,type="l",col="blue",lty=2,lwd=3)

points(x=prob,y=app7,type="l",col="black",lty=4,lwd=3)
legend(0.13,1.15,legend=c(expression(paste(mu, "=0 ")), 
                        expression(paste(mu, "=0.3 ")),
                        expression(paste( mu, "=0.5 "))),
     bty="n",lty=c(1,2,4),col=c("red","blue","black"),lwd=rep(3,3))

plot(x=mu0,y=app.mu.5,type="l", xlab = expression(bold(mu)), 
     ylab = "",col="red",lty=1,lwd=3)
#main = expression(paste("optimal appointment time against " ,mu, " with ", alpha, " =2 ", sigma, " =1")),
mtext(text="optimal appointment time",side=2,line=2.3,font=2,cex=1.3)
points(x=mu0,y=app.mu.75,type="l",col="blue",lty=2,lwd=3)
points(x=mu0,y=app.mu.95,type="l",col="black",lty=4,lwd=3)

legend(.0,.4,legend=c("q=0.5", "q=0.75", "q=0.95"),bty="n",
       col=c("red","blue","black"),lty=c(1,2,4),lwd=3)
dev.off()

#tiff format
tiff(file = "4.tiff", width = 960, height = 480, 
     units = "px"); 
par(mfrow=c(1,2),font.lab=2,cex.lab=1.4)
plot(x=prob,y=app5,type="l", xlab = expression(q), 
     ylab="",
     col="red",lty=1,lwd=3)
mtext(text="optimal appointment time",side=2,line=2.3,font=2,cex=1.3)

points(x=prob,y=app6,type="l",col="blue",lty=2,lwd=3)

points(x=prob,y=app7,type="l",col="black",lty=4,lwd=3)
legend(0.13,1.15,legend=c(expression(paste(mu, "=0 ")), 
                          expression(paste(mu, "=0.3 ")),
                          expression(paste( mu, "=0.5 "))),
       bty="n",lty=c(1,2,4),col=c("red","blue","black"),lwd=rep(3,3))

plot(x=mu0,y=app.mu.5,type="l", xlab = expression(bold(mu)), 
     ylab = "",col="red",lty=1,lwd=3)
#main = expression(paste("optimal appointment time against " ,mu, " with ", alpha, " =2 ", sigma, " =1")),
mtext(text="optimal appointment time",side=2,line=2.3,font=2,cex=1.3)
points(x=mu0,y=app.mu.75,type="l",col="blue",lty=2,lwd=3)
points(x=mu0,y=app.mu.95,type="l",col="black",lty=4,lwd=3)

legend(.0,.4,legend=c("q=0.5", "q=0.75", "q=0.95"),bty="n",
       col=c("red","blue","black"),lty=c(1,2,4),lwd=3)
dev.off()

#==========================
# median against sigma
sigma0 <- seq(0.5,1.5,by=.01)
app.sigma.5 <- app.sigma.75 <- app.sigma.95 <- numeric(length(sigma0))
for(i in 1:length(sigma0))
{
  app.sigma.5[i] <- opt.quantile.equcost(start=0.1, 0.5, par=c(2,0,sigma0[i]))
  app.sigma.75[i] <- opt.quantile.equcost(start=0.1, 0.75, par=c(2,0,sigma0[i]))
  app.sigma.95[i] <- opt.quantile.equcost(start=0.01, 0.95, par=c(2,0,sigma0[i]))
}
#==================================
#Figure 5
#\mu = 0, \alpha = 2
windows()
pdf("optimal quantile appointment time over different sigma.pdf",width=12)
par(mfrow=c(1,2),cex.lab=1.4,font.lab=2)
plot(x=prob,y=app2,type="l", xlab = expression(bold(q)), 
     ylab = "",col="red",lty=1,lwd=3)
#main = expression(paste("optimal appointment time against quantile q with different " ,sigma)),
mtext(text="optimal appoitment time",side=2,line=2.3,font=2,cex=1.3)
points(x=prob,y=app3,type="l",col="blue",lty=2,lwd=3)

points(x=prob,y=app4,type="l",col="black",lty=4,lwd=3)
legend(0.2,1.1,bty="n",legend=c(expression(paste(sigma, "=1")), 
                        expression(paste(sigma, "=0.7")),
                        expression(paste(sigma, "=0.5"))),
       lty=c(1,2,4),col=c("red","blue","black"),lwd=rep(3,3))

plot(x=sigma0,y=app.sigma.5,type="l", xlab = expression(bold(sigma)), 
     ylab = "",
     col="red",lty=1,ylim=c(0.5,2),lwd=3)
#main = expression(paste("optimal appointment time against " ,sigma, ", ", alpha, "=2, ", mu, "=0")),
mtext(text="optimal appoitment time",side=2,line=2.3,font=2,cex=1.3)    
points(x=sigma0,y=app.sigma.75,type="l",lty=2,lwd=3,col="blue")
points(x=sigma0,y=app.sigma.95,type="l",lty=4,lwd=3,col="black")
legend(1.0,0.9,bty="n",legend=c("q=0.5", "q=0.75", "q=0.95"),lty=c(1,2,4),
       col=c("red","blue","black"),lwd=3)
dev.off()

#tiff format
tiff(file = "5.tiff", width = 960, height = 480, 
     units = "px"); 
par(mfrow=c(1,2),cex.lab=1.4,font.lab=2)
plot(x=prob,y=app2,type="l", xlab = expression(bold(q)), 
     ylab = "",col="red",lty=1,lwd=3)
#main = expression(paste("optimal appointment time against quantile q with different " ,sigma)),
mtext(text="optimal appoitment time",side=2,line=2.3,font=2,cex=1.3)
points(x=prob,y=app3,type="l",col="blue",lty=2,lwd=3)

points(x=prob,y=app4,type="l",col="black",lty=4,lwd=3)
legend(0.2,1.1,bty="n",legend=c(expression(paste(sigma, "=1")), 
                                expression(paste(sigma, "=0.7")),
                                expression(paste(sigma, "=0.5"))),
       lty=c(1,2,4),col=c("red","blue","black"),lwd=rep(3,3))

plot(x=sigma0,y=app.sigma.5,type="l", xlab = expression(bold(sigma)), 
     ylab = "",
     col="red",lty=1,ylim=c(0.5,2),lwd=3)
#main = expression(paste("optimal appointment time against " ,sigma, ", ", alpha, "=2, ", mu, "=0")),
mtext(text="optimal appoitment time",side=2,line=2.3,font=2,cex=1.3)    
points(x=sigma0,y=app.sigma.75,type="l",lty=2,lwd=3,col="blue")
points(x=sigma0,y=app.sigma.95,type="l",lty=4,lwd=3,col="black")
legend(1.0,0.9,bty="n",legend=c("q=0.5", "q=0.75", "q=0.95"),lty=c(1,2,4),
       col=c("red","blue","black"),lwd=3)
dev.off()
#==========================
#study whether the optimization procedure is stable
#with different chocies of starting points
#use some examples to show that when the starting points changes,
#tiny change in the corresponding optimal appointment time

#=======================
#Table 1
opt.apptime <- matrix(NA,ncol=4,nrow=4)
opt.apptime[1,-1] <- c(0.5,0.75,0.95)
opt.apptime[-1,1] <- c(1e-2, 1e-3, 1e-4)
opt.apptime
for(i in 2:4)
  for(j in 2:4)
opt.apptime[i,j] <- opt.quantile.equcost(start=opt.apptime[i,1],
                                         q=opt.apptime[1,j],par=c(2,0,1))

# opt.quantile.equcost(start=0.1,
#                      q=0.75,par=c(2,0,1))
print(opt.apptime)
#=========================================
#comparison of two optimal opintment time: one is to minimize median of total
# cost and the other is to minimize the mean of total cost.
median.optA2 <- opt.quantile.equcost(start=0.1,q=0.5,par=c(2,0,1))
# 0.5902529

mean.optA2 <- nlm(mean.equcost,p=0.5,para=c(2,0,1))
# 1.000022


#employ another alpha
median.optA2 <- opt.quantile.equcost(start=0.1,q=0.5,par=c(5,0,1))
# 0.5902529
mean.optA2 <- nlm(mean.equcost,p=0.5,para=c(5,0,1))
# 1.000009

#mean > standard deviation
median.optA2 <- opt.quantile.equcost(start=0.1,q=0.5,par=c(2,4,1))
#32.22671
mean.optA2 <- nlm(mean.equcost,p=0.5,para=c(2,4,1))
#54.59775

#small varibility of lognormal distribution
median.optA2 <- opt.quantile.equcost(start=0.1,q=0.5,par=c(2,1,0.01))
#2.718072
mean.optA2 <- nlm(mean.equcost,p=0.5,para=c(2,1,0.01))
#0.5
#==================================
cat("comparison of sampling method with exact optimization\n")
# compare theorectical miminizer and sample-based minimizer
# the set up is mu=0, sigma=1, alpha=2 

# compare median
A.opt.real.med <- opt.quantile.equcost(start=0.1, 0.5, par=c(2,0,1)); A.opt.real.med
# 0.5902529
A.opt.real.low <- opt.quantile.equcost(start=0.1, 0.2, par=c(2,0,1)); A.opt.real.low
# 0.3997747
A.opt.real.upp <- opt.quantile.equcost(start=0.1, 0.8, par=c(2,0,1)); A.opt.real.upp
# 1.198141


set.seed(2389)
#with different choice of seeds, the corresponding results
#might be changed a little bit, but within the sampling variablity.
n <- 1000; mu <-0; sig <- 1
vec <- rlnorm(n, meanlog = mu, sdlog = sig)
# n=1000
A.opt.app.med <- opt.quantile.equcost.app(start=0.001, 0.5, data=vec); A.opt.app.med
# 0.6662777 
A.opt.app.low <- opt.quantile.equcost.app(start=0.1, 0.2, data=vec); A.opt.app.low
# 0.446384 
A.opt.app.upp <- opt.quantile.equcost.app(start=0.001, 0.8, data=vec); A.opt.app.upp
# 1.250492 



#n=100
A.opt.app.med <- opt.quantile.equcost.app(start=0.04, 0.5, data=vec); A.opt.app.med
#0.6580249
A.opt.app.low <- opt.quantile.equcost.app(start=0.1, 0.2, data=vec); A.opt.app.low
#0.5010178 
A.opt.app.upp <- opt.quantile.equcost.app(start=0.01, 0.8, data=vec); A.opt.app.upp
# 1.246782 



#n=10000
A.opt.app.med <- opt.quantile.equcost.app(start=0.04, 0.5,data=vec); A.opt.app.med
#0.595338 
A.opt.app.low <- opt.quantile.equcost.app(start=0.1, 0.2,data=vec); A.opt.app.low
# 0.3918486
A.opt.app.upp <- opt.quantile.equcost.app(start=0.001, 0.8,data=vec); A.opt.app.upp
# 1.228364



#==============================================
#unequal cost case
cat("optimize objective function")
obj.quantile.unequcost <- function(A2,para,dat,q)
{
  o1 <- para[1]
  u1 <- para[2]
  term1 <- o1*pmax(dat-A2,0)
  term2 <- u1*pmax(A2-dat,0)
  quantile(term1+term2,q)
}

set.seed(1234)
n <- 10000; mu <-0; sig <- 1
sim.dat <- rlnorm(n, meanlog = mu, sdlog = sig)

par1 <- c(1,2)
par2 <- c(2,1)
#the objective funtion may not differentiable, we employ optim for optimization
#comparing sampling method and theoretical optimizaiont
opt2.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par1,dat=sim.dat,q=0.2,control=list(maxit=500,trace=0))
#opt2.1$par = 0.3404488
opt2.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par2,dat=sim.dat,q=0.2,control=list(maxit=500,trace=0))
#opt2.2$par  0.443155
fit2.1 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
              para=c(par1,mu,sig),p=0.2,control=list(maxit=500,trace=1))

fit2.1$par
#0.3475496
fit2.2 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
                para=c(par2,mu,sig),p=0.2,control=list(maxit=500,trace=1))
fit2.2$par
# 0.4520467
#======================
cat("optimizing median\n")

opt5.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par1,dat=sim.dat,q=0.5,control=list(maxit=500,trace=0))
#opt5.1$par 0.447429
opt5.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par2,dat=sim.dat,q=0.5,control=list(maxit=500,trace=0))
#opt5.2$par 0.760561

fit5.1 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
                para=c(par1,mu,sig),p=0.5,control=list(maxit=500,trace=1))

fit5.1$par
#0.4364452
fit5.2 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
                para=c(par2,mu,sig),p=0.5,control=list(maxit=500,trace=1))
fit5.2$par
# 0.7441736


#=================
cat("optimizing 0.8 quantile")
opt8.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par1,dat=sim.dat,q=0.8,control=list(maxit=500,trace=0))
#opt8.1$par 0.8294358
opt8.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                para=par2,dat=sim.dat,q=0.8,control=list(maxit=500,trace=0))
# opt8.2$par  1.583693
fit8.1 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
                para=c(par1,mu,sig),p=0.8,control=list(maxit=500,trace=1))

fit8.1$par
# 0.8180401
fit8.2 <- optim(par=0.1,fn=q.unequcost,method="Brent",lower=0,upper=10,
                para=c(par2,mu,sig),p=0.8,control=list(maxit=500,trace=1))
fit8.2$par
# 1.578248

#========================
obj.mean.unequcost <- function(A2,para,dat,q)
{
  o1 <- para[1]
  u1 <- para[2]
  term1 <- o1*pmax(dat-A2,0)
  term2 <- u1*pmax(A2-dat,0)
  mean(term1+term2)
}

set.seed(1234)
n <- 1000; mu <-0; sig <- 1
sim.dat.1 <- rlnorm(n, meanlog = mu, sdlog = sig)

par1 <- c(1,5)
par2 <- c(5,1)


mean.appoptA2.1 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                    para=par1,dat=sim.dat,control=list(maxit=500,trace=0))

#0.3806392

mean.appoptA2.2 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                         para=par2,dat=sim.dat,control=list(maxit=500,trace=0))
#2.527588

median.appoptA2.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
      para=par1,dat=sim.dat,q=0.5,control=list(maxit=500,trace=0))
#0.3014261

median.appoptA2.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                           para=par2,dat=sim.dat,q=0.5,control=list(maxit=500,trace=0))
# 0.8917681

#small variation of logNormal distribution
mu <- 0; sig <- 0.01
sim.dat.2 <- rlnorm(n, meanlog = mu, sdlog = sig)
mean.appoptA2.1 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                    para=par1,dat=sim.dat.2,control=list(maxit=500,trace=0))
# 0.9907677

mean.appoptA2.2 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                         para=par2,dat=sim.dat.2,control=list(maxit=500,trace=0))
#1.009917

median.appoptA2.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                      para=par1,dat=sim.dat.2,q=0.5,control=list(maxit=500,trace=0))

#0.9960555
median.appoptA2.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=10,
                           para=par2,dat=sim.dat.2,q=0.5,control=list(maxit=500,trace=0))
#1.004569

#large variation of logNormal distribution
mu <- 0; sig <- 3
sim.dat.3 <- rlnorm(n, meanlog = mu, sdlog = sig)
mean.appoptA2.1 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                         para=par1,dat=sim.dat.3,control=list(maxit=500,trace=0))
#0.06327413

mean.appoptA2.2 <- optim(0.1,obj.mean.unequcost,method = "Brent",lower=0,upper=100,
                         para=par2,dat=sim.dat.3,control=list(maxit=500,trace=0))
#20.84294

median.appoptA2.1 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=100,
                           para=par1,dat=sim.dat.3,q=0.5,control=list(maxit=500,trace=0))

#0.1803604
median.appoptA2.2 <- optim(0.1,obj.quantile.unequcost,method = "Brent",lower=0,upper=100,
                           para=par2,dat=sim.dat.3,q=0.5,control=list(maxit=500,trace=0))
#0.9040136




