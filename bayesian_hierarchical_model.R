###########################################
# Author: Bryan Whiting
# Collaborators:
# Created: Nov 4, 2014
# Last edit: May 10, 2016
# Description: Estimate golf course tee times revenue using variables of interest.
##########################################

##########################################
# Packages and directories
##########################################
setwd("------")
library(MASS) #kde2d function
library(xtable)
library(sqldf)
library(ggplot2)
library(Hmisc) #errbar function
library(corrplot) # for graphing the covariance matrix
library(coda) #mcmc diagnostics, rf, autocorr

##########################################
# Resources
##########################################
# MCMC diagnostics
system("open http://www.people.fas.harvard.edu/~plam/teaching/methods/convergence/convergence_print.pdf")
# Coda package - MCMC diagnostics, etc
system("open http://cran.r-project.org/web/packages/coda/coda.pdf")
# MCMC diagnostics interpreted:
system("open http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_introbayes_sect008.htm")


##########################################
# Read in and Manipulate data
##########################################
#fd <- read.table("final.txt")
#
setwd("-----")
fd <- read.csv("-----")
dat <- as.data.frame(fd)

setwd("------")
# Descriptions
# ttfee - average fee paid by players in round
#   rack rate = posted price
#     if nobody, it's rack rate'.
#   ttfee - average price paid by those who paid.

# Dependent Variables
# SPD
#   if empty, call it 0.
# temp
#   Max & Min of temp for that day
#   temp is hourly, min and max are daily
# pcp06
#   pcp24 is usable
# Dewp - humidity measure
#   humidity is good. If dewpoint < temp, you have fog.
# Pull in Macro

### Data Cleansing - only care about revenue
### Truncate Quantity so it maxes at 4
fd$Qtrunc <- fd$Quantity
fd$Qtrunc[which(fd$Qtrunc > 4)] <- 4

### Care only about Revenue
fd$Revenue <- with(fd,TTFee*Qtrunc)
dat <- as.data.frame(fd$Revenue)
dat$TTimes <- fd$TTimes
dat$Date <- fd$TDPosix
colnames(dat) <- c("Rev","TTime","Date")

# get n_i
dat$na <- as.numeric(!is.na(dat$Rev))
n_i <- sqldf("select sum(na) as n_i, TTime from dat group by TTime")

# Since 6.2 only appears on certain days, we'll have to add n_as so that they all add up to the total.
days.recorded <- sqldf("select TTime, count(TTime) as nidays from dat group by TTime")
# Sanity check
# x <- dat[which(dat$TTime == 6.3),] # dim(x) should equal the nidays value

# Get the total number of days on record
num.days <- length(unique(dat$Date))
num.days
uniquedays <- unique(dat$Date)
# Days per year on Record
table(substr(uniquedays,1,4))

# True n_i should be 
n_i
ni <- n_i[,1]

# Assign 1:89 for each time slot
n_i$ind <- 1:89
dat <- sqldf("select dat.*, n_i.ind 
      from dat 
      left join n_i 
      on dat.TTime=n_i.TTime")


##########################################
# Exploratory Analysis
##########################################
# MLEs for Revenue
avg <- sqldf("select avg(Rev) as avgrev, variance(Rev) as varrev, TTime from dat group by TTime")
plot(x = avg$TTime, y = avg$avgrev, 
     main = "MLEs of Revenue at Tee Time", xlab = "Hour",
     ylab = "Revenue"
     )
abline(v = avg$TTime, col = 'grey') # REMEMBER- ttimes aren't real "times"

plot(x = avg$TTime, y = avg$varrev, col = "red", main = "VARIANCE of Rev @ TTime")
abline(v = avg$TTime) # REMEMBER- ttimes aren't real "times"

# Plotting on the same doesn't help
# ggplot(avg, aes(TTime)) + 
#   geom_line(aes(y = avgrev, colour = 1)) + 
#   geom_line(aes(y = varrev, colour = 2))

qplot(x = TTime, y = avgrev,data = avg,
      main = "Average Revenue by Tee Time") 
# + geom_vline(xintercept = avg$TTime)
#ts.plot(x = avg$TTime, y = avg$avgrev)

nrow(avg) #89 tee times

#### Create a table of times
tims <- cbind(avg$TTime, avg$avgrev,n_i$n_i)
obj <- round(t(tims),2)
obj <- obj[,c(1:2,10:11,40:42,70,71,82:83)]
rownames(obj) <- c("Hour.Min","Avg.Rev","Num.Obs")
obj <- xtable(obj)
print(obj,file = "data.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, include.colnames = F,
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)




##########################################
# MCMC
##########################################
# Model:
# Y_ij ~ N(\tt_i,\sigma^2)
# \tt_i ~ N(\tt_{i-1}, v^2)
# \tau^2 ~ IG(a_{\tau},b_{\tau})
# \sigma^2 ~ IG(a_{\sigma},b_{\sigma})

# Interpretation:
# Y_ij: Revenue at tee time "i", number "j"
# \tt_i: Average revenue at tee time "i"
# \sigma^2: within tee time variance
# \tt_{i-1}: Average revenue at previous tee time
# \tau^2: variance of average in revenue "i", between-tee-time variance

### Choosing good Prior Values
png("prior-sig.png")
plot(density(1/rgamma(100000,a.s<<- 50, shape = (b.s<<-5))),main = "Prior Sig2")
dev.off()
png("prior-tau.png")
plot(density(1/rgamma(100000,a.t<<- 15, shape = (b.t<<-10))),main = "Prior Tau2")
dev.off()

### Prior predictive distribution
npred = 10000
pr.pred <- list(NA)
for(i in 1:86){
  y.pred[[i]] <- rnorm(npred,theta[,(i+1)],sqrt(theta[,89])) #i+1 avoids 0th term
  # make the min 0 (nobody's gonna lose money on a round.)
  y.pred[[i]][which((y.pred[[i]] < 0))] <- 0 
}


# Elements of the data.
n <- 86 # Only 86 ttimes have data.
k <- 86
# Set up the revenue list, remove the nas
rev <- list(NA)
for(i in 1:n) {
  r <- dat$Rev[which(dat$ind==i)]
  rev[[i]] <- r[!is.na(r)]
}

# what do the sums look like?
lapply(rev, mean)
lapply(rev, length)

# hyperpriors
m = 60
s2 = 5
revbar0 = 100
n0 = 10
a.sig <- a.s
b.sig <- b.s
a.tau <- a.t
b.tau <- b.t


golfmcmc<-function(niter=200){
  # Starting values
  theta <- matrix(60,ncol = (k-1), nrow = niter)  
  theta0 <- rep(60,niter)
  thetak <- rep(60,niter)
  tau2 <- rep(1,niter)
  sig2 <- rep(1,niter)

  for(i in 2:niter)
  {
    ### GENERATE THETAS
    # Set-up
    prev <- i-1
    nxt  <- i+1
    tau2prev <- tau2[prev]
    sig2prev <- sig2[prev]
    
    # Get theta0 value
#     temp <- 1.0/(tau2prev*s2*n0 + sig2prev*(tau2prev+s2))
#     mustar <- (tau2prev*sig2prev*revbar0*n0 + theta[prev,1]*sig2prev*s2 + m*sig2prev*tau2)* temp
#     sigstar <- (sig2prev*tau2prev*s2) * temp
#     theta0[i] <- rnorm(1,mustar,sqrt(sigstar))
    
    # Try other way
    theta0[i] <- rnorm(1,m,sqrt(s2))
    
    
    # Get theta_1 value
      mu <- theta0[i] + theta[prev,2]
      temp <- 1.0 / (ni[1] * tau2prev + 2*sig2prev)
      mustar <- (mean(rev[[1]])*ni[1] * tau2prev + mu * sig2prev) * temp
      sigstar <- tau2prev * sig2prev * temp
      theta[i,1] <- rnorm(1, mustar, sqrt(sigstar))
    
    # Get theta_2 through theta_{k-1}
    for(j in 2:(k-1))
    {
      if(j == 85){ mu <- theta[i,j-1] + thetak[i] }
        else{
          mu <- theta[i,j-1] + theta[i,j+1] 
        }
      temp <- 1.0 / (ni[j] * tau2prev + 2*sig2prev)
      mustar <- (mean(rev[[j]])*ni[j] * tau2prev + mu * sig2prev) * temp
      sigstar <- tau2prev * sig2prev * temp
      theta[i,j] <- rnorm(1, mustar, sqrt(sigstar))
    }
    
    # Get theta_k value
    mu <-  theta[i,k-1]
    temp <- 1.0 / (ni[k] * tau2prev + sig2prev)
    mustar <- (rev[[k]]*ni[k] * tau2prev + mu * sig2prev) * temp
    sigstar <- tau2prev * sig2prev * temp
    thetak[i] <- rnorm(1, mustar, sqrt(sigstar)) 
    
    ### GENERATE SIG2, TAU2
    # Calculate ssq for sigma2
    ssq <- 0
    ssq <- ssq + sum((revbar0*n0-theta0[i])^2) #theta0
    for(j in 1:(k-1)){
      ssq <- ssq + sum((rev[[j]]-theta[i,j])^2) #theta_1:theta_{k-1}
    }
    ssq <- ssq + sum((rev[[k]]-thetak[i])^2) #theta_k
    
    #generate sig2
    astar <- a.sig+sum(ni)*0.5 #Instead of dividing by 2
    bstar <- (1/b.sig+ssq/2)^(-1)
    sig2[i] <- 1/rgamma(1,astar,scale=bstar)
    
    # Calculate sstau for tau2
    sstau <- 0
    sstau <- sstau + sum((theta0[i]-theta0[i-1])^2)
    sstau <- sstau + sum((theta[i,]-theta[i-1,])^2)
    sstau <- sstau + sum((thetak[i]-theta[i,(k-1)])^2)
    
    #generate tau2
    astar <- a.tau + (k*0.5) #Instead of dividing by 2
    bstar <- (1/b.tau+.5*sstau)^(-1)
    tau2[i] <- 1/rgamma(1,astar,scale=bstar)
    cat("iter =",i,"\n")
  }
  out <- cbind(theta0,theta,thetak,tau2,sig2)
  colnames(out) <- c(paste("theta",0:k,sep = ""),"tau2","sig2")
  return(out)
}
niter <- 10000
theta.full <- golfmcmc(niter)

##########################################
# MCMC Diagnostics & Convergence:
##########################################
### Burn-in
burn <- 500
theta <- theta.full[-(1:burn),]

### Marginal Distributions ###
psig2 <- theta[,88]
ptau2 <- theta[,89]

### Trace Plots ###
plot(theta[,1],type = "l")
plot(theta[,2],type = "l")
matplot(theta[,c(2,10,50)],type = "l")
matplot(theta[,6:10],type = "l")

# a sampel of trace plots
png("trace-thetas.png")
par(mfrow = c(2,2))
plot(theta[,2],type = "l", main = "Theta 2")
plot(theta[,10],type = "l", main = "Theta 10")
plot(theta[,50],type = "l", main = "Theta 50")
matplot(theta[,c(2,10,50)],type = "l")
title("Traceplots for Theta 2, 10, 50")
dev.off()

# all trace plots
traceplots <- function(){
  for(i in 1:87){
    plot(theta[,i],type = "l", main = paste("Theta",i))
    Sys.sleep(.15)
  }
}
#saveGIF(traceplots) # need ImageMagick to work

# Trace plots of sigma2, tau2
png("trace-sig-tau.png")
par(mfrow = c(1,2))
plot(psig2,type = "l",main = "sigma2")
plot(ptau2,type = "l",main = "tau2")
dev.off()

### Auto-correlation
autocorr.plot(theta[,1],auto.layout = F)
autocorrplots <- function(){
  par(mfrow = c(1,1))
  for(i in 1:87){
    autocorr.plot(theta[,i], main = paste("Theta",i),auto.layout = F)
    Sys.sleep(.15)
  }
  dev.off()
}
autocorrplots()

png("diags-autoc-theta.png")
par(mfrow = c(2,2))
autocorr.plot(theta[,20], main = paste("Theta",20),auto.layout = F)
autocorr.plot(theta[,40], main = paste("Theta",40),auto.layout = F)
autocorr.plot(theta[,67], main = paste("Theta",67),auto.layout = F)
autocorr.plot(theta[,75], main = paste("Theta",75),auto.layout = F)
dev.off()

png("diags-autoc-vars.png")
par(mfrow = c(1,2))
autocorr.plot(theta[,88], main = paste("Tau2"),auto.layout = F)
autocorr.plot(theta[,89], main = paste("Sig2"),auto.layout = F)
dev.off()


### Raftery-Lewis
thetamcmc <- mcmc(theta)
raftery.diag(thetamcmc)
rfd <- raftery.diag(theta)
depfacts <- rfd$resmatrix[,4]
png("diags-rfdeps.png")
plot(depfacts,main = "Raftery-Lewis Dependence Factors\n DF < 5 Desired",
     ylab = "Depdendence Factor",
     pch = 19)
text(x = c(88,89),y = (depfacts[c(88,89)]-.05),labels = c("tau2","sig2"))
dev.off()

### Geweke Diagnostic
# H0: Difference of means between first 10% and last 50% = 0.
gvals <- geweke.diag(thetamcmc)
pvals <- 1-pnorm(abs(gvals$z),0,1)

# Create plot of pvalues
notsig <- sig05 <- which(pvals > 0.05)
sig05 <- which(pvals <= 0.05 & pvals > 0.01)
sig01 <- which(pvals <= 0.01)
cols <- 1:89
cols[notsig]<-1
cols[sig05]<- 2
cols[sig01] <- 4
# plot the p-values
png("diags-geweke-pvals.png")
plot(pvals,xlim = c(0,130),col = cols,pch = 19,
     main = "Geweke Diagnostic P-values: \n Ho: 1st 10% = Last 50%",
     ylab = "p-value")
abline(h = 0.05)
abline(h = 0.01)
legend("topright",col = c(1,2,4,0,0),pch = c(19,19,19,0,0),
       legend = c("p>0.05","p<=0.05","p<=0.01",paste("Num <0.05 =",length(sig05)),paste("Num <0.01 =",length(sig01))),
       bty ="n")
dev.off()

### Effective Sample Size
effectiveSize(theta)

### HPD
HPDinterval(thetamcmc)

##########################################
# Posterior Statistics Results:
##########################################
### Posterior Densities
# Sigma, tau
plot(density(psig2))
plot(density(ptau2))

### Posterior Means, Variances, & 95% CIs
newk <- k+1 # to include theta0
# Means, Quantiles (95\% PI), Variance, Stdevs
means <- colMeans(theta)
#post.thetas <- cbind(means[2:newk],avg$avgrev[1:k],1:k)
quants <- apply(theta,2,quantile,c(.025,.975))
vars <- apply(theta,2,var)
sds <- apply(theta,2,sd)
post.sum <- cbind(means,t(quants),vars,sds)
# Prettify the answers into a table. 
colnames(post.sum) <- c("$E(\\theta|Y)$",".025\\%",".975\\%","$V(\\theta|Y)$","$\\sqrt{Var(\\theta|Y)}$")
rownames(post.sum) <- c(paste("$\\theta_{",0:86,"}$",sep = ""),"$\\tau^2$","$\\sigma^2$")
obj <- xtable(post.sum,caption ="Posterior Summary Statistics")
print(obj,file = "postsum.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

### Plotting Posterior Means
qplot(y = colMeans(theta)[1:newk],x = 1:newk) 
#+ geom_point(y = avg$avgrev, x = 1:newk, col = 2)

# Plot of posterior means with error bars
png("results-postmeanstheta.png")
errbar(x = 0:86,y = means[1:87],yplus = quants[1,1:87],yminus= quants[2,1:87], 
       ylab = "Average Rev / TTime",xlab = "Theta",
       col = "blue")
title("Posterior Means of Thetas")
dev.off()
boxplot(theta[,1:87],outline = F, "Boxplots")

# Posterior distribution of tau2 and sigma2
# not a good plot:
errbar(x = c(1,2),y = means[88:89],yplus = quants[1,88:89],yminus= quants[2,88:89],
       main = "Posterior Means", 
       col = "blue")
boxplot(theta[,88],outline = F)
boxplot(theta[,89],outline = F)

### Posterior Correlation matrix
covs <- cov(theta[,1:87])
png("covplot.png")
corrplot(covs,is.corr = F,main = "Covariance Plot", tl.pos = "n"
         )
dev.off()

cors <- cor(theta[,1:87])
png("corrplot.png")
corrplot(cors,method = "color",tl.pos = "n",
         #outline = T)
         addgrid.col = "light grey")
dev.off()

### Posterior Predictive Distribution
y.pred <- list(NA)
for(i in 1:86){
  y.pred[[i]] <- rnorm(niter,theta[,(i+1)],sqrt(theta[,89])) #i+1 avoids 0th term
  # make the min 0 (nobody's gonna lose money on a round.)
  y.pred[[i]][which((y.pred[[i]] < 0))] <- 0 
}

# Combine the predictive revenue means with ttime, and compare with MLEs
y.predmn <- lapply(y.pred,mean)
y.predmn <- unlist(y.predmn)
mles <- unlist(lapply(rev, mean))
prevbytime <- data.frame(TTime = n_i$TTime[1:86])
prevbytime$y.predmn <- y.predmn
prevbytime$mles <- mles

png("postpred-means.png")
plot(x=prevbytime$TTime, y = prevbytime$y.predmn,
     xlab = "Hour of Tee Time",
     main = "Bayes v. ML Estimates on Revenue",
     ylab = "Predicted Average Revenue",
     col = "blue", ylim = c(40,160),pch = 19, type = "o",lty = 2)
lines(x=prevbytime$TTime,y = prevbytime$mles, type = "o", pch = 19,lty = 2)
legend("topright",col = c("blue",1),lty = 1, bty = "n", legend = c("Bayes","MLE"))
abline(v = prevbytime$TTime,col = "light grey",lty = 2)
dev.off()

# Other way to plot it
# xx <- as.matrix(prevbytime)
# matplot(x = xx[,1],y = xx[,2:3],type = "o",pch = 19, lty = c(1,2))

# Plot posterior distribution of highest revenue
max.ind <- which(y.predmn == max(y.predmn))
xx <- y.pred[[max.ind]]
# Get hpd
source("~/stat651-bayes/bayes-functions.R")
hpd <- get.hpd(xx,.95)

png("postpred-130.png")
plot(density(xx), main = "Posterior Predictive for 1:30pm")
abline(v = mean(xx),col = "blue")
text(y = .002,x = mean(xx+10),labels = paste("mean = $",round(mean(y.pred[[44]])),sep = ""))
abline(v = quantile(xx,c(.025,.975)), col = "blue",lty = 2)
abline(v = hpd, col = "red",lty = 2, cex =3)
legend("topright",legend = c("95% PI","95% HPD"),col = c("blue","red"),lty = 2,bty = "n")
dev.off()



##########################################
# Questions We Can Answer
##########################################
### Questions of Interest
# What hours are best? (define best)
# What hours are worst? (define worst, and find pr(rev <20)
# What is the expected revenue at each time slot each month?
# Is there an hour-to-hour effect? (t-test on different hours)
# are the times different within hours?

### What's the probability of making less than $30?
cost <- 30
y.less <- NA
  for(i in 1:length(y.pred)){
    y.less[i] <- mean(y.pred[[i]]<cost)
  }
y.less30 <- y.less
cost <- 50
y.less <- NA
for(i in 1:length(y.pred)){
  y.less[i] <- mean(y.pred[[i]]<cost)
}
y.less50 <- y.less

prob.cost <- cbind(y.less30,y.less50)
png("results-probloss.png")
matplot(prob.cost,type = "o",lty=2,pch = 19,col = c(2,1),
        ylab = "Pr(Posterior Pred. Rev < Cost)",xlab = "Tee Time",
        main = "Prob of Loss (Rev < Cost)")
legend("top",legend = c("Pr(Rev) < $50","Pr(Rev) < $30"),bty = "n",col = c(1,2),lty = 1)
dev.off()
### What



##########################################
# Model Selection
##########################################
# If I have time, I'll get to this

##########################################
# Goodness of Fit
##########################################
### Posterior Predictive Checking
# With Density plots
pval <- NA
png("quantiles.png")
for(i in 1:86){
  xx <- rnorm(niter,theta[,(1+i)],sqrt(psig2))
  q <- pnorm(xx,theta[,(1+i)],sqrt(psig2))
  if (i == 1) {
    plot(density(q,from = 0, to = 1),ylim = c(0,1.5),
    main = "Density Estimates of Quantiles for All 86 Times")
  }
  else{lines(density(q,from = 0, to = 1))}
  pval[i] <- ks.test(q,punif)$p.value
}
dev.off()
#Make it a moustach
# text(x = .3, y = 1.3, label = ".",cex = 5)
# text(x = .3, y = 1.3, label = "O",cex = 5)
# text(x = .7, y = 1.3, label = ".",cex = 5)
# text(x = .7, y = 1.3, label = "O",cex = 5)
# text(x = .5, y = .6, label = "\\___/",cex = 5)

# K-S test
# prepare the p-values
png("quantiles-pvals.png")
signif <- (as.numeric(pval<.05)+2)
num.sig <- sum(pval<0.05)
# plot the p-values
plot(pval,xlim = c(0,120),col = signif,pch = 19,
     main = "K-S Test For 86 Tee Times",
     ylab = "p-value")
abline(h = 0.05)
legend("topright",col = c(2,3,0),pch = c(19,19,0),legend = c("p<0.05","p>=0.05",paste("Num Sig =",num.sig)),bty ="n")
dev.off()





### Bayesian Chi2 test




##########################################
# Demand Curves
##########################################
## Backing Out Demand Curves given MCMC data and golf revenue structure.
price.optim <- function(tim=1,n=1,P=floor(seq(10,370, length = 40))){
  # n is the number of total days over all time. probably not the right amplifier. n should be the number of days in a month.
  REV <- y.pred[[tim]]
  #plot(density(REV))
  PQmat <- matrix(NA,ncol = 5, nrow = length(P))
  rownames(PQmat) <- as.character(P)
  colnames(PQmat) <- as.character(c(0,1,2,3,4))
  
  for(i in 1:length(P)){
    Q = floor(REV/P[i])
    Q[which(Q>=5)] <-4
    #which(Q==5)
    q0 = sum(Q==0)
    q1 = sum(Q==1)
    q2 = sum(Q==2)
    q3 = sum(Q==3)
    q4 = sum(Q==4)
    tab <- cbind(q0,q1,q2,q3,q4)
    PQmat[i,] <- prop.table(tab)
  }
  PQmat
  Weighted.PQmat <- sweep(PQmat,2,c(0,1,2,3,4),"*")
  # Expected revenue for 1 day:
  ExpRev <- sweep(Weighted.PQmat,1,P,'*')[,2:5]
  ExpTotRev <- rowSums(ExpRev)
  
  Demand.mat <- ceiling(PQmat*n)
  
  # Probability matrix
  #matplot(PQmat, type = c("o"), main = "Probability")
  # Demand Curves
  #matplot(cumDem[,2:5],type = "o", main = paste("Demand",tim))
  # Revenue Curves for Each Q #QQQ Why does this exist?
  maxrev <- round(max(ExpTotRev),2)
  pmax.ind <- max(ExpTotRev)==ExpTotRev
  pmax <- P[which(pmax.ind==1)]
  pmax <- round(pmax,2)
  
  plot(x = P, y =ExpTotRev,type = "o", main = paste("Daily Expected Revenue for theta",tim,sep = ""))
  legend("topright",legend = c(paste("Max Rev = $",maxrev,sep = ""),paste("@ Price = $",pmax,sep = "")))
  list(rev= maxrev,price = pmax)
}
png("results-optimrev.png")
price.optim(40,n=1,P = seq(10,350,length = 1000))
dev.off()

prices1 = revs1<-NA
for(i in 1:86){
  obj <- price.optim(i,n=1,P = seq(10,350,length = 200))
  revs1[i] <- obj$rev
  prices1[i] <- obj$price
}

### Simplified Pricing function, focuses just on optimizing daily revenue.
price.to.rev <- function(P,tim){
  REV <- y.pred[[tim]]
  #plot(density(REV))
  Q = floor(REV/P)
  Q[which(Q>=5)] <-4
  q0 = sum(Q==0)
  q1 = sum(Q==1)
  q2 = sum(Q==2)
  q3 = sum(Q==3)
  q4 = sum(Q==4)
  Qprobs <- prop.table(cbind(q0,q1,q2,q3,q4))
  Weighted.Qprobs <- c(0,1,2,3,4)*Qprobs
  Exp.Rev <- sum(P*Weighted.Qprobs)
  -Exp.Rev #to get max value
}
price.to.rev(50.16,tim = 40)

# Return a list of the profit-maximizing prices for each tee-time
price = max.rev = NA
for(i in 1:86){
  # Get best
  best.p <- function(P){price.to.rev(P,tim = i)}
  # Optimize the Function
  obj <- optim(par = 100,best.p,method = "Brent", lower = 0, upper = 400)
  max.rev[i]<- -obj$value
  price[i] <- obj$par
}
revbyprice <- data.frame(max.rev)
revbyprice$price <- price
revbyprice$time <- n_i$TTime[1:86]

# Compare this method with the above, "by hand" methods
cbind(revs1,prices1,revbyprice) #seems to check out!

# Output maximum revenues in a table
obj <- revbyprice
colnames(obj) <- c("$E(Rev|P)$","Price","Time")
rownames(obj) <- c(paste("$\\theta_{",1:86,"}$",sep = ""))
obj <- xtable(obj,caption ="Revenue-Maximizing Prices at Each Tee Time")
print(obj,file = "revbyprice.txt",caption.placement = "top",include.rownames = T,backslash = T, tabular.environment='longtable', floating = F,
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

##########################################
# Results Tables
##########################################
mega <- cbind(post.sum,rbind(".",round(revbyprice,2),".","."))
colnames(mega) <- c("$E(\\theta|Y)$",".025\\%",".975\\%","$V(\\theta|Y)$","$\\sqrt{Var(\\theta|Y)}$","$E(Rev|P)$","Price","Time")
rownames(mega) <- c(paste("$\\theta_{",0:86,"}$",sep = ""),"$\\tau^2$","$\\sigma^2$")
obj <- xtable(mega,caption ="Posterior Summary Statistics and Revenue-Maximizing Prices at Each Tee Time")
print(obj,file = "mega.txt",caption.placement = "top",
      include.rownames = T,backslash = T, 
      tabular.environment='longtable', floating = F,
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)




##########################################
# To-Do:
##########################################
# Figure out why theta0 blows up.
# Get Posterior Means
# Get Posterior Variances
# Get Other important findings of relevance. What is the problem we're trying to solve?
# Go though class notes and see what he wants us to do.
# Posterior predictive





