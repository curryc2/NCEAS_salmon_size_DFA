#************************************************************************************************
#Project Name: ALASKAN SOCKEYE SIZE TRENDS - Bayesian Dynamic Factor Analysis of Size-at-Age Trends
#Creator: Dr. Curry James Cunningham, NOAA/NMFS, AFSC, ABL
#Date: 4.1.17
#
#Purpose: Conduct a DFA on the size of Sockeye salmon throughout the state using Bayesian Methods
#
#
#*************************************************************************************************
#VERSIONS:
#
#ASSUMPTIONS:
#
#
#NOTES:
#
#
#*************************************************************************************************
require(MARSS)
require(stringr)
require(xlsx)
require(reshape2)
require(ggplot2)
require(stringr)
require(RColorBrewer)
require(statss)
require(rstan)
require(loo)
require(foreach)
require(parallel)
require(snowfall)


#Source necessary scripts
source('R/add-alpha.R')

#==============================================
# CONTROL SECTION
fwa <- 1
oa <- 3

## number of processes
mm <- 2

#Years
fit.years <- 1975:2015
n.fit.years <- length(fit.years)

#Boolean for covariates
do.covars <- FALSE
#==============================================

#Read in Data
# data <- read.csv('Data/Length at Age.csv', header=FALSE)
data <- read.xlsx('Data/Length at Age.xlsx', sheetName='Reordered Length at Age.csv', startRow=1, header=FALSE, 
                  stringsAsFactors=FALSE)
n.groups <- ncol(data)-1
#Limit to only Mean Lengths
# head.titles <- as.vector(data[1,-1]) 
# head.stocks <- as.vector(data[2,-1])
# head.age <- data[3,-1]
# head.type <- data[4,-1]

head.location <- vector(length=n.groups)
head.region <- vector(length=n.groups)
head.source <- vector(length=n.groups)

head.titles <- vector(length=n.groups)
head.stocks <- vector(length=n.groups)
head.age <- vector(length=n.groups)
head.type <- vector(length=n.groups)

i <- 1
for(i in 1:n.groups) {
  head.location[i] <- data[1,i+1]
  head.region[i] <- data[2,i+1]
  head.source[i] <- data[3,i+1]
  head.titles[i] <- data[4,i+1]
  head.stocks[i] <- data[5,i+1]
  head.age[i] <- data[6,i+1]
  head.type[i] <- data[7,i+1]
}#next i

years <- as.numeric(data[-c(1:7),1])
n.years <- length(years)

#Extract values
# values <- read.xlsx('Data/Length at Age.xlsx', sheetName='Length at Age.csv', startRow=8, header=FALSE, stringsAsFactors=FALSE)
values <- as.data.frame(data[-c(1:7),-1])
values <- as.numeric(values)
dimnames(values)[[1]] <- years
names(values) <- head.titles

#Metadata
ages <- unique(head.age)
n.ages <- length(ages)

stocks <- unique(head.stocks)
n.stocks <- length(stocks)

types <- unique(head.type)
n.types <- length(types)

#Intro Plots
# #Mean Length of 1.2's
# temp.data <- as.matrix(values[,which(head.type=='mean' & head.age=='1.2')])
# # dimnames(temp.data) <- list(years,head.stocks[which(head.type=='mean' & head.age=='1.2')])
# temp.list <- melt(temp.data, na.rm=TRUE)
# names(temp.list) <- c('year','stock','value')
# temp.list$value <- as.numeric(temp.list$value)
# temp.list$year <- as.numeric(temp.list$year)


# g <- ggplot(data=temp.list, aes(x=year, y=value, colour=stock)) + 
#        geom_line()
# plot(g)


#########################################################################################
# SELECT DATA TO MODEL
temp.age <- paste(fwa, '.', oa, sep='')
temp.loc <- which(head.type=='mean' & head.age==temp.age)
temp.dat <- as.matrix(values[which(years %in% fit.years),temp.loc])
temp.dat.2 <- matrix(as.numeric(temp.dat), nrow=nrow(temp.dat), ncol=ncol(temp.dat))
ts_names <- head.stocks[temp.loc]
temp.locations <- head.location[temp.loc]
temp.sources <- head.source[temp.loc]
#########################################################################################
dat <- t(scale(temp.dat.2))

#########################################################################################
##### FIT STAN MODEL #####
fit <- fit_dfa(y=dat, num_trends=2, chains=2, iter=1e4)


names(fit)
# plot(fit)

rot <- rotate_trends(fit)

#Plot the trends
matplot(t(rot$trends_mean),type="l",lwd=2,ylab="mean trend")

#Leave-one-out Cross Validation
loo.cv <- loo(extract_log_lik(fit))
loo.cv$looic  #Statistic lower is better

#Extracet parameters
pars <- extract(fit)

#Plot Trends
trend.med <- apply(pars$x, c(2,3), median)
trend.low.95 <- apply(pars$x, c(2,3), quantile, 0.025)
trend.up.95 <- apply(pars$x, c(2,3), quantile, 0.975)
trend.low.50 <- apply(pars$x, c(2,3), quantile, 0.25)
trend.up.50 <- apply(pars$x, c(2,3), quantile, 0.75)

plot(x=NULL, y=NULL, xlim=c(min(fit.years), max(fit.years)), ylim=c(min(trend.low.95), max(trend.up.95)), xlab='Year', ylab='Trend Value')
axis(side=1, at=fit.years, col='black', labels=FALSE, tck=-0.01)

# display.brewer.all(colorblindFriendly=TRUE)
cols <- brewer.pal(n=mm, name='Dark2')[1:mm]

i <- 1
for(i in 1:mm) {
  polygon(x=c(fit.years,rev(fit.years)), y=c(trend.low.95[i,], rev(trend.up.95[i,])), col=add_alpha(col=cols[i], alpha=0.25), border=FALSE)
  polygon(x=c(fit.years,rev(fit.years)), y=c(trend.low.50[i,], rev(trend.up.50[i,])), col=add_alpha(cols[i], alpha=0.25), border=FALSE)
  lines(x=fit.years, y=trend.med[i,], col=cols[i], lwd=2)
  # points(x=fit.years, y=trend.mean[i,], pch=21, bg=cols[i])
}
#Plot 

# plot(pred_mean, type="l", lwd = 1, ylim = range(c(pred_mean, pred_lo, pred_hi)), main = "Trend")
# years
# polygon(x=c())
# 
# lines(pred_lo)
# lines(pred_hi)


#Plot Fit to data
pred.med <- apply(pars$pred, c(2,3), median)
pred.low.95 <- apply(pars$pred, c(2,3), quantile, 0.025)
pred.up.95 <- apply(pars$pred, c(2,3), quantile, 0.975)
pred.low.50 <- apply(pars$pred, c(2,3), quantile, 0.25)
pred.up.50 <- apply(pars$pred, c(2,3), quantile, 0.75)

n.per.page <- 4 #Number of plots per page
par(mfrow=c(n.per.page,3), oma=c(4,4,1,1), mar=c(0,4,0,0))

n.dat <- dim(dat)[1]

i <- 1
for(i in 1:n.dat) {
  y.lim <- c(min(pred.low.95[i,], dat[i,], na.rm=TRUE), max(pred.up.95[i,], dat[i,], na.rm=TRUE))
  plot(x=fit.years, y=dat[i,], pch=21, bg='gray', xaxt='n', ylim=y.lim, ylab=ts_names[i])
  polygon(x=c(fit.years, rev(fit.years)), y=c(pred.up.95[i,], rev(pred.low.95[i,])), col=rgb(1,0,0, alpha=0.25), border=FALSE)
  polygon(x=c(fit.years, rev(fit.years)), y=c(pred.up.50[i,], rev(pred.low.50[i,])), col=rgb(1,0,0, alpha=0.25), border=FALSE)
  lines(x=fit.years, y=pred.med[i,], col='red')
  if(i%%4==0 | i==n.dat) {
    axis(side=1, at=fit.years, labels=FALSE, col='gray')
    pty.yrs <- pretty(fit.years)
    pty.yrs <- pty.yrs[which(pty.yrs %in% fit.years)]
    axis(side=1, at=pty.yrs, labels=pty.yrs, col='black')
    
  }
}#next i


names(pars)

#Plot the loadings
rot <- rotate_trends(fit)

dim(rot$Z_rot)

z.med <- apply(rot$Z_rot, c(2,3), mean)
z.low.95 <- apply(rot$Z_rot, c(2,3), quantile, 0.025)
z.up.95 <- apply(rot$Z_rot, c(2,3), quantile, 0.975)
z.low.50 <- apply(rot$Z_rot, c(2,3), quantile, 0.25)
z.up.50 <- apply(rot$Z_rot, c(2,3), quantile, 0.75)

n.dat <- dim(dat)[1]

par(mfrow=c(mm,1))
i <- 1
for(i in 1:mm) {
  y.lim <- c(min(z.low.95), max(z.up.95))
  x.lim <- c(1,n.dat)
  plot(x=NULL, y=NULL, xlim=x.lim, ylim=y.lim)
  xs <- c(1:n.dat)
  segments(x0=xs, y0=z.low.95[,i], x1=xs, y1=z.up.95[,i], lwd=1)
  segments(x0=xs, y0=z.low.50[,i], x1=xs, y1=z.up.50[,i], lwd=3)
  
  points(x=c(1:n.dat), y=z.med[,i], pch=21, bg='gray')
  
  abline(h=0)
  
  if(i==mm) {
    axis(side=1, at=c(1:n.dat), labels=ts_names, )
  }
}#next i



#====================================================
#Calculate Trial Statistics
trial.mm <- c(1:5)
n.trial.mm <- length(trial.mm)

fits <- vector('list', length=n.trial.mm)

i <- 1
for(i in 1:n.trial.mm) {
  fits[[i]] <- fit_dfa(y=dat, num_trends=trial.mm[i])
}


#Create table of values
loo.stat <- vector(length=n.trial.mm)

i <- 1
for(i in 1:n.trial.mm) {
  loo.stat[i] <- loo(extract_log_lik(fits[[i]]))$looic
}

df.sel <- data.frame(trial.mm, loo.stat, loo.stat-min(loo.stat))
names(df.sel) <- c('Number_Trends', 'loo.stat', 'delta_loo.stat')
df.sel


# 
# wrapper_fxn <- function(i) {
#   fits <- fit_dfa(y=dat, num_trends=trial.mm[i])
#   return(fits)
# }
# 
# 
# #Run in parallel with snowfall
# sfInit(parallel=TRUE, cpus=detectCores()-1, type='SOCK')  #Detect Cores
# sfExportAll()
# sfLibrary("statss")
# output <- sfLapply(trial.mm, fun=wrapper_fxn)
# sfStop()





plot(rot$trends_mean[1,], type='l')
plot(rot$trends_mean[2,], type='l')
AIC(fit)
